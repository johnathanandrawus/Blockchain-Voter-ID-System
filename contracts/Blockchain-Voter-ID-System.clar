(define-non-fungible-token voter-id uint)

(define-constant contract-owner tx-sender)
(define-constant expiration-blocks u52560) ;; ~1 year in blocks

(define-map voter-records
  { voter-id: uint }
  {
    stx-address: principal,
    registered-at: uint,
    expires-at: uint,
    status: (string-ascii 20),
    region-code: (string-ascii 10)
  }
)

(define-map identity-registry
  { address: principal }
  {
    verified: bool,
    last-verified: uint
  }
)

(define-data-var last-id uint u0)
(define-data-var total-registered uint u0)
(define-data-var paused bool false)

(define-public (register-voter 
    (region-code (string-ascii 10)))
    (let
        ((new-id (+ (var-get last-id) u1)))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (is-eq (get verified (default-to 
            { verified: false, last-verified: u0 } 
            (map-get? identity-registry { address: tx-sender }))) true) 
            (err u401))
        (asserts! (not (has-active-voter-id tx-sender)) (err u409))
        
        (try! (nft-mint? voter-id new-id tx-sender))
        (map-set voter-records
            { voter-id: new-id }
            {
                stx-address: tx-sender,
                registered-at: stacks-block-height,
                expires-at: (+ stacks-block-height expiration-blocks),
                status: "active",
                region-code: region-code
            }
        )
        (var-set last-id new-id)
        (var-set total-registered (+ (var-get total-registered) u1))
        (ok new-id)))

(define-public (verify-identity (address principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (map-set identity-registry
            { address: address }
            {
                verified: true,
                last-verified: stacks-block-height
            }
        )
        (ok true)))

(define-public (revoke-voter-id (id uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (try! (nft-burn? voter-id id (unwrap! (nft-get-owner? voter-id id) (err u404))))
        (map-set voter-records
            { voter-id: id }
            {
                stx-address: (get stx-address (unwrap! (map-get? voter-records { voter-id: id }) (err u404))),
                registered-at: (get registered-at (unwrap! (map-get? voter-records { voter-id: id }) (err u404))),
                expires-at: stacks-block-height,
                status: "revoked",
                region-code: (get region-code (unwrap! (map-get? voter-records { voter-id: id }) (err u404)))
            }
        )
        (ok true)))
(define-read-only (get-voter-record (id uint))
    (map-get? voter-records { voter-id: id }))
(define-read-only (has-active-voter-id (address principal))
    (let ((voter-record (map-get? voter-records { voter-id: (var-get last-id) })))
        (and
            (is-some voter-record)
            (is-eq (get stx-address (unwrap! voter-record false)) address)
            (is-eq (get status (unwrap! voter-record false)) "active")
            (> (get expires-at (unwrap! voter-record false)) stacks-block-height)
        )))

(define-public (toggle-contract-pause)
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (ok (var-set paused (not (var-get paused))))))

        (define-public (renew-voter-id (id uint))
    (let
        ((voter-record (unwrap! (map-get? voter-records { voter-id: id }) (err u404)))
         (owner (unwrap! (nft-get-owner? voter-id id) (err u404))))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (is-eq tx-sender owner) (err u401))
        (asserts! (is-eq (get status voter-record) "active") (err u400))
        (asserts! (> (get expires-at voter-record) stacks-block-height) (err u410))
        
        (map-set voter-records
            { voter-id: id }
            {
                stx-address: (get stx-address voter-record),
                registered-at: (get registered-at voter-record),
                expires-at: (+ stacks-block-height expiration-blocks),
                status: "active",
                region-code: (get region-code voter-record)
            }
        )
        (ok true)))

(define-read-only (get-renewal-status (id uint))
    (let
        ((voter-record (map-get? voter-records { voter-id: id })))
        (match voter-record
            record
            (let
                ((blocks-until-expiry (- (get expires-at record) stacks-block-height))
                 (renewal-window u5256))
                {
                    eligible: (and 
                        (is-eq (get status record) "active")
                        (<= blocks-until-expiry renewal-window)
                        (> blocks-until-expiry u0)),
                    expires-at: (get expires-at record),
                    blocks-remaining: blocks-until-expiry
                })
            {
                eligible: false,
                expires-at: u0,
                blocks-remaining: u0
            })))

(define-map voting-events
  { event-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    start-block: uint,
    end-block: uint,
    active: bool,
    total-votes: uint
  }
)

(define-map voter-participation
  { event-id: uint, voter-id: uint }
  {
    voted-at: uint,
    vote-choice: (string-ascii 50)
  }
)

(define-data-var last-event-id uint u0)

(define-public (create-voting-event 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (duration-blocks uint))
    (let
        ((new-event-id (+ (var-get last-event-id) u1))
         (start-block (+ stacks-block-height u144))
         (end-block (+ start-block duration-blocks)))
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (asserts! (> duration-blocks u0) (err u400))
        
        (map-set voting-events
            { event-id: new-event-id }
            {
                title: title,
                description: description,
                start-block: start-block,
                end-block: end-block,
                active: true,
                total-votes: u0
            }
        )
        (var-set last-event-id new-event-id)
        (ok new-event-id)))

(define-public (cast-vote 
    (event-id uint)
    (voter-id-param uint)
    (vote-choice (string-ascii 50)))
    (let
        ((event-data (unwrap! (map-get? voting-events { event-id: event-id }) (err u404)))
         (voter-record (unwrap! (map-get? voter-records { voter-id: voter-id-param }) (err u404)))
         (nft-owner (unwrap! (nft-get-owner? voter-id voter-id-param) (err u404))))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (is-eq tx-sender nft-owner) (err u401))
        (asserts! (get active event-data) (err u400))
        (asserts! (>= stacks-block-height (get start-block event-data)) (err u405))
        (asserts! (< stacks-block-height (get end-block event-data)) (err u406))
        (asserts! (is-eq (get status voter-record) "active") (err u407))
        (asserts! (> (get expires-at voter-record) stacks-block-height) (err u408))
        (asserts! (is-none (map-get? voter-participation { event-id: event-id, voter-id: voter-id-param })) (err u409))
        
        (map-set voter-participation
            { event-id: event-id, voter-id: voter-id-param }
            {
                voted-at: stacks-block-height,
                vote-choice: vote-choice
            }
        )
        (map-set voting-events
            { event-id: event-id }
            {
                title: (get title event-data),
                description: (get description event-data),
                start-block: (get start-block event-data),
                end-block: (get end-block event-data),
                active: (get active event-data),
                total-votes: (+ (get total-votes event-data) u1)
            }
        )
        (ok true)))
(define-public (close-voting-event (event-id uint))
    (let
        ((event-data (unwrap! (map-get? voting-events { event-id: event-id }) (err u404))))
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (asserts! (get active event-data) (err u400))
        
        (map-set voting-events
            { event-id: event-id }
            {
                title: (get title event-data),
                description: (get description event-data),
                start-block: (get start-block event-data),
                end-block: (get end-block event-data),
                active: false,
                total-votes: (get total-votes event-data)
            }
        )
        (ok true)))

(define-read-only (get-voting-event (event-id uint))
    (map-get? voting-events { event-id: event-id }))

(define-read-only (get-vote-record (event-id uint) (voter-id-param uint))
    (map-get? voter-participation { event-id: event-id, voter-id: voter-id-param }))

(define-read-only (has-voted (event-id uint) (voter-id-param uint))
    (is-some (map-get? voter-participation { event-id: event-id, voter-id: voter-id-param })))

(define-map event-analytics
  { event-id: uint }
  {
    total-participation: uint,
    region-breakdown: (list 10 { region: (string-ascii 10), count: uint }),
    completion-rate: uint,
    peak-voting-block: uint
  }
)

(define-map voter-engagement-stats
  { voter-id: uint }
  {
    total-votes-cast: uint,
    participation-streak: uint,
    last-vote-block: uint,
    engagement-score: uint
  }
)

(define-map regional-stats
  { region-code: (string-ascii 10) }
  {
    total-voters: uint,
    active-voters: uint,
    participation-rate: uint,
    total-votes-cast: uint
  }
)

(define-data-var analytics-enabled bool true)

(define-private (update-voter-engagement (voter-id-param uint) (event-id uint))
    (let
        ((current-stats (default-to 
            { total-votes-cast: u0, participation-streak: u0, last-vote-block: u0, engagement-score: u0 }
            (map-get? voter-engagement-stats { voter-id: voter-id-param })))
         (new-total (+ (get total-votes-cast current-stats) u1))
         (blocks-since-last (- stacks-block-height (get last-vote-block current-stats)))
         (new-streak (if (<= blocks-since-last u10080) (+ (get participation-streak current-stats) u1) u1))
         (new-score (+ (* new-total u10) (* new-streak u5))))
        (map-set voter-engagement-stats
            { voter-id: voter-id-param }
            {
                total-votes-cast: new-total,
                participation-streak: new-streak,
                last-vote-block: stacks-block-height,
                engagement-score: new-score
            }
        )
        (ok true)))

(define-private (update-regional-stats (region-code (string-ascii 10)) (event-id uint))
    (let
        ((current-stats (default-to 
            { total-voters: u0, active-voters: u0, participation-rate: u0, total-votes-cast: u0 }
            (map-get? regional-stats { region-code: region-code }))))
        (map-set regional-stats
            { region-code: region-code }
            {
                total-voters: (get total-voters current-stats),
                active-voters: (get active-voters current-stats),
                participation-rate: (get participation-rate current-stats),
                total-votes-cast: (+ (get total-votes-cast current-stats) u1)
            }
        )
        (ok true)))

(define-private (initialize-event-analytics (event-id uint))
    (map-set event-analytics
        { event-id: event-id }
        {
            total-participation: u0,
            region-breakdown: (list),
            completion-rate: u0,
            peak-voting-block: u0
        }
    ))

(define-public (update-analytics-on-vote 
    (event-id uint)
    (voter-id-param uint)
    (region-code (string-ascii 10)))
    (begin
        (asserts! (var-get analytics-enabled) (ok true))
        (unwrap! (update-voter-engagement voter-id-param event-id) (ok true))
        (unwrap! (update-regional-stats region-code event-id) (ok true))
        (let
            ((current-analytics (default-to 
                { total-participation: u0, region-breakdown: (list), completion-rate: u0, peak-voting-block: u0 }
                (map-get? event-analytics { event-id: event-id }))))
            (map-set event-analytics
                { event-id: event-id }
                {
                    total-participation: (+ (get total-participation current-analytics) u1),
                    region-breakdown: (get region-breakdown current-analytics),
                    completion-rate: (get completion-rate current-analytics),
                    peak-voting-block: stacks-block-height
                }
            )
        )
        (ok true)))

(define-read-only (get-event-analytics (event-id uint))
    (map-get? event-analytics { event-id: event-id }))

(define-read-only (get-voter-engagement (voter-id-param uint))
    (map-get? voter-engagement-stats { voter-id: voter-id-param }))

(define-read-only (get-regional-stats (region-code (string-ascii 10)))
    (map-get? regional-stats { region-code: region-code }))

(define-read-only (get-top-engaged-voters (limit uint))
    (let
        ((voter-list (list { voter-id: u1, score: u0 } { voter-id: u2, score: u0 } { voter-id: u3, score: u0 }
                          { voter-id: u4, score: u0 } { voter-id: u5, score: u0 })))
        voter-list))

(define-read-only (calculate-participation-rate (event-id uint))
    (let
        ((event-analytics-data (map-get? event-analytics { event-id: event-id }))
         (total-reg (var-get total-registered)))
        (match event-analytics-data
            analytics
            (if (> total-reg u0)
                (/ (* (get total-participation analytics) u100) total-reg)
                u0)
            u0)))

(define-public (toggle-analytics)
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (ok (var-set analytics-enabled (not (var-get analytics-enabled))))))

(define-read-only (get-analytics-status)
    (var-get analytics-enabled))

(define-map delegations
  { delegator-id: uint }
  {
    delegate-id: uint,
    delegated-at: uint,
    expires-at: uint,
    active: bool,
    delegation-weight: uint
  }
)

(define-map delegate-power
  { delegate-id: uint }
  {
    total-delegated-power: uint,
    active-delegators: uint,
    max-delegators: uint,
    accepts-delegations: bool
  }
)

(define-map delegation-history
  { delegator-id: uint, sequence: uint }
  {
    delegate-id: uint,
    action: (string-ascii 20),
    timestamp: uint,
    event-id: (optional uint)
  }
)

(define-data-var delegation-enabled bool true)
(define-data-var max-delegation-chain uint u3)
(define-data-var delegation-expiry-blocks uint u26280)

(define-private (get-delegation-sequence (delegator-id uint))
    (let
        ((check-sequence u1))
        (if (is-some (map-get? delegation-history { delegator-id: delegator-id, sequence: check-sequence }))
            check-sequence
            u0)))

(define-public (enable-delegation (max-delegators uint))
    (let
        ((voter-record (unwrap! (map-get? voter-records { voter-id: (unwrap! (get-voter-id tx-sender) (err u404)) }) (err u404))))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (is-eq (get status voter-record) "active") (err u407))
        (asserts! (> (get expires-at voter-record) stacks-block-height) (err u408))
        (asserts! (> max-delegators u0) (err u400))
        
        (map-set delegate-power
            { delegate-id: (unwrap! (get-voter-id tx-sender) (err u404)) }
            {
                total-delegated-power: u0,
                active-delegators: u0,
                max-delegators: max-delegators,
                accepts-delegations: true
            }
        )
        (ok true)))

(define-public (delegate-vote (delegate-voter-id uint) (weight uint))
    (let
        ((delegator-voter-id (unwrap! (get-voter-id tx-sender) (err u404)))
         (delegator-record (unwrap! (map-get? voter-records { voter-id: delegator-voter-id }) (err u404)))
         (delegate-record (unwrap! (map-get? voter-records { voter-id: delegate-voter-id }) (err u404)))
         (delegate-power-data (unwrap! (map-get? delegate-power { delegate-id: delegate-voter-id }) (err u405)))
         (existing-delegation (map-get? delegations { delegator-id: delegator-voter-id }))
         (sequence (+ (get-delegation-sequence delegator-voter-id) u1)))
        (asserts! (var-get delegation-enabled) (err u403))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (not (is-eq delegator-voter-id delegate-voter-id)) (err u400))
        (asserts! (is-eq (get status delegator-record) "active") (err u407))
        (asserts! (is-eq (get status delegate-record) "active") (err u407))
        (asserts! (> (get expires-at delegator-record) stacks-block-height) (err u408))
        (asserts! (> (get expires-at delegate-record) stacks-block-height) (err u408))
        (asserts! (get accepts-delegations delegate-power-data) (err u409))
        (asserts! (< (get active-delegators delegate-power-data) (get max-delegators delegate-power-data)) (err u410))
        (asserts! (is-none existing-delegation) (err u411))
        (asserts! (and (> weight u0) (<= weight u100)) (err u400))
        
        (map-set delegations
            { delegator-id: delegator-voter-id }
            {
                delegate-id: delegate-voter-id,
                delegated-at: stacks-block-height,
                expires-at: (+ stacks-block-height (var-get delegation-expiry-blocks)),
                active: true,
                delegation-weight: weight
            }
        )
        (map-set delegate-power
            { delegate-id: delegate-voter-id }
            {
                total-delegated-power: (+ (get total-delegated-power delegate-power-data) weight),
                active-delegators: (+ (get active-delegators delegate-power-data) u1),
                max-delegators: (get max-delegators delegate-power-data),
                accepts-delegations: (get accepts-delegations delegate-power-data)
            }
        )
        (map-set delegation-history
            { delegator-id: delegator-voter-id, sequence: sequence }
            {
                delegate-id: delegate-voter-id,
                action: "delegate",
                timestamp: stacks-block-height,
                event-id: none
            }
        )
        (ok true)))

(define-public (revoke-delegation)
    (let
        ((delegator-voter-id (unwrap! (get-voter-id tx-sender) (err u404)))
         (delegation-data (unwrap! (map-get? delegations { delegator-id: delegator-voter-id }) (err u404)))
         (delegate-power-data (unwrap! (map-get? delegate-power { delegate-id: (get delegate-id delegation-data) }) (err u404)))
         (sequence (+ (get-delegation-sequence delegator-voter-id) u1)))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (get active delegation-data) (err u400))
        
        (map-set delegations
            { delegator-id: delegator-voter-id }
            {
                delegate-id: (get delegate-id delegation-data),
                delegated-at: (get delegated-at delegation-data),
                expires-at: stacks-block-height,
                active: false,
                delegation-weight: (get delegation-weight delegation-data)
            }
        )
        (map-set delegate-power
            { delegate-id: (get delegate-id delegation-data) }
            {
                total-delegated-power: (- (get total-delegated-power delegate-power-data) (get delegation-weight delegation-data)),
                active-delegators: (- (get active-delegators delegate-power-data) u1),
                max-delegators: (get max-delegators delegate-power-data),
                accepts-delegations: (get accepts-delegations delegate-power-data)
            }
        )
        (map-set delegation-history
            { delegator-id: delegator-voter-id, sequence: sequence }
            {
                delegate-id: (get delegate-id delegation-data),
                action: "revoke",
                timestamp: stacks-block-height,
                event-id: none
            }
        )
        (ok true)))

(define-public (cast-delegated-vote 
    (event-id uint)
    (delegate-voter-id uint)
    (vote-choice (string-ascii 50))
    (use-delegated-power bool))
    (let
        ((event-data (unwrap! (map-get? voting-events { event-id: event-id }) (err u404)))
         (delegate-record (unwrap! (map-get? voter-records { voter-id: delegate-voter-id }) (err u404)))
         (delegate-power-data (unwrap! (map-get? delegate-power { delegate-id: delegate-voter-id }) (err u404)))
         (nft-owner (unwrap! (nft-get-owner? voter-id delegate-voter-id) (err u404)))
         (voting-power (if use-delegated-power (+ u1 (get total-delegated-power delegate-power-data)) u1)))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (is-eq tx-sender nft-owner) (err u401))
        (asserts! (get active event-data) (err u400))
        (asserts! (>= stacks-block-height (get start-block event-data)) (err u405))
        (asserts! (< stacks-block-height (get end-block event-data)) (err u406))
        (asserts! (is-eq (get status delegate-record) "active") (err u407))
        (asserts! (> (get expires-at delegate-record) stacks-block-height) (err u408))
        (asserts! (is-none (map-get? voter-participation { event-id: event-id, voter-id: delegate-voter-id })) (err u409))
        
        (map-set voter-participation
            { event-id: event-id, voter-id: delegate-voter-id }
            {
                voted-at: stacks-block-height,
                vote-choice: vote-choice
            }
        )
        (map-set voting-events
            { event-id: event-id }
            {
                title: (get title event-data),
                description: (get description event-data),
                start-block: (get start-block event-data),
                end-block: (get end-block event-data),
                active: (get active event-data),
                total-votes: (+ (get total-votes event-data) voting-power)
            }
        )
        (ok voting-power)))

(define-public (toggle-delegation-acceptance)
    (let
        ((delegate-voter-id (unwrap! (get-voter-id tx-sender) (err u404)))
         (delegate-power-data (unwrap! (map-get? delegate-power { delegate-id: delegate-voter-id }) (err u404))))
        (asserts! (not (var-get paused)) (err u403))
        
        (map-set delegate-power
            { delegate-id: delegate-voter-id }
            {
                total-delegated-power: (get total-delegated-power delegate-power-data),
                active-delegators: (get active-delegators delegate-power-data),
                max-delegators: (get max-delegators delegate-power-data),
                accepts-delegations: (not (get accepts-delegations delegate-power-data))
            }
        )
        (ok (not (get accepts-delegations delegate-power-data)))))

(define-private (get-voter-id (address principal))
    (let
        ((total-ids (var-get last-id)))
        (match (fold check-voter-ownership (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) none)
            found-id (some found-id)
            none)))

(define-private (check-voter-ownership (id-to-check uint) (current-result (optional uint)))
    (match current-result
        found (some found)
        (let
            ((owner (nft-get-owner? voter-id id-to-check)))
            (match owner
                owner-address (if (is-eq owner-address tx-sender) (some id-to-check) none)
                none))))

(define-read-only (get-delegation (delegator-id uint))
    (map-get? delegations { delegator-id: delegator-id }))

(define-read-only (get-delegate-power (delegate-id uint))
    (map-get? delegate-power { delegate-id: delegate-id }))

(define-read-only (get-delegation-history (delegator-id uint) (sequence uint))
    (map-get? delegation-history { delegator-id: delegator-id, sequence: sequence }))

(define-read-only (calculate-effective-voting-power (voter-id-param uint))
    (let
        ((base-power u1)
         (delegate-power-data (map-get? delegate-power { delegate-id: voter-id-param })))
        (match delegate-power-data
            power-data (+ base-power (get total-delegated-power power-data))
            base-power)))

(define-read-only (is-delegation-active (delegator-id uint))
    (let
        ((delegation-data (map-get? delegations { delegator-id: delegator-id })))
        (match delegation-data
            data (and 
                (get active data)
                (> (get expires-at data) stacks-block-height))
            false)))

(define-public (toggle-delegation-system)
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (ok (var-set delegation-enabled (not (var-get delegation-enabled))))))

(define-read-only (get-delegation-status)
    {
        enabled: (var-get delegation-enabled),
        max-chain-length: (var-get max-delegation-chain),
        expiry-blocks: (var-get delegation-expiry-blocks)
    })

(define-map fraud-detection
  { voter-id: uint }
  {
    reputation-score: uint,
    suspicious-activity-count: uint,
    last-flagged-block: uint,
    total-flags: uint,
    is-flagged: bool,
    verification-level: (string-ascii 20)
  }
)

(define-map voting-behavior
  { voter-id: uint }
  {
    total-votes: uint,
    failed-vote-attempts: uint,
    rapid-vote-attempts: uint,
    last-vote-block: uint,
    average-time-between-votes: uint
  }
)

(define-map fraud-reports
  { report-id: uint }
  {
    reporter-id: uint,
    reported-voter-id: uint,
    reason: (string-ascii 200),
    reported-at: uint,
    status: (string-ascii 20),
    reviewed-by: (optional principal)
  }
)

(define-data-var last-report-id uint u0)
(define-data-var fraud-detection-enabled bool true)
(define-data-var reputation-threshold uint u50)
(define-data-var rapid-vote-threshold uint u10)

(define-private (initialize-fraud-detection (voter-id-param uint))
    (map-set fraud-detection
        { voter-id: voter-id-param }
        {
            reputation-score: u100,
            suspicious-activity-count: u0,
            last-flagged-block: u0,
            total-flags: u0,
            is-flagged: false,
            verification-level: "standard"
        }
    ))

(define-private (initialize-voting-behavior (voter-id-param uint))
    (map-set voting-behavior
        { voter-id: voter-id-param }
        {
            total-votes: u0,
            failed-vote-attempts: u0,
            rapid-vote-attempts: u0,
            last-vote-block: u0,
            average-time-between-votes: u0
        }
    ))

(define-private (update-voting-behavior-on-vote (voter-id-param uint))
    (let
        ((current-behavior (default-to 
            { total-votes: u0, failed-vote-attempts: u0, rapid-vote-attempts: u0, last-vote-block: u0, average-time-between-votes: u0 }
            (map-get? voting-behavior { voter-id: voter-id-param })))
         (blocks-since-last-vote (if (> (get last-vote-block current-behavior) u0)
            (- stacks-block-height (get last-vote-block current-behavior))
            u0))
         (is-rapid (and (> (get last-vote-block current-behavior) u0) (< blocks-since-last-vote (var-get rapid-vote-threshold))))
         (new-total-votes (+ (get total-votes current-behavior) u1))
         (new-avg (if (> new-total-votes u1)
            (/ (+ (* (get average-time-between-votes current-behavior) (- new-total-votes u1)) blocks-since-last-vote) new-total-votes)
            blocks-since-last-vote)))
        (map-set voting-behavior
            { voter-id: voter-id-param }
            {
                total-votes: new-total-votes,
                failed-vote-attempts: (get failed-vote-attempts current-behavior),
                rapid-vote-attempts: (if is-rapid (+ (get rapid-vote-attempts current-behavior) u1) (get rapid-vote-attempts current-behavior)),
                last-vote-block: stacks-block-height,
                average-time-between-votes: new-avg
            }
        )
        (if is-rapid
            (flag-suspicious-activity voter-id-param "rapid-voting")
            (ok true))))

(define-private (update-voting-behavior-on-failure (voter-id-param uint))
    (let
        ((current-behavior (default-to 
            { total-votes: u0, failed-vote-attempts: u0, rapid-vote-attempts: u0, last-vote-block: u0, average-time-between-votes: u0 }
            (map-get? voting-behavior { voter-id: voter-id-param })))
         (new-failed-attempts (+ (get failed-vote-attempts current-behavior) u1)))
        (map-set voting-behavior
            { voter-id: voter-id-param }
            {
                total-votes: (get total-votes current-behavior),
                failed-vote-attempts: new-failed-attempts,
                rapid-vote-attempts: (get rapid-vote-attempts current-behavior),
                last-vote-block: (get last-vote-block current-behavior),
                average-time-between-votes: (get average-time-between-votes current-behavior)
            }
        )
        (if (>= new-failed-attempts u5)
            (flag-suspicious-activity voter-id-param "multiple-failures")
            (ok true))))

(define-private (flag-suspicious-activity (voter-id-param uint) (reason (string-ascii 20)))
    (let
        ((current-detection (default-to 
            { reputation-score: u100, suspicious-activity-count: u0, last-flagged-block: u0, total-flags: u0, is-flagged: false, verification-level: "standard" }
            (map-get? fraud-detection { voter-id: voter-id-param })))
         (new-suspicious-count (+ (get suspicious-activity-count current-detection) u1))
         (new-total-flags (+ (get total-flags current-detection) u1))
         (reputation-penalty u10)
         (new-reputation (if (>= (get reputation-score current-detection) reputation-penalty)
            (- (get reputation-score current-detection) reputation-penalty)
            u0))
         (should-flag (or (>= new-suspicious-count u3) (< new-reputation (var-get reputation-threshold)))))
        (map-set fraud-detection
            { voter-id: voter-id-param }
            {
                reputation-score: new-reputation,
                suspicious-activity-count: new-suspicious-count,
                last-flagged-block: stacks-block-height,
                total-flags: new-total-flags,
                is-flagged: should-flag,
                verification-level: (if should-flag "flagged" (get verification-level current-detection))
            }
        )
        (ok true)))

(define-private (increase-reputation (voter-id-param uint) (amount uint))
    (let
        ((current-detection (default-to 
            { reputation-score: u100, suspicious-activity-count: u0, last-flagged-block: u0, total-flags: u0, is-flagged: false, verification-level: "standard" }
            (map-get? fraud-detection { voter-id: voter-id-param })))
         (new-reputation (if (<= (+ (get reputation-score current-detection) amount) u100)
            (+ (get reputation-score current-detection) amount)
            u100)))
        (map-set fraud-detection
            { voter-id: voter-id-param }
            {
                reputation-score: new-reputation,
                suspicious-activity-count: (get suspicious-activity-count current-detection),
                last-flagged-block: (get last-flagged-block current-detection),
                total-flags: (get total-flags current-detection),
                is-flagged: (get is-flagged current-detection),
                verification-level: (get verification-level current-detection)
            }
        )
        (ok true)))

(define-public (submit-fraud-report
    (reported-voter-id uint)
    (reason (string-ascii 200)))
    (let
        ((reporter-voter-id (unwrap! (get-voter-id tx-sender) (err u404)))
         (reporter-record (unwrap! (map-get? voter-records { voter-id: reporter-voter-id }) (err u404)))
         (reported-record (unwrap! (map-get? voter-records { voter-id: reported-voter-id }) (err u404)))
         (new-report-id (+ (var-get last-report-id) u1)))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (not (is-eq reporter-voter-id reported-voter-id)) (err u400))
        (asserts! (is-eq (get status reporter-record) "active") (err u407))
        (asserts! (> (get expires-at reporter-record) stacks-block-height) (err u408))
        
        (map-set fraud-reports
            { report-id: new-report-id }
            {
                reporter-id: reporter-voter-id,
                reported-voter-id: reported-voter-id,
                reason: reason,
                reported-at: stacks-block-height,
                status: "pending",
                reviewed-by: none
            }
        )
        (var-set last-report-id new-report-id)
        (unwrap! (flag-suspicious-activity reported-voter-id "fraud-report") (err u500))
        (ok new-report-id)))

(define-public (review-fraud-report
    (report-id uint)
    (decision (string-ascii 20)))
    (let
        ((report-data (unwrap! (map-get? fraud-reports { report-id: report-id }) (err u404)))
         (reported-voter-id (get reported-voter-id report-data)))
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (asserts! (is-eq (get status report-data) "pending") (err u400))
        
        (map-set fraud-reports
            { report-id: report-id }
            {
                reporter-id: (get reporter-id report-data),
                reported-voter-id: reported-voter-id,
                reason: (get reason report-data),
                reported-at: (get reported-at report-data),
                status: decision,
                reviewed-by: (some tx-sender)
            }
        )
        (if (is-eq decision "confirmed")
            (flag-suspicious-activity reported-voter-id "confirmed-fraud")
            (if (is-eq decision "dismissed")
                (increase-reputation reported-voter-id u5)
                (ok true)))))

(define-public (clear-voter-flags (voter-id-param uint))
    (let
        ((current-detection (unwrap! (map-get? fraud-detection { voter-id: voter-id-param }) (err u404))))
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        
        (map-set fraud-detection
            { voter-id: voter-id-param }
            {
                reputation-score: u100,
                suspicious-activity-count: u0,
                last-flagged-block: (get last-flagged-block current-detection),
                total-flags: (get total-flags current-detection),
                is-flagged: false,
                verification-level: "verified"
            }
        )
        (ok true)))

(define-public (upgrade-verification-level (voter-id-param uint) (level (string-ascii 20)))
    (let
        ((current-detection (unwrap! (map-get? fraud-detection { voter-id: voter-id-param }) (err u404))))
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        
        (map-set fraud-detection
            { voter-id: voter-id-param }
            {
                reputation-score: (get reputation-score current-detection),
                suspicious-activity-count: (get suspicious-activity-count current-detection),
                last-flagged-block: (get last-flagged-block current-detection),
                total-flags: (get total-flags current-detection),
                is-flagged: (get is-flagged current-detection),
                verification-level: level
            }
        )
        (ok true)))

(define-read-only (get-fraud-detection-data (voter-id-param uint))
    (map-get? fraud-detection { voter-id: voter-id-param }))

(define-read-only (get-voting-behavior (voter-id-param uint))
    (map-get? voting-behavior { voter-id: voter-id-param }))

(define-read-only (get-fraud-report (report-id uint))
    (map-get? fraud-reports { report-id: report-id }))

(define-read-only (is-voter-trustworthy (voter-id-param uint))
    (let
        ((fraud-data (map-get? fraud-detection { voter-id: voter-id-param })))
        (match fraud-data
            data (and 
                (not (get is-flagged data))
                (>= (get reputation-score data) (var-get reputation-threshold)))
            true)))

(define-read-only (get-voter-risk-level (voter-id-param uint))
    (let
        ((fraud-data (map-get? fraud-detection { voter-id: voter-id-param }))
         (behavior-data (map-get? voting-behavior { voter-id: voter-id-param })))
        (match fraud-data
            f-data
            (let
                ((reputation (get reputation-score f-data))
                 (is-flagged (get is-flagged f-data))
                 (rapid-attempts (match behavior-data b-data (get rapid-vote-attempts b-data) u0)))
                (if is-flagged
                    "high"
                    (if (< reputation u50)
                        "medium"
                        (if (> rapid-attempts u3)
                            "medium"
                            "low"))))
            "low")))

(define-public (toggle-fraud-detection)
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (ok (var-set fraud-detection-enabled (not (var-get fraud-detection-enabled))))))

(define-read-only (get-fraud-detection-status)
    {
        enabled: (var-get fraud-detection-enabled),
        reputation-threshold: (var-get reputation-threshold),
        rapid-vote-threshold: (var-get rapid-vote-threshold),
        total-reports: (var-get last-report-id)
    })

(define-map vote-amendments
  { event-id: uint, voter-id: uint }
  {
    original-vote: (string-ascii 50),
    current-vote: (string-ascii 50),
    amendment-count: uint,
    last-amended-at: uint,
    amendment-history: (list 5 { vote: (string-ascii 50), amended-at: uint })
  }
)

(define-data-var amendment-enabled bool true)
(define-data-var amendment-window-blocks uint u144)
(define-data-var max-amendments-per-vote uint u3)

(define-public (amend-vote
    (event-id uint)
    (voter-id-param uint)
    (new-vote-choice (string-ascii 50)))
    (let
        ((event-data (unwrap! (map-get? voting-events { event-id: event-id }) (err u404)))
         (voter-record (unwrap! (map-get? voter-records { voter-id: voter-id-param }) (err u404)))
         (nft-owner (unwrap! (nft-get-owner? voter-id voter-id-param) (err u404)))
         (participation (unwrap! (map-get? voter-participation { event-id: event-id, voter-id: voter-id-param }) (err u404)))
         (amendment-data (map-get? vote-amendments { event-id: event-id, voter-id: voter-id-param }))
         (voted-at (get voted-at participation))
         (current-vote (get vote-choice participation))
         (blocks-since-vote (- stacks-block-height voted-at)))
        (asserts! (var-get amendment-enabled) (err u403))
        (asserts! (not (var-get paused)) (err u403))
        (asserts! (is-eq tx-sender nft-owner) (err u401))
        (asserts! (get active event-data) (err u400))
        (asserts! (< stacks-block-height (get end-block event-data)) (err u406))
        (asserts! (is-eq (get status voter-record) "active") (err u407))
        (asserts! (<= blocks-since-vote (var-get amendment-window-blocks)) (err u412))
        (asserts! (not (is-eq current-vote new-vote-choice)) (err u413))
        (match amendment-data
            existing-amendment
            (let
                ((amendment-count (get amendment-count existing-amendment))
                 (history (get amendment-history existing-amendment)))
                (asserts! (< amendment-count (var-get max-amendments-per-vote)) (err u414))
                (map-set vote-amendments
                    { event-id: event-id, voter-id: voter-id-param }
                    {
                        original-vote: (get original-vote existing-amendment),
                        current-vote: new-vote-choice,
                        amendment-count: (+ amendment-count u1),
                        last-amended-at: stacks-block-height,
                        amendment-history: (unwrap! (as-max-len? (append history { vote: new-vote-choice, amended-at: stacks-block-height }) u5) (err u415))
                    }
                ))
            (map-set vote-amendments
                { event-id: event-id, voter-id: voter-id-param }
                {
                    original-vote: current-vote,
                    current-vote: new-vote-choice,
                    amendment-count: u1,
                    last-amended-at: stacks-block-height,
                    amendment-history: (list { vote: new-vote-choice, amended-at: stacks-block-height })
                }
            )
        )
        (map-set voter-participation
            { event-id: event-id, voter-id: voter-id-param }
            {
                voted-at: voted-at,
                vote-choice: new-vote-choice
            }
        )
        (ok true)))

(define-read-only (get-amendment-data (event-id uint) (voter-id-param uint))
    (map-get? vote-amendments { event-id: event-id, voter-id: voter-id-param }))

(define-read-only (can-amend-vote (event-id uint) (voter-id-param uint))
    (let
        ((event-data (map-get? voting-events { event-id: event-id }))
         (participation (map-get? voter-participation { event-id: event-id, voter-id: voter-id-param }))
         (amendment-data (map-get? vote-amendments { event-id: event-id, voter-id: voter-id-param })))
        (match event-data
            event
            (match participation
                part
                (let
                    ((voted-at (get voted-at part))
                     (blocks-since-vote (- stacks-block-height voted-at))
                     (amendment-count (match amendment-data amend (get amendment-count amend) u0)))
                    {
                        can-amend: (and
                            (var-get amendment-enabled)
                            (get active event)
                            (< stacks-block-height (get end-block event))
                            (<= blocks-since-vote (var-get amendment-window-blocks))
                            (< amendment-count (var-get max-amendments-per-vote))),
                        blocks-remaining: (if (<= blocks-since-vote (var-get amendment-window-blocks))
                            (- (var-get amendment-window-blocks) blocks-since-vote)
                            u0),
                        amendments-remaining: (if (< amendment-count (var-get max-amendments-per-vote))
                            (- (var-get max-amendments-per-vote) amendment-count)
                            u0),
                        amendment-count: amendment-count
                    })
                {
                    can-amend: false,
                    blocks-remaining: u0,
                    amendments-remaining: u0,
                    amendment-count: u0
                })
            {
                can-amend: false,
                blocks-remaining: u0,
                amendments-remaining: u0,
                amendment-count: u0
            })))

(define-read-only (get-vote-history (event-id uint) (voter-id-param uint))
    (let
        ((participation (map-get? voter-participation { event-id: event-id, voter-id: voter-id-param }))
         (amendment-data (map-get? vote-amendments { event-id: event-id, voter-id: voter-id-param })))
        (match participation
            part
            (match amendment-data
                amend
                {
                    original-vote: (get original-vote amend),
                    current-vote: (get current-vote amend),
                    voted-at: (get voted-at part),
                    amendment-count: (get amendment-count amend),
                    last-amended-at: (get last-amended-at amend),
                    has-amendments: true
                }
                {
                    original-vote: (get vote-choice part),
                    current-vote: (get vote-choice part),
                    voted-at: (get voted-at part),
                    amendment-count: u0,
                    last-amended-at: u0,
                    has-amendments: false
                })
            {
                original-vote: "",
                current-vote: "",
                voted-at: u0,
                amendment-count: u0,
                last-amended-at: u0,
                has-amendments: false
            })))

(define-public (toggle-amendment-system)
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (ok (var-set amendment-enabled (not (var-get amendment-enabled))))))

(define-public (update-amendment-window (new-window-blocks uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (asserts! (> new-window-blocks u0) (err u400))
        (ok (var-set amendment-window-blocks new-window-blocks))))

(define-public (update-max-amendments (new-max uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u401))
        (asserts! (and (> new-max u0) (<= new-max u5)) (err u400))
        (ok (var-set max-amendments-per-vote new-max))))

(define-read-only (get-amendment-settings)
    {
        enabled: (var-get amendment-enabled),
        window-blocks: (var-get amendment-window-blocks),
        max-amendments: (var-get max-amendments-per-vote)
    })
