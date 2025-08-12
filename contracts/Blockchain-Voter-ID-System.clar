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