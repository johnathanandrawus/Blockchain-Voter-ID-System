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
