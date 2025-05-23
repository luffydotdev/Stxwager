;; p2p-betting.clar
;; A P2P betting smart contract for the Stacks blockchain

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POOL-CLOSED (err u101))
(define-constant ERR-POOL-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-RESOLVED (err u104))
(define-constant ERR-TIMEOUT-NOT-REACHED (err u105))
(define-constant ERR-INVALID-OUTCOME (err u106))
(define-constant ERR-ALREADY-JOINED (err u107))

;; Pool status constants
(define-constant STATUS-OPEN u1)
(define-constant STATUS-CLOSED u2)
(define-constant STATUS-RESOLVED u3)
(define-constant STATUS-CANCELED u4)

;; Data structures

;; Bet pool structure
(define-map betting-pools
  { pool-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    status: uint,
    total-stake: uint,
    min-stake: uint,
    max-participants: uint,
    current-participants: uint,
    timeout-height: uint,
    oracle: principal,
    possible-outcomes: (list 10 (string-utf8 50)),
    winning-outcome: (optional (string-utf8 50))
  }
)

;; Track user stakes in pools
(define-map pool-stakes
  { pool-id: uint, user: principal }
  {
    stake-amount: uint,
    outcome-prediction: (string-utf8 50)
  }
)

;; Track participants in each pool
(define-map pool-participants
  { pool-id: uint }
  { participants: (list 100 principal) }
)

;; Track total winnings for each user
(define-map user-winnings
  { user: principal }
  { amount: uint }
)

;; Counter for pool IDs
(define-data-var next-pool-id uint u1)

;; Functions

;; Create a new betting pool
(define-public (create-betting-pool 
                (title (string-utf8 100))
                (description (string-utf8 500))
                (min-stake uint)
                (max-participants uint)
                (timeout-blocks uint)
                (oracle principal)
                (possible-outcomes (list 10 (string-utf8 50))))
  (let ((pool-id (var-get next-pool-id))
        (timeout-height (+ stacks-block-height timeout-blocks)))
    
    ;; Update the next pool ID
    (var-set next-pool-id (+ pool-id u1))
    
    ;; Create the pool
    (map-set betting-pools
      { pool-id: pool-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        status: STATUS-OPEN,
        total-stake: u0,
        min-stake: min-stake,
        max-participants: max-participants,
        current-participants: u0,
        timeout-height: timeout-height,
        oracle: oracle,
        possible-outcomes: possible-outcomes,
        winning-outcome: none
      }
    )
    
    ;; Initialize empty participants list
    (map-set pool-participants
      { pool-id: pool-id }
      { participants: (list) }
    )
    
    (ok pool-id)
  )
)

;; Join a betting pool and place a bet
(define-public (place-bet (pool-id uint) (stake-amount uint) (outcome-prediction (string-utf8 50)))
  (let ((pool (unwrap! (map-get? betting-pools { pool-id: pool-id }) (err ERR-POOL-NOT-FOUND)))
        (participants-data (unwrap! (map-get? pool-participants { pool-id: pool-id }) (err ERR-POOL-NOT-FOUND))))
    
    ;; Check if pool is open
    (asserts! (is-eq (get status pool) STATUS-OPEN) (err ERR-POOL-CLOSED))
    
    ;; Check if stake meets minimum
    (asserts! (>= stake-amount (get min-stake pool)) (err ERR-INSUFFICIENT-FUNDS))
    
    ;; Check if outcome is valid
    (asserts! (is-some (index-of (get possible-outcomes pool) outcome-prediction)) (err ERR-INVALID-OUTCOME))
    
    ;; Check if user already joined
    (asserts! (is-none (map-get? pool-stakes { pool-id: pool-id, user: tx-sender })) (err ERR-ALREADY-JOINED))
    
    ;; Check if max participants reached
    (asserts! (< (get current-participants pool) (get max-participants pool)) (err ERR-POOL-CLOSED))
    
    ;; Transfer stake to contract
    (unwrap! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)) (err ERR-INSUFFICIENT-FUNDS))
    
    ;; Update pool total stake
    (map-set betting-pools
      { pool-id: pool-id }
      (merge pool {
        total-stake: (+ (get total-stake pool) stake-amount),
        current-participants: (+ (get current-participants pool) u1)
      })
    )
    
    ;; Record user stake
    (map-set pool-stakes
      { pool-id: pool-id, user: tx-sender }
      {
        stake-amount: stake-amount,
        outcome-prediction: outcome-prediction
      }
    )
    
    
    (ok true)
  )
)

;; Close betting period (only creator can do this)
(define-public (close-betting-period (pool-id uint))
  (let ((pool (unwrap! (map-get? betting-pools { pool-id: pool-id }) (err ERR-POOL-NOT-FOUND))))
    
    ;; Check if sender is creator
    (asserts! (is-eq tx-sender (get creator pool)) (err ERR-NOT-AUTHORIZED))
    
    ;; Check if pool is still open
    (asserts! (is-eq (get status pool) STATUS-OPEN) (err ERR-POOL-CLOSED))
    
    ;; Update pool status to closed
    (map-set betting-pools
      { pool-id: pool-id }
      (merge pool { status: STATUS-CLOSED })
    )
    
    (ok true)
  )
)


;; Helper to calculate total stake of winners
(define-private (calculate-winners-stake (user principal) (result { pool-id: uint, outcome: (string-utf8 50), total: uint }))
  (let ((stake-data (map-get? pool-stakes { pool-id: (get pool-id result), user: user })))
    (if (and (is-some stake-data) 
             (is-eq (get outcome-prediction (unwrap-panic stake-data)) (get outcome result)))
      { pool-id: (get pool-id result), outcome: (get outcome result), total: (+ (get total result) (get stake-amount (unwrap-panic stake-data))) }
      result
    )
  )
)




;; Read-only functions

;; Get pool details
(define-read-only (get-pool-details (pool-id uint))
  (map-get? betting-pools { pool-id: pool-id })
)

;; Get user stake in a pool
(define-read-only (get-user-stake (pool-id uint) (user principal))
  (map-get? pool-stakes { pool-id: pool-id, user: user })
)

;; Get pool participants
(define-read-only (get-pool-participants (pool-id uint))
  (map-get? pool-participants { pool-id: pool-id })
)

;; Get user total winnings
(define-read-only (get-user-winnings (user principal))
  (default-to { amount: u0 } (map-get? user-winnings { user: user }))
)

;; Check if user is in a pool
(define-read-only (is-user-in-pool (pool-id uint) (user principal))
  (is-some (map-get? pool-stakes { pool-id: pool-id, user: user }))
)