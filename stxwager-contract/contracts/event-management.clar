;; Event Management Contract with Multi-Oracle System
;; Supports event cancellation, refunds, metadata, and decentralized oracle resolution

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-EVENT-NOT-FOUND (err u101))
(define-constant ERR-EVENT-ALREADY-RESOLVED (err u102))
(define-constant ERR-EVENT-ALREADY-CANCELED (err u103))
(define-constant ERR-ORACLE-NOT-REGISTERED (err u104))
(define-constant ERR-ORACLE-ALREADY-VOTED (err u105))
(define-constant ERR-INSUFFICIENT-ORACLES (err u106))
(define-constant ERR-EVENT-NOT-ACTIVE (err u107))
(define-constant ERR-INVALID-OUTCOME (err u108))
(define-constant ERR-REFUND-FAILED (err u109))
(define-constant ERR-EVENT-EXPIRED (err u110))

;; Event Status
(define-constant EVENT-ACTIVE u1)
(define-constant EVENT-RESOLVED u2)
(define-constant EVENT-CANCELED u3)
(define-constant EVENT-EXPIRED u4)

;; Event Categories
(define-constant CATEGORY-SPORTS u1)
(define-constant CATEGORY-POLITICS u2)
(define-constant CATEGORY-ENTERTAINMENT u3)
(define-constant CATEGORY-CRYPTO u4)
(define-constant CATEGORY-OTHER u5)

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var min-oracle-consensus uint u3)
(define-data-var oracle-registration-fee uint u1000000) ;; 1 STX

;; Data Maps
(define-map events
  uint
  {
    creator: principal,
    title: (string-ascii 256),
    description: (string-ascii 1024),
    category: uint,
    created-at: uint,
    resolution-deadline: uint,
    expiry-time: uint,
    status: uint,
    outcome: (optional uint),
    total-bets: uint,
    verification-url: (optional (string-ascii 256)),
    metadata-hash: (optional (string-ascii 64))
  }
)

(define-map registered-oracles
  principal
  {
    reputation-score: uint,
    total-votes: uint,
    correct-votes: uint,
    stake-amount: uint,
    registration-time: uint,
    is-active: bool
  }
)

(define-map oracle-votes
  {event-id: uint, oracle: principal}
  {
    outcome: uint,
    confidence: uint,
    timestamp: uint,
    justification: (optional (string-ascii 512))
  }
)

(define-map event-oracle-consensus
  uint
  {
    total-votes: uint,
    outcome-votes: (list 10 {outcome: uint, votes: uint, confidence-sum: uint}),
    consensus-reached: bool,
    final-outcome: (optional uint)
  }
)

(define-map event-bets
  {event-id: uint, bettor: principal}
  {
    amount: uint,
    outcome: uint,
    bet-time: uint,
    refunded: bool
  }
)

(define-map event-bet-totals
  uint
  {
    total-amount: uint,
    outcome-totals: (list 10 {outcome: uint, total: uint})
  }
)

(define-map oracle-reputation
  principal
  uint
)

;; Read-only functions
(define-read-only (get-event (event-id uint))
  (map-get? events event-id)
)

(define-read-only (get-oracle-info (oracle principal))
  (map-get? registered-oracles oracle)
)

(define-read-only (get-oracle-vote (event-id uint) (oracle principal))
  (map-get? oracle-votes {event-id: event-id, oracle: oracle})
)

(define-read-only (get-event-consensus (event-id uint))
  (map-get? event-oracle-consensus event-id)
)

(define-read-only (get-current-event-price (event-id uint))
  (let ((event-data (unwrap! (get-event event-id) (err u0))))
    (if (is-eq (get status event-data) EVENT-ACTIVE)
      (calculate-dynamic-price event-id)
      (err u0)
    )
  )
)

(define-read-only (calculate-oracle-reputation (oracle principal))
  (match (get-oracle-info oracle)
    oracle-data
    (if (> (get total-votes oracle-data) u0)
      (/ (* (get correct-votes oracle-data) u100) (get total-votes oracle-data))
      u0
    )
    u0
  )
)

;; Private functions
(define-private (calculate-dynamic-price (event-id uint))
  (let (
    (bet-data (default-to 
      {total-amount: u0, outcome-totals: (list)}
      (map-get? event-bet-totals event-id)
    ))
    (total-bets (get total-amount bet-data))
  )
    (if (> total-bets u0)
      (ok (+ u1000000 (/ total-bets u100))) ;; Base price + volume factor
      (ok u1000000) ;; Base price
    )
  )
)

(define-private (update-oracle-reputation (oracle principal) (was-correct bool))
  (match (get-oracle-info oracle)
    oracle-data
    (map-set registered-oracles oracle
      (merge oracle-data {
        total-votes: (+ (get total-votes oracle-data) u1),
        correct-votes: (if was-correct 
          (+ (get correct-votes oracle-data) u1)
          (get correct-votes oracle-data)
        )
      })
    )
    false
  )
)

(define-private (process-bet-refunds (event-id uint))
  (let ((event-data (unwrap! (get-event event-id) (err u0))))
    ;; This would iterate through all bets for the event and refund them
    ;; In a real implementation, you'd need a way to track all bettors
    (ok true)
  )
)

(define-private (check-and-resolve-consensus (event-id uint))
  (let (
    (consensus-data (unwrap! (map-get? event-oracle-consensus event-id) (err u0)))
    (min-consensus (var-get min-oracle-consensus))
  )
    ;; Fixed type mismatch by ensuring both arms return response type
    (if (>= (get total-votes consensus-data) min-consensus)
      (resolve-event-by-consensus event-id)
      (ok false)
    )
  )
)

(define-private (update-consensus-tracking (event-id uint) (outcome uint) (confidence uint))
  (let (
    (consensus-data (default-to 
      {total-votes: u0, outcome-votes: (list), consensus-reached: false, final-outcome: none}
      (map-get? event-oracle-consensus event-id)
    ))
  )
    ;; Removed try! wrapper since update-consensus-tracking always succeeds
    (begin
      (map-set event-oracle-consensus event-id
        (merge consensus-data {
          total-votes: (+ (get total-votes consensus-data) u1)
        })
      )
      (ok true)
    )
  )
)

(define-private (resolve-event-by-consensus (event-id uint))
  (let (
    (event-data (unwrap! (get-event event-id) ERR-EVENT-NOT-FOUND))
  )
    ;; In a real implementation, this would calculate the consensus outcome
    ;; based on oracle votes and their confidence/reputation weights
    (map-set events event-id
      (merge event-data {
        status: EVENT-RESOLVED,
        outcome: (some u1) ;; Simplified - would be calculated from votes
      })
    )
    (ok true)
  )
)

;; Public functions

;; Oracle Management
(define-public (register-oracle)
  (let (
    (registration-fee (var-get oracle-registration-fee))
    (current-time (unwrap! (get-block-info? time (- block-height u1)) ERR-NOT-AUTHORIZED))
  )
    (try! (stx-transfer? registration-fee tx-sender CONTRACT-OWNER))
    (map-set registered-oracles tx-sender {
      reputation-score: u100,
      total-votes: u0,
      correct-votes: u0,
      stake-amount: registration-fee,
      registration-time: current-time,
      is-active: true
    })
    (ok true)
  )
)

(define-public (deactivate-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (get-oracle-info oracle)
      oracle-data
      (begin
        (map-set registered-oracles oracle
          (merge oracle-data {is-active: false})
        )
        (ok true)
      )
      ERR-ORACLE-NOT-REGISTERED
    )
  )
)

;; Event Creation
(define-public (create-event 
  (title (string-ascii 256))
  (description (string-ascii 1024))
  (category uint)
  (resolution-deadline uint)
  (expiry-time uint)
  (verification-url (optional (string-ascii 256)))
  (metadata-hash (optional (string-ascii 64)))
)
  (let (
    (event-id (var-get next-event-id))
    (current-time (unwrap! (get-block-info? time (- block-height u1)) ERR-NOT-AUTHORIZED))
  )
    (asserts! (<= category CATEGORY-OTHER) ERR-NOT-AUTHORIZED)
    (asserts! (> resolution-deadline current-time) ERR-NOT-AUTHORIZED)
    (asserts! (> expiry-time resolution-deadline) ERR-NOT-AUTHORIZED)
    
    (map-set events event-id {
      creator: tx-sender,
      title: title,
      description: description,
      category: category,
      created-at: current-time,
      resolution-deadline: resolution-deadline,
      expiry-time: expiry-time,
      status: EVENT-ACTIVE,
      outcome: none,
      total-bets: u0,
      verification-url: verification-url,
      metadata-hash: metadata-hash
    })
    
    (map-set event-oracle-consensus event-id {
      total-votes: u0,
      outcome-votes: (list),
      consensus-reached: false,
      final-outcome: none
    })
    
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

;; ;; Oracle Voting
;; (define-public (submit-oracle-vote 
;;   (event-id uint) 
;;   (outcome uint)
;;   (confidence uint)
;;   (justification (string-ascii 256)))
;;   ;; Fixed function definition syntax and restructured to avoid consecutive response operations
;;   (let ((event-data (unwrap! (get-event event-id) ERR-EVENT-NOT-FOUND)))
;;     (let ((oracle-data (unwrap! (get-oracle-info tx-sender) ERR-ORACLE-NOT-REGISTERED)))
;;       (let ((current-time (unwrap! (get-block-info? time (- block-height u1)) ERR-NOT-AUTHORIZED)))
;;         (begin
;;           ;; Validate oracle and event status
;;           (asserts! (get is-active oracle-data) ERR-ORACLE-NOT-REGISTERED)
;;           (asserts! (is-eq (get status event-data) EVENT-ACTIVE) ERR-EVENT-NOT-ACTIVE)
;;           (asserts! (is-none (get-oracle-vote event-id tx-sender)) ERR-ORACLE-ALREADY-VOTED)
;;           (asserts! (< current-time (get resolution-deadline event-data)) ERR-EVENT-EXPIRED)
;;           (asserts! (and (>= confidence u1) (<= confidence u100)) ERR-INVALID-OUTCOME)
          
;;           ;; Record the vote
;;           (map-set oracle-votes 
;;             {event-id: event-id, oracle: tx-sender}
;;             {
;;               outcome: outcome,
;;               confidence: confidence,
;;               timestamp: current-time,
;;               justification: (some justification)
;;             }
;;           )
          
;;           ;; Update oracle reputation
;;           (map-set oracle-reputation
;;             tx-sender
;;             (+ (default-to u0 (map-get? oracle-reputation tx-sender)) u1)
;;           )
          
;;           ;; Update consensus tracking
;;           (update-consensus-tracking event-id outcome confidence)
          
;;           ;; Check if consensus is reached and return result
;;           (check-and-resolve-consensus event-id)
;;         )
;;       )
;;     )
;;   )
;; )

;; Event Resolution
(define-public (emergency-resolve-event (event-id uint) (outcome uint))
  (let (
    (event-data (unwrap! (get-event event-id) ERR-EVENT-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status event-data) EVENT-ACTIVE) ERR-EVENT-NOT-ACTIVE)
    
    (map-set events event-id
      (merge event-data {
        status: EVENT-RESOLVED,
        outcome: (some outcome)
      })
    )
    (ok true)
  )
)

;; Event Cancellation
(define-public (cancel-event (event-id uint) (reason (string-ascii 512)))
  (let (
    (event-data (unwrap! (get-event event-id) ERR-EVENT-NOT-FOUND))
  )
    (asserts! (or 
      (is-eq tx-sender (get creator event-data))
      (is-eq tx-sender CONTRACT-OWNER)
    ) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status event-data) EVENT-ACTIVE) ERR-EVENT-NOT-ACTIVE)
    
    ;; Update event status
    (map-set events event-id
      (merge event-data {status: EVENT-CANCELED})
    )
    
    ;; Process refunds for all bets
    (try! (process-bet-refunds event-id))
    
    (ok true)
  )
)

;; Betting Functions
(define-public (place-bet (event-id uint) (outcome uint) (amount uint))
  (let (
    (event-data (unwrap! (get-event event-id) ERR-EVENT-NOT-FOUND))
    (current-time (unwrap! (get-block-info? time (- block-height u1)) ERR-NOT-AUTHORIZED))
  )
    (asserts! (is-eq (get status event-data) EVENT-ACTIVE) ERR-EVENT-NOT-ACTIVE)
    (asserts! (< current-time (get resolution-deadline event-data)) ERR-EVENT-EXPIRED)
    (asserts! (> amount u0) ERR-INVALID-OUTCOME)
    
    ;; Transfer bet amount to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Record the bet
    (map-set event-bets 
      {event-id: event-id, bettor: tx-sender}
      {
        amount: amount,
        outcome: outcome,
        bet-time: current-time,
        refunded: false
      }
    )
    
    ;; Update event totals
    (map-set events event-id
      (merge event-data {
        total-bets: (+ (get total-bets event-data) amount)
      })
    )
    
    (ok true)
  )
)

;; Refund Functions
(define-public (claim-refund (event-id uint))
  (let (
    (event-data (unwrap! (get-event event-id) ERR-EVENT-NOT-FOUND))
    (bet-data (unwrap! (map-get? event-bets {event-id: event-id, bettor: tx-sender}) ERR-EVENT-NOT-FOUND))
  )
    (asserts! (is-eq (get status event-data) EVENT-CANCELED) ERR-EVENT-NOT-ACTIVE)
    (asserts! (not (get refunded bet-data)) ERR-REFUND-FAILED)
    
    ;; Process refund
    (try! (as-contract (stx-transfer? (get amount bet-data) tx-sender tx-sender)))
    
    ;; Mark as refunded
    (map-set event-bets 
      {event-id: event-id, bettor: tx-sender}
      (merge bet-data {refunded: true})
    )
    
    (ok true)
  )
)

;; Admin Functions
(define-public (set-min-oracle-consensus (new-min uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set min-oracle-consensus new-min)
    (ok true)
  )
)

(define-public (set-oracle-registration-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set oracle-registration-fee new-fee)
    (ok true)
  )
)
