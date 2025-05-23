
;; title: bet-placement
;; version:
;; summary:
;; description:

;; Bet Placement Smart Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-params (err u101))
(define-constant err-event-not-found (err u102))

;; Define data structures
(define-map bets
  { bet-id: uint }
  {
    bettor: principal,
    amount: uint,
    odds: uint,
    event-id: uint,
    status: (string-ascii 20)
  }
)

(define-map events
  { event-id: uint }
  {
    description: (string-ascii 256),
    outcome: (optional (string-ascii 100))
  }
)

(define-data-var next-bet-id uint u1)
(define-data-var next-event-id uint u1)

;; Bet Creation
(define-public (create-bet (amount uint) (odds uint) (event-id uint))
  (let
    (
      (bet-id (var-get next-bet-id))
    )
    (asserts! (> amount u0) err-invalid-params)
    (asserts! (> odds u0) err-invalid-params)
    (asserts! (is-some (get-event event-id)) err-event-not-found)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set bets
      { bet-id: bet-id }
      {
        bettor: tx-sender,
        amount: amount,
        odds: odds,
        event-id: event-id,
        status: "active"
      }
    )
    (var-set next-bet-id (+ bet-id u1))
    (ok bet-id)
  )
)

;; Bet Validation (implemented as part of create-bet)

;; Event Registration
(define-public (register-event (description (string-ascii 256)))
  (let
    (
      (event-id (var-get next-event-id))
    )
    (map-set events
      { event-id: event-id }
      {
        description: description,
        outcome: none
      }
    )
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

;; Helper function to get event details
(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

;; Helper function to get bet details
(define-read-only (get-bet (bet-id uint))
  (map-get? bets { bet-id: bet-id })
)

;; Function to set event outcome (only contract owner can do this)
(define-public (set-event-outcome (event-id uint) (outcome (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (match (get-event event-id)
      event (ok (map-set events { event-id: event-id } (merge event { outcome: (some outcome) })))
      err-event-not-found
    )
  )
)