
;; title: bet-settlement
;; version:
;; summary:
;; description:

;; bet-settlement.clar
;; A smart contract for bet settlement with oracle integration and dispute resolution

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant dispute-period u10) ;; 10 blocks for dispute period
(define-constant oracle-address 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
(define-constant min-bet-amount u1000000) ;; in microSTX (1 STX)
(define-constant dispute-fee u500000) ;; 0.5 STX to file a dispute

;; Error codes
(define-constant err-unauthorized u1)
(define-constant err-event-not-found u2)
(define-constant err-event-already-exists u3)
(define-constant err-bet-not-found u4)
(define-constant err-insufficient-funds u5)
(define-constant err-event-not-settled u6)
(define-constant err-event-already-settled u7)
(define-constant err-dispute-period-active u8)
(define-constant err-dispute-period-ended u9)
(define-constant err-invalid-outcome u10)

;; Data structures
(define-map events 
  { event-id: (string-utf8 36) }
  {
    description: (string-utf8 256),
    options: (list 10 (string-utf8 64)),
    start-block: uint,
    end-block: uint,
    settled: bool,
    outcome: (optional (string-utf8 64)),
    settlement-block: (optional uint),
    total-bets: uint
  }
)

(define-map bets
  { event-id: (string-utf8 36), better: principal }
  {
    amount: uint,
    option: (string-utf8 64),
    paid: bool
  }
)

(define-map disputes
  { event-id: (string-utf8 36), disputer: principal }
  {
    claimed-outcome: (string-utf8 64),
    resolved: bool
  }
)

(define-map event-totals
  { event-id: (string-utf8 36), option: (string-utf8 64) }
  { total-amount: uint }
)

;; Read-only functions
(define-read-only (get-event (event-id (string-utf8 36)))
  (map-get? events { event-id: event-id })
)

(define-read-only (get-bet (event-id (string-utf8 36)) (better principal))
  (map-get? bets { event-id: event-id, better: better })
)

(define-read-only (get-dispute (event-id (string-utf8 36)) (disputer principal))
  (map-get? disputes { event-id: event-id, disputer: disputer })
)

(define-read-only (get-option-total (event-id (string-utf8 36)) (option (string-utf8 64)))
  (default-to { total-amount: u0 } 
    (map-get? event-totals { event-id: event-id, option: option })
  )
)

(define-read-only (is-dispute-period-active (event-id (string-utf8 36)))
  (let ((event (get-event event-id)))
    (if (is-none event)
      false
      (let ((settlement-block (get settlement-block (unwrap-panic event))))
        (if (is-none settlement-block)
          false
          (< stacks-block-height (+ (unwrap-panic settlement-block) dispute-period))
        )
      )
    )
  )
)

;; Public functions

;; Create a new betting event
(define-public (create-event 
    (event-id (string-utf8 36)) 
    (description (string-utf8 256)) 
    (options (list 10 (string-utf8 64)))
    (start-block uint)
    (end-block uint))
  (begin
    ;; Only contract owner or oracle can create events
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender oracle-address)) (err err-unauthorized))
    ;; Check if event already exists
    (asserts! (is-none (get-event event-id)) (err err-event-already-exists))
    ;; Validate event parameters
    (asserts! (> (len options) u0) (err err-invalid-outcome))
    (asserts! (>= end-block start-block) (err err-invalid-outcome))
    
    ;; Store the event
    (map-set events
      { event-id: event-id }
      {
        description: description,
        options: options,
        start-block: start-block,
        end-block: end-block,
        settled: false,
        outcome: none,
        settlement-block: none,
        total-bets: u0
      }
    )
    (ok true)
  )
)

;; Place a bet on an even