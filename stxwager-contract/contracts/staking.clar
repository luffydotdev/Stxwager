;; Staking Smart Contract
;; Allows users to stake STX tokens for better odds and dynamic rewards

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-STAKE-LOCKED (err u104))
(define-constant ERR-NO-REWARDS (err u105))
(define-constant ERR-TRANSFER-FAILED (err u106))

;; Data Variables
(define-data-var platform-fee-rate uint u250) ;; 2.5% (250/10000)
(define-data-var base-reward-rate uint u500) ;; 5% base APY (500/10000)
(define-data-var total-staked uint u0)
(define-data-var reward-pool uint u0)

;; Data Maps
(define-map stakes
  { staker: principal }
  {
    amount: uint,
    stake-height: uint,
    last-reward-height: uint,
    total-rewards: uint
  }
)

(define-map stake-multipliers
  { tier: uint }
  { multiplier: uint }
)

;; Initialize stake multipliers
(map-set stake-multipliers { tier: u1 } { multiplier: u10000 }) ;; 1x for < 1000 STX
(map-set stake-multipliers { tier: u2 } { multiplier: u12000 }) ;; 1.2x for 1000-5000 STX
(map-set stake-multipliers { tier: u3 } { multiplier: u15000 }) ;; 1.5x for 5000-10000 STX
(map-set stake-multipliers { tier: u4 } { multiplier: u20000 }) ;; 2x for > 10000 STX

;; Read-only functions
(define-read-only (get-stake (staker principal))
  (map-get? stakes { staker: staker })
)

(define-read-only (get-total-staked)
  (var-get total-staked)
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-stake-tier (amount uint))
  (if (< amount u1000000000) ;; < 1000 STX (in microSTX)
    u1
    (if (< amount u5000000000) ;; < 5000 STX
      u2
      (if (< amount u10000000000) ;; < 10000 STX
        u3
        u4
      )
    )
  )
)

(define-read-only (calculate-rewards (staker principal))
  (match (map-get? stakes { staker: staker })
    stake-data
    (let
      (
        (stake-amount (get amount stake-data))
        (blocks-staked (- stacks-block-height (get last-reward-height stake-data)))
        (tier (get-stake-tier stake-amount))
        (multiplier (default-to u10000 (get multiplier (map-get? stake-multipliers { tier: tier }))))
        (base-reward (/ (* stake-amount (var-get base-reward-rate) blocks-staked) u10000))
        (adjusted-reward (/ (* base-reward multiplier) u10000))
      )
      (ok adjusted-reward)
    )
    (err ERR-NOT-FOUND)
  )
)

;; Public functions
(define-public (stake-tokens (amount uint))
  (let
    (
      (staker tx-sender)
      (current-stake (default-to 
        { amount: u0, stake-height: u0, last-reward-height: u0, total-rewards: u0 }
        (map-get? stakes { staker: staker })
      ))
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount staker (as-contract tx-sender)))
    
    ;; Update stake data
    (map-set stakes
      { staker: staker }
      {
        amount: (+ (get amount current-stake) amount),
        stake-height: (if (is-eq (get amount current-stake) u0) stacks-block-height (get stake-height current-stake)),
        last-reward-height: stacks-block-height,
        total-rewards: (get total-rewards current-stake)
      }
    )
    
    ;; Update total staked
    (var-set total-staked (+ (var-get total-staked) amount))
    
    (ok true)
  )
)

(define-public (claim-rewards)
  (let
    (
      (staker tx-sender)
      (rewards-result (try! (calculate-rewards staker)))
      (reward (> rewards-result u0))
    )
    (asserts! reward (err ERR-NO-REWARDS))
    
    (match (map-get? stakes { staker: staker })
      stake-data
      (begin
        (map-set stakes
          { staker: staker }
          (merge stake-data { 
            last-reward-height: stacks-block-height,
            total-rewards: (+ (get total-rewards stake-data) rewards-result)
          })
        )
        
        ;; Transfer rewards from reward pool
        (match (as-contract (stx-transfer? rewards-result tx-sender staker))
          success (begin
            (var-set reward-pool (- (var-get reward-pool) rewards-result))
            (ok rewards-result)
          )
          error (err ERR-TRANSFER-FAILED)
        )
      )
      (err ERR-NOT-FOUND)
    )
  )
)

(define-public (unstake-tokens (amount uint))
  (let
    (
      (staker tx-sender)
      (stake-data (unwrap! (map-get? stakes { staker: staker }) ERR-NOT-FOUND))
      (staked-amount (get amount stake-data))
    )
    (asserts! (>= staked-amount amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Claim any pending rewards first (optional - ignore errors)
    (match (claim-rewards)
      success true
      error true ;; Continue even if claiming rewards fails
    )
    
    
    ;; Update or remove stake
    (if (is-eq staked-amount amount)
      (map-delete stakes { staker: staker })
      (map-set stakes
        { staker: staker }
        (merge stake-data { amount: (- staked-amount amount) })
      )
    )
    
    ;; Update total staked
    (var-set total-staked (- (var-get total-staked) amount))
    
    ;; Transfer STX back to staker
    (try! (as-contract (stx-transfer? amount tx-sender staker)))
    
    (ok true)
  )
)

(define-public (payout-winnings (winner principal) (winnings uint))
  (let
    (
      (stake-data (unwrap! (map-get? stakes { staker: winner }) ERR-NOT-FOUND))
      (staked-amount (get amount stake-data))
      (platform-fee (/ (* winnings (var-get platform-fee-rate)) u10000))
      (net-winnings (- winnings platform-fee))
    )
    ;; Only contract owner or authorized contracts can call this
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    ;; Transfer winnings to winner
    (try! (as-contract (stx-transfer? net-winnings tx-sender winner)))
    
    ;; Return staked amount
    (try! (as-contract (stx-transfer? staked-amount tx-sender winner)))
    
    ;; Remove stake since it's been paid out
    (map-delete stakes { staker: winner })
    (var-set total-staked (- (var-get total-staked) staked-amount))
    
    ;; Add platform fee to reward pool
    (var-set reward-pool (+ (var-get reward-pool) platform-fee))
    
    (ok { winnings: net-winnings, stake-returned: staked-amount, fee: platform-fee })
  )
)

;; Admin functions
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-rate u1000) ERR-INVALID-AMOUNT) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

(define-public (set-base-reward-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-rate u2000) ERR-INVALID-AMOUNT) ;; Max 20%
    (var-set base-reward-rate new-rate)
    (ok true)
  )
)

(define-public (fund-reward-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool (+ (var-get reward-pool) amount))
    (ok true)
  )
)

(define-public (update-stake-multiplier (tier uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (and (>= tier u1) (<= tier u4)) ERR-INVALID-AMOUNT)
    (asserts! (and (>= multiplier u5000) (<= multiplier u50000)) ERR-INVALID-AMOUNT) ;; 0.5x to 5x
    (map-set stake-multipliers { tier: tier } { multiplier: multiplier })
    (ok true)
  )
)

;; Emergency functions
(define-public (emergency-withdraw)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (try! (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender CONTRACT-OWNER)))
    (ok true)
  )
)
