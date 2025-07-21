;; Dynamic Staking Rewards Smart Contract
;; Features: Dynamic reward calculation based on participation and duration
;; Auto-payout on bet wins with staked token return

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-BET-NOT-ACTIVE (err u105))
(define-constant ERR-UNAUTHORIZED (err u106))
(define-constant ERR-INSUFFICIENT-REWARDS (err u107))

;; Data Variables
(define-data-var platform-fee-rate uint u250) ;; 2.5% (250/10000)
(define-data-var base-reward-rate uint u500) ;; 5% (500/10000)
(define-data-var reward-pool uint u0) ;; Pool for paying rewards
(define-data-var total-staked uint u0)
(define-data-var next-stake-id uint u1)
(define-data-var next-bet-id uint u1)

;; Constants for reward calculation
(define-constant BLOCKS-PER-DAY u144) ;; ~10 min blocks, 144 blocks per day
(define-constant PRECISION u10000000000) ;; Higher precision for calculations

;; Data Maps
(define-map stakes
  { stake-id: uint }
  {
    staker: principal,
    amount: uint,
    start-block: uint,
    last-claim-block: uint,
    active: bool
  }
)

(define-map user-stakes
  { user: principal }
  { stake-ids: (list 100 uint) }
)

(define-map bets
  { bet-id: uint }
  {
    bettor: principal,
    staked-amount: uint,
    stake-id: uint,
    bet-amount: uint,
    odds: uint, ;; multiplier * 1000 (e.g., 2000 = 2x)
    active: bool,
    won: bool,
    settled: bool
  }
)

;; Private Functions

;; Calculate dynamic reward rate based on amount and duration
(define-private (calculate-dynamic-rate (amount uint) (duration uint))
  (let
    (
      (amount-bonus (if (>= amount u1000000) u200 ;; 2% bonus for large stakes (>= 1M STX)
                   (if (>= amount u100000) u100 ;; 1% bonus for medium stakes (>= 100K STX)
                       u0))) ;; no bonus for small stakes
      (duration-bonus (if (>= duration (* BLOCKS-PER-DAY u10)) u300 ;; 3% bonus for 10+ days
                      (if (>= duration (* BLOCKS-PER-DAY u5)) u150 ;; 1.5% bonus for 5+ days
                          u0))) ;; no bonus for short duration
    )
    (+ (var-get base-reward-rate) amount-bonus duration-bonus)
  )
)

;; Calculate pending rewards for a stake - Fixed calculation
(define-private (calculate-pending-rewards (stake-id uint))
  (match (map-get? stakes { stake-id: stake-id })
    stake-data
    (let
      (
        (current-block stacks-block-height)
        (blocks-since-last-claim (- current-block (get last-claim-block stake-data)))
        (duration (- current-block (get start-block stake-data)))
        (dynamic-rate (calculate-dynamic-rate (get amount stake-data) duration))
        ;; Fixed reward calculation: annual rate converted to per-block rate
        ;; Formula: (amount * rate * blocks) / (blocks_per_year * 10000)
        (blocks-per-year (* BLOCKS-PER-DAY u365))
        (reward-numerator (* (* (get amount stake-data) dynamic-rate) blocks-since-last-claim))
        (reward-denominator (* blocks-per-year u10000))
        (rewards (/ reward-numerator reward-denominator))
      )
      (if (and (> blocks-since-last-claim u0) (get active stake-data))
          (ok rewards)
          (ok u0))
    )
    ERR-NOT-FOUND
  )
)

;; Update user stake list - Fixed to return proper response
(define-private (add-stake-to-user (user principal) (stake-id uint))
  (match (map-get? user-stakes { user: user })
    existing-stakes
    (begin
      (map-set user-stakes
        { user: user }
        { stake-ids: (unwrap! (as-max-len? (append (get stake-ids existing-stakes) stake-id) u100) (err u999)) }
      )
      (ok true)
    )
    (begin
      (map-set user-stakes
        { user: user }
        { stake-ids: (list stake-id) }
      )
      (ok true)
    )
  )
)

;; Public Functions

;; Add funds to reward pool (only owner)
(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Add to reward pool
    (var-set reward-pool (+ (var-get reward-pool) amount))
    
    (ok amount)
  )
)

;; Stake tokens
(define-public (stake-tokens (amount uint))
  (let
    (
      (stake-id (var-get next-stake-id))
      (current-block stacks-block-height)
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Create stake record
    (map-set stakes
      { stake-id: stake-id }
      {
        staker: tx-sender,
        amount: amount,
        start-block: current-block,
        last-claim-block: current-block,
        active: true
      }
    )
    
    ;; Update user stakes
    (try! (add-stake-to-user tx-sender stake-id))
    
    ;; Update contract state
    (var-set total-staked (+ (var-get total-staked) amount))
    (var-set next-stake-id (+ stake-id u1))
    
    (ok stake-id)
  )
)

;; Claim staking rewards - Fixed implementation
(define-public (claim-rewards (stake-id uint))
  (let ((stake-data-opt (map-get? stakes { stake-id: stake-id })))
    (match stake-data-opt
      stake-data
      (begin
        (asserts! (is-eq (get staker stake-data) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get active stake-data) ERR-NOT-FOUND)
        
        (match (calculate-pending-rewards stake-id)
          rewards
          (begin
            ;; Check if there are rewards to claim and sufficient pool
            (if (> rewards u0)
                (begin
                  (asserts! (>= (var-get reward-pool) rewards) ERR-INSUFFICIENT-REWARDS)
                  
                  ;; Update last claim block BEFORE transfer
                  (map-set stakes
                    { stake-id: stake-id }
                    (merge stake-data { last-claim-block: stacks-block-height })
                  )
                  
                  ;; Transfer rewards from contract to user
                  (try! (as-contract (stx-transfer? rewards tx-sender (get staker stake-data))))
                  
                  ;; Reduce reward pool
                  (var-set reward-pool (- (var-get reward-pool) rewards))
                  
                  (ok rewards)
                )
                ;; No rewards to claim, just update timestamp
                (begin
                  (map-set stakes
                    { stake-id: stake-id }
                    (merge stake-data { last-claim-block: stacks-block-height })
                  )
                  (ok u0)
                )
            )
          )
          error (err error)
        )
      )
      ERR-NOT-FOUND
    )
  )
)

;; Place a bet using staked tokens
(define-public (place-bet (stake-id uint) (bet-amount uint) (odds uint))
  (match (map-get? stakes { stake-id: stake-id })
    stake-data
    (let
      (
        (bet-id (var-get next-bet-id))
      )
      (asserts! (is-eq (get staker stake-data) tx-sender) ERR-UNAUTHORIZED)
      (asserts! (get active stake-data) ERR-NOT-FOUND)
      (asserts! (>= (get amount stake-data) bet-amount) ERR-INSUFFICIENT-BALANCE)
      (asserts! (> bet-amount u0) ERR-INVALID-AMOUNT)
      (asserts! (> odds u1000) ERR-INVALID-AMOUNT) ;; odds must be > 1.0
      
      ;; Create bet record
      (map-set bets
        { bet-id: bet-id }
        {
          bettor: tx-sender,
          staked-amount: (get amount stake-data),
          stake-id: stake-id,
          bet-amount: bet-amount,
          odds: odds,
          active: true,
          won: false,
          settled: false
        }
      )
      
      (var-set next-bet-id (+ bet-id u1))
      (ok bet-id)
    )
    ERR-NOT-FOUND
  )
)

;; Settle bet (admin function for demo - in production would be automated or use oracle)
(define-public (settle-bet (bet-id uint) (won bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    (match (map-get? bets { bet-id: bet-id })
      bet-data
      (begin
        (asserts! (get active bet-data) ERR-BET-NOT-ACTIVE)
        (asserts! (not (get settled bet-data)) ERR-BET-NOT-ACTIVE)
        
        ;; Update bet status
        (map-set bets
          { bet-id: bet-id }
          (merge bet-data { 
            won: won, 
            settled: true, 
            active: false 
          })
        )
        
        ;; Process payout if won
       ;; Process payout if won
        (if won
          (begin
            (try! (process-winning-payout bet-id))
            (ok true)
          )
          (ok true)
        )
      )
      ERR-NOT-FOUND
    )
  )
)

;; Process winning payout with auto-return of staked tokens
(define-public (process-winning-payout (bet-id uint))
  (match (map-get? bets { bet-id: bet-id })
    bet-data
    (let
      (
        (winnings (/ (* (get bet-amount bet-data) (get odds bet-data)) u1000))
        (platform-fee (/ (* winnings (var-get platform-fee-rate)) u10000))
        (net-winnings (- winnings platform-fee))
        (total-payout (+ (get staked-amount bet-data) net-winnings))
      )
      (asserts! (get won bet-data) ERR-BET-NOT-ACTIVE)
      (asserts! (get settled bet-data) ERR-BET-NOT-ACTIVE)
      
      ;; Transfer total payout (staked amount + net winnings) to bettor
      (try! (as-contract (stx-transfer? total-payout tx-sender (get bettor bet-data))))
      
      ;; Update stake to inactive (tokens returned)
      (match (map-get? stakes { stake-id: (get stake-id bet-data) })
        stake-data
        (map-set stakes
          { stake-id: (get stake-id bet-data) }
          (merge stake-data { active: false })
        )
        false
      )
      
      ;; Update total staked
      (var-set total-staked (- (var-get total-staked) (get staked-amount bet-data)))
      
      (ok total-payout)
    )
    ERR-NOT-FOUND
  )
)

;; Unstake tokens (if not used in active bet)
(define-public (unstake-tokens (stake-id uint))
  (match (map-get? stakes { stake-id: stake-id })
    stake-data
    (begin
      (asserts! (is-eq (get staker stake-data) tx-sender) ERR-UNAUTHORIZED)
      (asserts! (get active stake-data) ERR-NOT-FOUND)
      
      ;; First claim any pending rewards
      (try! (claim-rewards stake-id))
      
      ;; Transfer staked amount back to user
      (try! (as-contract (stx-transfer? (get amount stake-data) tx-sender (get staker stake-data))))
      
      ;; Update stake to inactive
      (map-set stakes
        { stake-id: stake-id }
        (merge stake-data { active: false })
      )
      
      ;; Update total staked
      (var-set total-staked (- (var-get total-staked) (get amount stake-data)))
      
      (ok (get amount stake-data))
    )
    ERR-NOT-FOUND
  )
)

;; Read-only functions

;; Get stake details
(define-read-only (get-stake-details (stake-id uint))
  (map-get? stakes { stake-id: stake-id })
)

;; Get bet details
(define-read-only (get-bet-details (bet-id uint))
  (map-get? bets { bet-id: bet-id })
)

;; Get user stakes
(define-read-only (get-user-stakes (user principal))
  (map-get? user-stakes { user: user })
)

;; Get pending rewards for a stake
(define-read-only (get-pending-rewards (stake-id uint))
  (calculate-pending-rewards stake-id)
)

;; Get total staked amount
(define-read-only (get-total-staked)
  (var-get total-staked)
)

;; Get reward pool balance
(define-read-only (get-reward-pool)
  (var-get reward-pool)
)

;; Get current reward rates for given amount and duration
(define-read-only (get-dynamic-rate (amount uint) (duration uint))
  (calculate-dynamic-rate amount duration)
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Admin functions

;; Update platform fee rate (only owner)
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-rate u1000) ERR-INVALID-AMOUNT) ;; max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

;; Update base reward rate (only owner)
(define-public (set-base-reward-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-rate u2000) ERR-INVALID-AMOUNT) ;; max 20%
    (var-set base-reward-rate new-rate)
    (ok true)
  )
)

;; Emergency withdraw (only owner)
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    (ok amount)
  )
)