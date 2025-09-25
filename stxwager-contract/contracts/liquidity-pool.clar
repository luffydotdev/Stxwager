;; Liquidity Pool & Yield Farming Smart Contract
;; Comprehensive system for liquidity provision, proportional payouts, and yield farming

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED u100)
(define-constant ERR-INSUFFICIENT-BALANCE u101)
(define-constant ERR-INVALID-AMOUNT u102)
(define-constant ERR-POOL-NOT-FOUND u103)
(define-constant ERR-NO-LIQUIDITY u104)
(define-constant ERR-ALREADY-EXISTS u105)
(define-constant ERR-REWARD-CALCULATION-FAILED u106)
(define-constant  ERR-INSUFFICIENT_BALANCE u108)
(define-constant ERR-WITHDRAWAL-TOO-LARGE u107)
(define-constant ERR-POOL-PAUSED u108)
(define-constant ERR-MINIMUM-DEPOSIT u109)
(define-constant ERR-LOCK-PERIOD-ACTIVE u110)

;; Contract Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant PRECISION u1000000) ;; 6 decimal places for calculations
(define-constant MIN_DEPOSIT u1000000) ;; 1 STX minimum deposit
(define-constant BLOCKS_PER_DAY u144) ;; Approximate blocks per day
(define-constant MAX_POOLS u50)
(define-constant FEE_DENOMINATOR u10000) ;; For percentage calculations (100.00%)

;; Data Variables
(define-data-var next-pool-id uint u1)
(define-data-var total-platform-fees uint u0)
(define-data-var platform-fee-rate uint u30) ;; 0.30% platform fee
(define-data-var reward-distribution-interval uint u144) ;; Daily reward distribution

;; Liquidity Pool Structure
(define-map liquidity-pools
  uint ;; pool-id
  {
    name: (string-ascii 50),
    total-liquidity: uint,
    total-shares: uint,
    accumulated-fees: uint,
    reward-rate: uint, ;; Rewards per block per share (scaled by PRECISION)
    last-reward-block: uint,
    accumulated-reward-per-share: uint,
    is-active: bool,
    created-at: uint,
    lock-period: uint, ;; Minimum lock period in blocks
    early-withdrawal-penalty: uint ;; Penalty percentage (in basis points)
  }
)

;; User Liquidity Positions
(define-map user-positions
  {user: principal, pool-id: uint}
  {
    shares: uint,
    deposit-amount: uint,
    deposited-at: uint,
    last-reward-claim: uint,
    reward-debt: uint, ;; Tracks rewards already accounted for
    total-rewards-earned: uint
  }
)

;; Pool Fee Distribution
(define-map pool-fees
  uint ;; pool-id
  {
    total-fees-collected: uint,
    fees-per-share: uint,
    last-fee-distribution: uint
  }
)

;; Yield Farming Rewards
(define-map farming-rewards
  {user: principal, pool-id: uint}
  {
    pending-rewards: uint,
    claimed-rewards: uint,
    last-claim-block: uint,
    stake-duration: uint ;; For bonus calculations
  }
)

;; Pool Performance Tracking
(define-map pool-stats
  uint ;; pool-id
  {
    total-deposits: uint,
    total-withdrawals: uint,
    total-rewards-distributed: uint,
    unique-depositors: uint,
    apr: uint, ;; Annual percentage rate (scaled by PRECISION)
    volume-24h: uint
  }
)

;; User Indexes
(define-map user-pool-list principal (list 20 uint))
(define-map pool-user-list uint (list 1000 principal))




;; Calculate proportional share based on deposit amount
(define-private (calculate-shares (deposit-amount uint) (total-liquidity uint) (total-shares uint))
  (if (is-eq total-shares u0)
    ;; First deposit gets shares equal to deposit amount
    deposit-amount
    ;; Subsequent deposits get proportional shares
    (/ (* deposit-amount total-shares) total-liquidity)
  )
)

;; Calculate withdrawal amount based on shares
(define-private (calculate-withdrawal-amount (shares uint) (total-liquidity uint) (total-shares uint))
  (if (is-eq total-shares u0)
    u0
    (/ (* shares total-liquidity) total-shares)
  )
)

;; Update pool rewards
(define-private (update-pool-rewards (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
    (current-block stacks-block-height)
    (blocks-elapsed (- current-block (get last-reward-block pool)))
  )
    (if (and (> blocks-elapsed u0) (> (get total-shares pool) u0))
      (let (
        (reward-per-share (/ (* (get reward-rate pool) blocks-elapsed) (get total-shares pool)))
        (new-accumulated (+ (get accumulated-reward-per-share pool) reward-per-share))
      )
        (map-set liquidity-pools pool-id 
          (merge pool {
            accumulated-reward-per-share: new-accumulated,
            last-reward-block: current-block
          }))
        (ok new-accumulated)
      )
      (ok (get accumulated-reward-per-share pool))
    )
  )
)

;; Calculate pending rewards for a user
(define-private (calculate-pending-rewards (user principal) (pool-id uint))
  (match (map-get? user-positions {user: user, pool-id: pool-id})
    position 
      (match (map-get? liquidity-pools pool-id)
        pool
          (let (
            (accumulated-per-share (get accumulated-reward-per-share pool))
            (user-shares (get shares position))
            (reward-debt (get reward-debt position))
            (pending (- (* user-shares accumulated-per-share) reward-debt))
          )
            (/ pending PRECISION)
          )
        u0
      )
    u0
  )
)

;; Read-only Functions

;; Get pool information
(define-read-only (get-pool-info (pool-id uint))
  (map-get? liquidity-pools pool-id)
)

;; Get user position
(define-read-only (get-user-position (user principal) (pool-id uint))
  (map-get? user-positions {user: user, pool-id: pool-id})
)

;; Get user's pool list
(define-read-only (get-user-pools (user principal))
  (default-to (list) (map-get? user-pool-list user))
)

;; Get pool statistics
(define-read-only (get-pool-stats (pool-id uint))
  (map-get? pool-stats pool-id)
)

;; Calculate user's share percentage
(define-read-only (calculate-share-percentage (user principal) (pool-id uint))
  (match (get-user-position user pool-id)
    position 
      (match (get-pool-info pool-id)
        pool
          (if (> (get total-shares pool) u0)
            (/ (* (get shares position) u100 PRECISION) (get total-shares pool))
            u0
          )
        u0
      )
    u0
  )
)

;; Get pending rewards
(define-read-only (get-pending-rewards (user principal) (pool-id uint))
  (calculate-pending-rewards user pool-id)
)

;; Get pool APR
(define-read-only (get-pool-apr (pool-id uint))
  (match (get-pool-stats pool-id)
    stats (get apr stats)
    u0
  )
)

;; Public Functions

;; Create a new liquidity pool
(define-public (create-pool 
    (name (string-ascii 50))
    (reward-rate uint)
    (lock-period uint)
    (early-withdrawal-penalty uint))
  (let (
    (pool-id (var-get next-pool-id))
    (current-block stacks-block-height)
  )
    (begin
      ;; Only contract owner can create pools
      (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR-NOT-AUTHORIZED))
      
      ;; Validate inputs
      (asserts! (> (len name) u0) (err ERR-INVALID-AMOUNT))
      (asserts! (<= early-withdrawal-penalty u5000) (err ERR-INVALID-AMOUNT)) ;; Max 50% penalty
      
      ;; Create pool
      (map-set liquidity-pools pool-id {
        name: name,
        total-liquidity: u0,
        total-shares: u0,
        accumulated-fees: u0,
        reward-rate: reward-rate,
        last-reward-block: current-block,
        accumulated-reward-per-share: u0,
        is-active: true,
        created-at: current-block,
        lock-period: lock-period,
        early-withdrawal-penalty: early-withdrawal-penalty
      })
      
      ;; Initialize pool stats
      (map-set pool-stats pool-id {
        total-deposits: u0,
        total-withdrawals: u0,
        total-rewards-distributed: u0,
        unique-depositors: u0,
        apr: u0,
        volume-24h: u0
      })
      
      ;; Initialize pool fees
      (map-set pool-fees pool-id {
        total-fees-collected: u0,
        fees-per-share: u0,
        last-fee-distribution: current-block
      })
      
      ;; Update counter
      (var-set next-pool-id (+ pool-id u1))
      
      (ok pool-id)
    )
  )
)

;; Deposit STX into liquidity pool
(define-public (deposit (pool-id uint) (amount uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
    (current-block stacks-block-height)
    (user-pools (default-to (list) (map-get? user-pool-list tx-sender)))
    (existing-position (map-get? user-positions {user: tx-sender, pool-id: pool-id}))
    (shares-to-mint (calculate-shares amount (get total-liquidity pool) (get total-shares pool)))
  )
    (begin
      ;; Validate inputs
      (asserts! (get is-active pool) (err ERR-POOL-PAUSED))
      (asserts! (>= amount MIN_DEPOSIT) (err ERR-MINIMUM-DEPOSIT))
      
      ;; Update pool rewards before deposit
      (unwrap! (update-pool-rewards pool-id) (err ERR-REWARD-CALCULATION-FAILED))
      
      ;; Transfer STX to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update or create user position
      (match existing-position
        position 
          ;; Update existing position
          (map-set user-positions {user: tx-sender, pool-id: pool-id}
            (merge position {
              shares: (+ (get shares position) shares-to-mint),
              deposit-amount: (+ (get deposit-amount position) amount),
              reward-debt: (* (+ (get shares position) shares-to-mint) 
                             (get accumulated-reward-per-share pool))
            }))
        ;; Create new position
        (begin
          (map-set user-positions {user: tx-sender, pool-id: pool-id} {
            shares: shares-to-mint,
            deposit-amount: amount,
            deposited-at: current-block,
            last-reward-claim: current-block,
            reward-debt: (* shares-to-mint (get accumulated-reward-per-share pool)),
            total-rewards-earned: u0
          })
          
          ;; Add pool to user's list if not already there
          (if (is-none (index-of user-pools pool-id))
            (map-set user-pool-list tx-sender 
              (unwrap! (as-max-len? (append user-pools pool-id) u20) (err ERR-INVALID-AMOUNT)))
            true
          )
        )
      )
      
      ;; Update pool state
      (map-set liquidity-pools pool-id 
        (merge pool {
          total-liquidity: (+ (get total-liquidity pool) amount),
          total-shares: (+ (get total-shares pool) shares-to-mint)
        }))
      
      ;; Update pool statistics
      (update-pool-stats-deposit pool-id amount)
      
      (ok shares-to-mint)
    )
  )
)

;; Withdraw from liquidity pool
(define-public (withdraw (pool-id uint) (shares-to-burn uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
    (position (unwrap! (map-get? user-positions {user: tx-sender, pool-id: pool-id}) (err ERR-INSUFFICIENT-BALANCE)))
    (current-block stacks-block-height)
    (withdrawal-amount (calculate-withdrawal-amount shares-to-burn (get total-liquidity pool) (get total-shares pool)))
    (time-locked (+ (get deposited-at position) (get lock-period pool)))
    (is-early-withdrawal (< current-block time-locked))
  )
    (begin
      ;; Validate withdrawal
      (asserts! (> shares-to-burn u0) (err ERR-INVALID-AMOUNT))
      (asserts! (<= shares-to-burn (get shares position)) (err ERR-WITHDRAWAL-TOO-LARGE))
      (asserts! (> withdrawal-amount u0) (err ERR-NO-LIQUIDITY))
      
      ;; Update pool rewards before withdrawal
      (unwrap! (update-pool-rewards pool-id) (err ERR-REWARD-CALCULATION-FAILED))
      
      ;; Calculate penalty if early withdrawal
      (let (
        (penalty-amount (if is-early-withdrawal
                         (/ (* withdrawal-amount (get early-withdrawal-penalty pool)) FEE_DENOMINATOR)
                         u0))
        (final-amount (- withdrawal-amount penalty-amount))
      )
        
        ;; Claim pending rewards
        (try! (claim-rewards pool-id))
        
        ;; Update user position
        (let ((remaining-shares (- (get shares position) shares-to-burn)))
          (if (is-eq remaining-shares u0)
            ;; Remove position entirely
            (map-delete user-positions {user: tx-sender, pool-id: pool-id})
            ;; Update position
            (map-set user-positions {user: tx-sender, pool-id: pool-id}
              (merge position {
                shares: remaining-shares,
                reward-debt: (* remaining-shares (get accumulated-reward-per-share pool))
              }))
          )
        )
        
        ;; Update pool state
        (map-set liquidity-pools pool-id 
          (merge pool {
            total-liquidity: (- (get total-liquidity pool) withdrawal-amount),
            total-shares: (- (get total-shares pool) shares-to-burn),
            accumulated-fees: (+ (get accumulated-fees pool) penalty-amount)
          }))
        
        ;; Transfer STX back to user
        (try! (as-contract (stx-transfer? final-amount tx-sender tx-sender)))
        
        ;; Update pool statistics
        (update-pool-stats-withdrawal pool-id withdrawal-amount)
        
        (ok {amount: final-amount, penalty: penalty-amount})
      )
    )
  )
)

;; Claim yield farming rewards
(define-public (claim-rewards (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
    (position (unwrap! (map-get? user-positions {user: tx-sender, pool-id: pool-id}) (err ERR-INSUFFICIENT_BALANCE)))
    (current-block stacks-block-height)
  )
    (begin
      ;; Update pool rewards
      (unwrap! (update-pool-rewards pool-id) (err ERR-REWARD-CALCULATION-FAILED))
      
      ;; Calculate pending rewards
      (let (
        (updated-pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
        (pending-rewards (calculate-pending-rewards tx-sender pool-id))
      )
        (if (> pending-rewards u0)
          (begin
            ;; Update user position
            (map-set user-positions {user: tx-sender, pool-id: pool-id}
              (merge position {
                last-reward-claim: current-block,
                reward-debt: (* (get shares position) (get accumulated-reward-per-share updated-pool)),
                total-rewards-earned: (+ (get total-rewards-earned position) pending-rewards)
              }))
            
            ;; Transfer rewards to user
            (try! (as-contract (stx-transfer? pending-rewards tx-sender tx-sender)))
            
            (ok pending-rewards)
          )
          (ok u0)
        )
      )
    )
  )
)

;; Add platform fees to pool
(define-public (add-platform-fees (pool-id uint) (fee-amount uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
  )
    (begin
      ;; Only contract can add fees (from platform operations)
      (asserts! (is-eq tx-sender (as-contract tx-sender)) (err ERR-NOT-AUTHORIZED))
      
      ;; Add fees to pool
      (map-set liquidity-pools pool-id 
        (merge pool {
          accumulated-fees: (+ (get accumulated-fees pool) fee-amount)
        }))
      
      ;; Update total platform fees
      (var-set total-platform-fees (+ (var-get total-platform-fees) fee-amount))
      
      (ok true)
    )
  )
)

;; Distribute accumulated fees to liquidity providers
(define-public (distribute-fees (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
    (fees-to-distribute (get accumulated-fees pool))
  )
    (begin
      ;; Only contract owner can trigger distribution
      (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR-NOT-AUTHORIZED))
      (asserts! (> fees-to-distribute u0) (err ERR-INVALID-AMOUNT))
      (asserts! (> (get total-shares pool) u0) (err ERR-NO-LIQUIDITY))
      
      ;; Calculate fees per share
      (let (
        (fees-per-share (/ (* fees-to-distribute PRECISION) (get total-shares pool)))
        (pool-fee-info (default-to {total-fees-collected: u0, fees-per-share: u0, last-fee-distribution: u0} 
                                  (map-get? pool-fees pool-id)))
      )
        ;; Update pool fees tracking
        (map-set pool-fees pool-id {
          total-fees-collected: (+ (get total-fees-collected pool-fee-info) fees-to-distribute),
          fees-per-share: (+ (get fees-per-share pool-fee-info) fees-per-share),
          last-fee-distribution: stacks-block-height
        })
        
        ;; Reset accumulated fees in pool
        (map-set liquidity-pools pool-id 
          (merge pool {accumulated-fees: u0}))
        
        (ok fees-per-share)
      )
    )
  )
)

;; Helper function to update pool statistics on deposit
(define-private (update-pool-stats-deposit (pool-id uint) (amount uint))
  (match (map-get? pool-stats pool-id)
    stats 
      (map-set pool-stats pool-id 
        (merge stats {
          total-deposits: (+ (get total-deposits stats) amount),
          volume-24h: (+ (get volume-24h stats) amount)
        }))
    false
  )
)

;; Helper function to update pool statistics on withdrawal
(define-private (update-pool-stats-withdrawal (pool-id uint) (amount uint))
  (match (map-get? pool-stats pool-id)
    stats 
      (map-set pool-stats pool-id 
        (merge stats {
          total-withdrawals: (+ (get total-withdrawals stats) amount),
          volume-24h: (+ (get volume-24h stats) amount)
        }))
    false
  )
)

;; Admin Functions

;; Update pool reward rate
(define-public (update-reward-rate (pool-id uint) (new-rate uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
  )
    (begin
      ;; Only contract owner can update rates
      (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR-NOT-AUTHORIZED))
      
      ;; Update pool rewards before changing rate
      (unwrap! (update-pool-rewards pool-id) (err ERR-REWARD-CALCULATION-FAILED))
      
      ;; Update reward rate
      (map-set liquidity-pools pool-id 
        (merge pool {reward-rate: new-rate}))
      
      (ok true)
    )
  )
)

;; Pause/unpause pool
(define-public (set-pool-status (pool-id uint) (active bool))
  (let (
    (pool (unwrap! (map-get? liquidity-pools pool-id) (err ERR-POOL-NOT-FOUND)))
  )
    (begin
      ;; Only contract owner can change status
      (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR-NOT-AUTHORIZED))
      
      (map-set liquidity-pools pool-id 
        (merge pool {is-active: active}))
      
      (ok true)
    )
  )
)

;; Emergency withdrawal (contract owner only)
(define-public (emergency-withdraw (amount uint))
  (begin
    ;; Only contract owner can emergency withdraw
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR-NOT-AUTHORIZED))
    
    ;; Transfer STX to owner
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    
    (ok true)
  )
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-pools: (- (var-get next-pool-id) u1),
    total-platform-fees: (var-get total-platform-fees),
    platform-fee-rate: (var-get platform-fee-rate),
    contract-balance: (stx-get-balance (as-contract tx-sender))
  }
)