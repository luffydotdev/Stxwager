;; Staking Interface Contract
;; Provides helper functions and integration points

;; Constants
(define-constant CONTRACT-OWNER tx-sender)

;; Helper functions for frontend integration
(define-read-only (get-staker-info (staker principal))
  (let
    (
      (stake-data (contract-call? .staking get-stake staker))
      (pending-rewards (contract-call? .staking calculate-rewards staker))
    )
    {
      stake: stake-data,
      pending-rewards: pending-rewards,
      total-staked: (contract-call? .staking get-total-staked)
    }
  )
)

(define-read-only (get-staking-stats)
  {
    total-staked: (contract-call? .staking get-total-staked),
    platform-fee-rate: (contract-call? .staking get-platform-fee-rate),
    current-block: stacks-block-height
  }
)

;; Batch operations
(define-public (stake-and-bet (stake-amount uint) (bet-amount uint) (bet-data (buff 256)))
  (begin
    ;; First stake tokens
    (try! (contract-call? .staking stake-tokens stake-amount))
    
    ;; Then place bet (this would integrate with your betting contract)
    ;; (try! (contract-call? .betting place-bet bet-amount bet-data))
    
    (ok true)
  )
)

(define-public (claim-all-rewards (stakers (list 50 principal)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err u100))
    (ok (map claim-single-reward stakers))
  )
)

(define-private (claim-single-reward (staker principal))
  (match (as-contract (contract-call? .staking claim-rewards))
    success true
    error false
  )
)

;; Additional helper functions
(define-read-only (get-user-dashboard (user principal))
  (let
    (
      (stake-info (contract-call? .staking get-stake user))
      (rewards (contract-call? .staking calculate-rewards user))
    )
    {
      user: user,
      stake-info: stake-info,
      pending-rewards: rewards,
      current-block: stacks-block-height
    }
  )
)

(define-read-only (get-stake-tier-info (amount uint))
  {
    tier: (contract-call? .staking get-stake-tier amount),
    amount: amount
  }
)
