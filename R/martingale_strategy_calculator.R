#' @description Calculates the number of repetitions needed for a specific number of consequtive failed trades/bet to appear. 
#' This can apply to roulette betting but also trading algorithms which use the same logic on doubling down after a failed trade.
#'  
#' @title Martingale Strategy Repetitions
#' @param length_of_targetted_sequence The number of consecutive failed trades/bets that we try to calculate the expected number of repetitions for 
#' @param prob_of_success The probability of a sucessful trade/bet
#' @param simulations_num The number of simulations to be run
#' @param trials_per_sim  The number of trials in each simulation
#' @param quantile_perc   (Optional) When set, the number of repetitions expected with such probability is returned.
#' @return A list containing the number of repetitions needed to reach the targetted sequence for the first time in each simulation 
#' (will be zero if the sequence is not found) and, when the quantile_perc is set, the above number of repetitions.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://en.wikipedia.org/wiki/Gambler%27s_fallacy
#' summary(num_of_trials_needed)
martingale_strategy_repetitions <- function(length_of_targetted_sequence, prob_of_success = 18/37, simulations_num, trials_per_sim, quantile_perc)  {

  full_sequence =runif(trials_per_sim*simulations_num,0,1)
  full_sequence = full_sequence <=prob_of_success
  
  num_of_trials_needed = rep(0,simulations_num)
  
  for(j in 1:simulations_num)
  {
    x = full_sequence[((j-1)*trials_per_sim+1):(j*trials_per_sim)]
    cat(j)
    cat('\n')

    max_seq = 1
    max_seqs = 1
    
    for(i in 1:(length(x)-1))
    {
      if(x[i]==x[i+1])
      {
        max_seq = max_seq+1
        if(max_seq==targetted_sequence)
        {
          num_of_trials_needed[j] = i+1
          break()
        }
      }else
      {
        max_seqs = c(max_seqs,max_seq)
        max_seq=1
      }
    }
  }

  num_of_trials_needed_no_zeros = num_of_trials_needed
  num_of_trials_needed_no_zeros[num_of_trials_needed_no_zeros==0]=trials_per_sim
  summary(num_of_trials_needed)
  
  relevant_quantile = quantile(num_of_trials_needed_no_zeros,quantile_perc)
  
  return(list(num_of_trials_needed= num_of_trials_needed,))
}
