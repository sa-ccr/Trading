#' @description Calculates the potential profit or loss when someone is betting in the roulette based on the martingale system while trying to reduce
#' the risk by 1. Starting to double after the first loss 2. Not doubling if the second number is zero.
#'  
#' @title Roulette P&L betting based on a modified martingale strategy
#' @param bet_minimum The minimum betting amount that the casino allows
#' @param bet_maximum The maximum betting amount that the casino allows
#' @param initial_capital The initial capital to be used
#' @param simulations_num The number of simulations to be run
#' @param trials_per_sim  The number of trials in each simulation
#' @return A list containing the minimum, the maximum and the final balance for each simulation. Also the P&L graph for the last simulation will be plotted.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://en.wikipedia.org/wiki/Gambler%27s_fallacy
#' 
#' @examples
#' 
#' # This software is covered by GPL license and provided strictly for educational reasons (no actual investment/betting decisions should be taken based on this)
#' pl_results = roulette_pl_calculator_fibonacci(bet_minimum = 0.1 , bet_maximum = 6000, initial_capital = 20000, simulations_num = 1000, trials_per_sim = 10000)
#' summary(pl_results$min_capital)
#' summary(pl_results$max_capital)
#' summary(pl_results$final_capital)
#' 
roulette_pl_calculator_fibonacci <- function(bet_minimum, bet_maximum, initial_capital, simulations_num, trials_per_sim, stop_loss)  {
  
  full_sequence = floor(runif(trials_per_sim*simulations_num,0,38))
  
  fibonacci_seq = capped_fibonacci_seq(max_number = bet_maximum/bet_minimum)
  fibonacci_seq_length = length(fibonacci_seq)
  current_capital_vec = rep(0,trials_per_sim-1)
  final_capital = rep(0,simulations_num-1)
  min_capital   = rep(0,simulations_num-1)
  max_capital   = rep(0,simulations_num-1)
  
  for(j in 1:simulations_num)
  {
    bet_amount = bet_minimum
    current_capital = initial_capital
    x = full_sequence[((j-1)*trials_per_sim+1):(j*trials_per_sim)]
    fibonacci_counter = 2
    cat(j)
    cat('\n')
    
    for(i in 1:(length(x)-1))
    {
      if(x[i+1]==0)
      {
        current_capital = current_capital - fibonacci_seq[fibonacci_counter]*bet_amount
        current_capital_vec[i] = current_capital
        if(fibonacci_counter+1>fibonacci_seq_length)
        {          fibonacci_counter = 2
        }else
        {          fibonacci_counter = fibonacci_counter + 1        }
        
        next()
      } 
      
      if((x[i]%%2==x[i+1]%%2))
      {       current_capital   = current_capital + fibonacci_seq[fibonacci_counter]*bet_amount
              fibonacci_counter = max(fibonacci_counter-2,2)
      }else if(x[i]%%2!=x[i+1]%%2)
      {       current_capital = current_capital - fibonacci_seq[fibonacci_counter]*bet_amount
              if(fibonacci_counter+1>fibonacci_seq_length)
              {          fibonacci_counter = 2
              }else
              {          fibonacci_counter = fibonacci_counter + 1        }
      }else stop('You shouldnt be here')
      
      current_capital_vec[i] = current_capital
      
      if(current_capital<0) break()
    }
    final_capital[j] = current_capital
    min_capital[j]   = min(current_capital_vec)
    max_capital[j]   = max(current_capital_vec)
  }

  
  plot(seq(1,trials_per_sim-1),current_capital_vec)
  return(list(max_capital = max_capital, min_capital = min_capital, final_capital = final_capital))
}
