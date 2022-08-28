#' @description Calculates the potential profit or loss when someone is betting in the roulette based on the Labouchere Betting System.
#'  
#' @title Roulette P&L betting based on the Labouchere Betting System
#' @param bet_minimum The minimum betting amount that the casino allows
#' @param bet_maximum The maximum betting amount that the casino allows
#' @param initial_capital The initial capital to be used
#' @param profit_target The profit amount to be earned
#' @param profit_sequence (Optional) the amounts of the bets to reach this profit amount. If omitted, the minimum betting amount will be used
#' @param simulations_num The number of simulations to be run
#' @param trials_per_sim  The number of trials in each simulation
#' @return A list containing the minimum, the maximum and the final balance for each simulation. Also the P&L graph for the last simulation will be plotted.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://en.wikipedia.org/wiki/Roulette#Betting_strategies_and_tactics
#' 
#' @examples
#' 
#' # This software is covered by GPL license and provided strictly for educational
#' # reasons (no actual investment/betting decisions should be taken based on this)
#' # On top of these, the below example contains a tiny number of simulations and
#' # trials just to pass CRAN tests - the user would have to highly increase both
#' # variables when running these.
#' pl_results = roulette_pl_calculator_labouchere(bet_minimum = 0.1 , bet_maximum = 3276.8,
#' initial_capital = 20000, profit_target = 100, profit_sequence = rep(10,10),
#'  simulations_num = 100, trials_per_sim = 100)
#' summary(pl_results$min_capital)
#' summary(pl_results$max_capital)
#' summary(pl_results$final_capital)
#' 

roulette_pl_calculator_labouchere <- function(bet_minimum, bet_maximum, initial_capital, profit_target, profit_sequence, simulations_num, trials_per_sim)  {
  
  full_sequence = floor(runif(trials_per_sim*simulations_num,0,38))
  current_capital_vec = c()
  final_capital = rep(0,simulations_num-1)
  min_capital   = rep(0,simulations_num-1)
  max_capital   = rep(0,simulations_num-1)
  
  if(missing(profit_sequence)) profit_sequence = rep(bet_minimum,profit_target/bet_minimum)
  
  if(sum(profit_sequence)!=profit_target) stop('The sum of the elements of the profit sequence has to equal the profit target')
  
  for(j in 1:simulations_num)
  {
    current_capital_vec = c()
    temp_profit_sequence = profit_sequence
    bet_amount = temp_profit_sequence[1] + temp_profit_sequence[length(temp_profit_sequence)]
    current_capital = initial_capital
    x = full_sequence[((j-1)*trials_per_sim+1):(j*trials_per_sim)]
    if(j%%500==0)
    {      cat(paste('Simulation Number:',j))
           cat('\n')    }
    
    for(i in 1:(length(x)-1))
    {
      if(x[i+1]==0)
      {
        current_capital = current_capital - bet_amount
        temp_profit_sequence = c(temp_profit_sequence, bet_amount)
        current_capital_vec[i] = current_capital
        if(temp_profit_sequence[1]+ temp_profit_sequence[length(temp_profit_sequence)]>bet_maximum)
        {          bet_amount = bet_maximum
        }else
        {          bet_amount = temp_profit_sequence[1] + temp_profit_sequence[length(temp_profit_sequence)]        }
        
        next()
      } 
      
      if((x[i]%%2==x[i+1]%%2))
      {       current_capital = current_capital + bet_amount
              if(length(temp_profit_sequence) %in% c(1,2)){
                current_capital_vec[i] = current_capital
                break()
              } 
              temp_profit_sequence = temp_profit_sequence[2:(length(temp_profit_sequence)-1)]
              bet_amount = ifelse(length(temp_profit_sequence)==1,temp_profit_sequence[1],temp_profit_sequence[1] + temp_profit_sequence[length(temp_profit_sequence)])
      }else if(x[i]%%2!=x[i+1]%%2)
      {       current_capital = current_capital - bet_amount
              temp_profit_sequence = c(temp_profit_sequence, bet_amount)
      if(temp_profit_sequence[1]+ temp_profit_sequence[length(temp_profit_sequence)]>bet_maximum)
      {          bet_amount = bet_maximum
      }else
      {          bet_amount = temp_profit_sequence[1]+ temp_profit_sequence[length(temp_profit_sequence)]        }
      }else stop('You shouldnt be here')
      
      current_capital_vec[i] = current_capital
      
      if(current_capital<0) break()
    }
    final_capital[j] = current_capital
    
    if(current_capital>20100)
    {
      fds=3
    }
    min_capital[j]   = min(current_capital_vec)
    max_capital[j]   = max(current_capital_vec)
  }
  
  
  plot(seq(1,length(current_capital_vec)),current_capital_vec)
  return(list(max_capital = max_capital, min_capital = min_capital, final_capital = final_capital))
}
