#' @description Calculates the potential profit or loss when someone is betting on a specific number in the roulette and keeps doubling every eighteen 
#' spins if the number hasn't appeared yet.
#'  
#' @title Roulette P&L betting on a specific number
#' @param bet_minimum The minimum betting amount that the casino allows
#' @param bet_maximum The maximum betting amount that the casino allows
#' @param initial_capital The initial capital to be used
#' @param targeted_number The specific number that we expect to be drawn (statistically speaking, this should have zero effect on the results)
#' @param simulations_num The number of simulations to be run
#' @param trials_per_sim  The number of trials in each simulation
#' @param stop_loss   (Optional) The number of spins after which the betting amount will go back to the minimum if the targeted number hasn't appeared.
#' @return A list containing the minimum, the maximum and the final balance for each simulation. Also the P&L graph for the last simulation will be plotted.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://en.wikipedia.org/wiki/Gambler%27s_fallacy
#' 
#' 
#' @examples
#' 
#' # This software is covered by GPL license and provided strictly for educational reasons (no actual investment decisions should be taken based on this)
#' pl_results = roulette_pl_calculator_specific_number(bet_minimum =0.1 , bet_maximum = 3276.8, initial_capital = 20000, targeted_number = 0, simulations_num = 1000, trials_per_sim = 10000, stop_loss = 180)
#' summary(pl_results$min_capital)
#' summary(pl_results$max_capital)
#' summary(pl_results$final_capital)
#' 
roulette_pl_calculator_specific_number <- function(bet_minimum, bet_maximum, initial_capital, targeted_number, simulations_num, trials_per_sim, stop_loss)  {
  
  full_sequence = floor(runif(trials_per_sim*simulations_num,0,38))
  
  current_capital_vec = rep(0,trials_per_sim-1)
  final_capital = rep(0,simulations_num-1)
  min_capital   = rep(0,simulations_num-1)
  max_capital   = rep(0,simulations_num-1)
  loss_counter_list = list()
  
  for(j in 1:simulations_num)
  {
    bet_amount = bet_minimum
    current_capital = initial_capital
    x = full_sequence[((j-1)*trials_per_sim+1):(j*trials_per_sim)]
    cat(j)
    cat('\n')
    
    loss_counter_vec = numeric(0)
    loss_counter = 0
    for(i in 1:(length(x)))
    {
      if(x[i]!=0)
      {
        current_capital = current_capital-bet_amount
        loss_counter = loss_counter+1
        
        if(loss_counter%%18==0)
        {        bet_amount = bet_amount*2   }
        if(!missing(stop_loss)){
          if(loss_counter>stop_loss)
          {        bet_amount=bet_minimum      }
        }
      }else
      {
        current_capital = current_capital + 35*bet_amount
        bet_amount=bet_minimum
        loss_counter_vec = c(loss_counter_vec, loss_counter)
        loss_counter = 0
      }
      current_capital_vec[i] = current_capital
      if(current_capital<0) break()
    }
    loss_counter_list[[j]] = loss_counter_vec
    final_capital[j] = current_capital
    min_capital[j]   = min(current_capital_vec)
    max_capital[j]   = max(current_capital_vec)
  }
  
  ff=data.frame(cbind(x,current_capital_vec))
  plot(seq(1,trials_per_sim),current_capital_vec)
  return(list(max_capital = max_capital, min_capital = min_capital, final_capital = final_capital))
}
