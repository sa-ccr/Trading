#' Displays how the functionality related to the euromillions analysis can be utilized 
#' @title  Euromillions analysis example
#' @return The final results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' # This software is covered by GPL license and provided strictly for educational
#' # reasons (no actual investment/betting decisions should be taken based on this)
#' 
#' final_results = EuroMillionsExample()

EuroMillionsExample <- function()  {
  
  euromillions_results = EuroMillionsResults()
  
  user_input = top5(euromillions_results, date_since='2005-01-01', least_lucky=TRUE)
  
  backtested_results = EuroLotteryBacktesting(euromillions_results, date_since='2005-01-01', user_input)
  
  payout_results = CalcEuroLotteryPnL(backtested_results, plot_results = TRUE)
  
  # returns all the 139,838,160 possible combinations, can create memory issues.
  #all_combinations = EuroLotteryAllCombinations()
  
  return(payout_results)
}
