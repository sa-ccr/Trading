#' Displays how the functionality related to the Set For Life analysis can be utilized 
#' @title  Set For Life analysis example
#' @return The final results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' # This software is covered by GPL license and provided strictly for educational
#' # reasons (no actual investment/betting decisions should be taken based on this)
#' 
#' final_results = SetForLifeExample()

SetForLifeExample <- function()  {
  
  setforlife_results = SetForLifeResults()
  
  user_input = c(10,20,30,40,50,5)
  
  backtested_results = SetForLifeBacktesting(setforlife_results, date_since='2005-01-01', user_input)
  
  payout_results = CalcSetForLifePnL(backtested_results, plot_results = FALSE)
  
  return(payout_results)
}
