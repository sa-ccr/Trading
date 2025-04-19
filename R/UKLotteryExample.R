#' Displays how the functionality related to the UK Lottery analysis can be utilized 
#' @title  UK Lottery analysis example
#' @return The final results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' # This software is covered by GPL license and provided strictly for educational
#' # reasons (no actual investment/betting decisions should be taken based on this)
#' 
#' final_results = UKLotteryExample()

UKLotteryExample <- function()  {
  
  uklottery_results = UKLotteryResults()
  
  user_input = c(5,10,20,30,40,50)
  
  backtested_results = UKLotteryBacktesting(uklottery_results,, user_input)
  
  payout_results = CalcUKLotteryPnL(backtested_results, plot_results = FALSE)
  
  return(payout_results)
}
