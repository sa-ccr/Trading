#' Displays how the functionality related to the UK ThunderBall analysis can be utilized 
#' @title  UK ThunderBall analysis example
#' @return The final results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' # This software is covered by GPL license and provided strictly for educational
#' # reasons (no actual investment/betting decisions should be taken based on this)
#' 
#' final_results = EuroJackpotExample()

UKThunderballExample <- function()  {
  
  ukthunderball_results = UKThunderBallResults()
  
  user_input = c(10,20,30,31,32,5)
  
  backtested_results = UKThunderballBacktesting(ukthunderball_results, user_input = user_input)
  
  payout_results = CalcEuroLotteryPnL(backtested_results, plot_results = TRUE)

  return(payout_results)
}
