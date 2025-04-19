#' Returns all the UK ThunderBall results until the beginning of 2025
#' @title  Returns all the UK ThunderBall results until the beginning of 2025
#' @return A dataframe with all the UK ThunderBall results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' UKThunderBall_Results = UKThunderBallResults()

UKThunderBallResults <- function()  {
  
  really_final2 <- data.table(read.csv(system.file("extdata", "UK_ThunderBall_history.csv", package = "Trading")))
  really_final2$dates_parsed = as.Date(really_final2$Date," %m/%d/%Y")
  
  return(really_final2)
}