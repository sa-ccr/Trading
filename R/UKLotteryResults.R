#' Returns all the UKLottery results since the first draw on Nov 1994 until the beginning of 2025
#' @title  Returns all the UKLottery results until the beginning of 2025
#' @return A dataframe with all the UKLottery results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' UKLotto_results = UKLotteryResults()

UKLotteryResults <- function()  {
  
  really_final2 <- data.table(read.csv(system.file("extdata", "UK_Lottery_history.csv", package = "Trading")))
  dats = really_final2$Date
  really_final2$dates_parsed = as.Date(dats," %d/%m/%Y")
  
  return(really_final2)
}