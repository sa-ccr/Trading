#' Returns all the SetForLifeResults results since the first draw on Feb 2004 until the end of 2023
#' @title  Returns all the EuroJackpot results until the end of 2023
#' @return A dataframe with all the EuroJackpot results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' eurojackpot_results = EuroJackpotResults()

SetForLifeResults <- function()  {
  
  really_final2 <- data.table(read.csv(system.file("extdata", "set_for_life_history.csv", package = "Trading")))
  dats = really_final2$Date
  really_final2$dates_parsed = as.Date(dats," %d/%m/%Y")
  
  return(really_final2)
}