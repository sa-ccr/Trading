#' Returns all the EuroJackpot results since the first draw on Feb 2004 until the end of 2023
#' @title  Returns all the EuroJackpot results until the end of 2023
#' @return A dataframe with all the EuroJackpot results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' eurojackpot_results = EuroJackpotResults()

EuroJackpotResults <- function()  {
  
  really_final2 <- data.table(read.csv(system.file("extdata", "eurojackpot_results.csv", package = "Trading")))
  
  dats = really_final2$Dates
  
  dats = sub("Friday","",really_final2$Dates)
  dats = sub("Tuesday","",really_final2$Dates)
  dats = sub("Tuesday","",dats)
  dats = sub("Friday","",really_final2$Dates)
  dats = sub("Tuesday","",dats)
  dats
  dats = sub("rd","",dats)
  dats = sub("st","",dats)
  dats = sub("th","",dats)
  dats = sub("nd","",dats)
  dats = sub("Augu ","August ",dats)
  really_final2$dates_parsed = as.Date(dats," %d %B %Y")
  
  return(really_final2)
  
}