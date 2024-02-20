#' Returns the top 5 most or least lucky euromillion numbers
#' @title  Top 5 most or least lucky numbers for EuroMillions/EuroJackpot
#' @param eurolottery_results The full list of EuroMillions/EuroJackpot results
#' @param date_since The date after which the analysis is to be performed, i.e. 2022-12-22 
#' @param least_lucky If TRUE, the least lucky numbers will be returned (default FALSE)
#' @return Top 5 numbers
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' euromillions_results = EuroMillionsResults()
#' top_5 = top5(euromillions_results, '2022-12-22', least_lucky=TRUE)
#' 
top5 <- function(eurolottery_results, date_since, least_lucky=FALSE)  
{
  if(!missing(date_since))
  {    eurolottery_results = eurolottery_results[dates_parsed>date_since]}
  if(least_lucky)
  {    return(as.numeric(names(sort(table(unlist(eurolottery_results[,2:6]))))[1:5]))
  }else
  {    return(as.numeric(names(sort(table(unlist(eurolottery_results[,2:6]))))[46:50]))}
}