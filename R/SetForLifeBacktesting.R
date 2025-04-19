#' Backtests the numbers the user has selected against the full (or the specified) history of Set For Life results 
#' @title  Set For Life Backtesting
#' @param setforlife_results The full list of Set For Life results
#' @param date_since The date after which the analysis is to be performed, i.e. 2022-12-22 
#' @param user_input The seven numbers the user has selected
#' @return The backtested results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' setforlife_results = SetForLifeResults()
#' user_input = c(10,20,30,40,50,5,10)
#' backtested_results = SetForLifeBacktesting(setforlife_results, '2005-01-01', user_input)

SetForLifeBacktesting <- function(setforlife_results, date_since, user_input)  {
  
  if(!missing(date_since))
  {    setforlife_results = setforlife_results[dates_parsed>date_since]}
  matched_numbers = list()
  setforlife_results$first_five = ""
  setforlife_results$life_ball      = ""
  setforlife_results$winning_scale = 0
  
  for(i in 1:nrow(setforlife_results))
  {
    first_five = sum(user_input[1:5]%in% setforlife_results[i,2:6])
    life_ball   = sum(user_input[6]%in% setforlife_results[i,7])
    setforlife_results$first_five[i] = ifelse(length(which(setforlife_results[i,2:6]%in%user_input[1:5]) )==0,"",paste0(which(setforlife_results[i,2:6]%in%user_input[1:5]), collapse=","))
    setforlife_results$life_ball[i]   = ifelse(length(which(setforlife_results[i,7] %in%user_input[6:7] ))==0,"",paste0(which(setforlife_results[i,7] %in%user_input[6] ), collapse=","))
    payout_results = rep(0,13)
    
    if(first_five==2&life_ball==0)
    {
      payout_results[1] = payout_results[1] + 1
      setforlife_results$winning_scale[i] = 1
    }else if(first_five==2&life_ball==1)
    {
      payout_results[2] = payout_results[2] + 1
      setforlife_results$winning_scale[i] = 2
    }else if(first_five==3&life_ball==0)
    {
      payout_results[4] = payout_results[3] + 1
      setforlife_results$winning_scale[i] = 3
    }else if(first_five==3&life_ball==1)
    {
      payout_results[5] = payout_results[4] + 1
      setforlife_results$winning_scale[i] = 4
    }else if(first_five==4&life_ball==0)
    {
      payout_results[7] = payout_results[5] + 1
      setforlife_results$winning_scale[i] = 5
    }else if(first_five==4&life_ball==1)
    {
      payout_results[9] = payout_results[6] + 1
      setforlife_results$winning_scale[i] = 6
    }else if(first_five==5&life_ball==0)
    {
      payout_results[11] = payout_results[7] + 1
      setforlife_results$winning_scale[i] = 7
    }else if(first_five==5&life_ball==1)
    {
      payout_results[12] = payout_results[8] + 1
      setforlife_results$winning_scale[i] = 8
    }
  }
  return(setforlife_results)
}