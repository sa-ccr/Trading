#' Backtests the numbers the user has selected against the full (or the specified) history of UK ThunderBall results 
#' @title  UK ThunderBall Backtesting
#' @param ukthunderball_results The full list of UK ThunderBall results
#' @param date_since The date after which the analysis is to be performed, i.e. 2022-12-22 
#' @param user_input The seven numbers the user has selected
#' @return The backtested results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' ukthunderball_results = UKThunderBallResults()
#' user_input = c(10,20,30,31,32,5)
#' backtested_results = UKThunderballBacktesting(ukthunderball_results, user_input = user_input)

UKThunderballBacktesting <- function(ukthunderball_results, date_since, user_input)  {
  
  if(!missing(date_since))
  {    ukthunderball_results = ukthunderball_results[dates_parsed>date_since]}
  matched_numbers = list()
  ukthunderball_results$first_five = ""
  ukthunderball_results$bonus      = ""
  ukthunderball_results$winning_scale = 0
  
  for(i in 1:nrow(ukthunderball_results))
  {
    first_five = sum(user_input[1:5]%in% ukthunderball_results[i,2:6])
    last_two   = sum(user_input[6]%in% ukthunderball_results[i,7])
    ukthunderball_results$first_five[i] = ifelse(length(which(ukthunderball_results[i,2:6]%in%user_input[1:5]) )==0,"",paste0(which(ukthunderball_results[i,2:6]%in%user_input[1:5]), collapse=","))
    ukthunderball_results$last_two[i]   = ifelse(length(which(ukthunderball_results[i,7] %in%user_input[6] ))==0,"",paste0(which(ukthunderball_results[i,7] %in%user_input[6] ), collapse=","))
    payout_results = rep(0,13)
    
    if(first_five==0&&last_two==1)
    {
      payout_results[1] = payout_results[1] + 1
      ukthunderball_results$winning_scale[i] = 1
    }else if(first_five==1&&last_two==1)
    {
      payout_results[2] = payout_results[2] + 1
      ukthunderball_results$winning_scale[i] = 2
    }else if(first_five==2&&last_two==1)
    {
      payout_results[3] = payout_results[3] + 1
      ukthunderball_results$winning_scale[i] = 3
    }else if(first_five==3&last_two==0)
    {
      payout_results[4] = payout_results[4] + 1
      ukthunderball_results$winning_scale[i] = 4
    }else if(first_five==3&last_two==1)
    {
      payout_results[5] = payout_results[5] + 1
      ukthunderball_results$winning_scale[i] = 5
    }else if(first_five==4&&last_two==0)
    {
      payout_results[6] = payout_results[6] + 1
      ukthunderball_results$winning_scale[i] = 6
    }else if(first_five==4&&last_two==1)
    {
      payout_results[6] = payout_results[6] + 1
      ukthunderball_results$winning_scale[i] = 7
    }else if(first_five==5&&last_two==0)
    {
      payout_results[6] = payout_results[6] + 1
      ukthunderball_results$winning_scale[i] = 8
    }else if(first_five==5&&last_two==1)
    {
      payout_results[7] = payout_results[6] + 1
      ukthunderball_results$winning_scale[i] = 9
    }
    
    
  }
  return(ukthunderball_results)
}