#' Backtests the numbers the user has selected against the full (or the specified) history of UKLottery results 
#' @title  UKLottery Backtesting
#' @param uklottery_results The full list of UKLottery results
#' @param date_since The date after which the analysis is to be performed, i.e. 2022-12-22 
#' @param user_input The seven numbers the user has selected
#' @return The backtested results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' uklottery_results = UKLotteryResults()
#' user_input = c(5,10,20,30,40,50)
#' backtested_results = UKLotteryBacktesting(uklottery_results, '2005-01-01', user_input)

UKLotteryBacktesting <- function(uklottery_results, date_since, user_input)  {
  
  if(!missing(date_since))
  {    uklottery_results = uklottery_results[dates_parsed>date_since]}
  matched_numbers = list()
  uklottery_results$first_six = ""
  uklottery_results$bonus      = ""
  uklottery_results$winning_scale = 0
  
  for(i in 1:nrow(uklottery_results))
  {
    first_six = sum(user_input[1:6]%in% uklottery_results[i,2:7])
    last_two   = sum(user_input[1:6]%in% uklottery_results[i,8])
    uklottery_results$first_six[i] = ifelse(length(which(uklottery_results[i,2:7]%in%user_input[1:6]) )==0,"",paste0(which(uklottery_results[i,2:7]%in%user_input[1:6]), collapse=","))
    uklottery_results$last_two[i]   = ifelse(length(which(uklottery_results[i,8] %in%user_input[1:6] ))==0,"",paste0(which(uklottery_results[i,8] %in%user_input[1:6] ), collapse=","))
    payout_results = rep(0,13)
    
    if(first_six==2)
    {
      payout_results[1] = payout_results[1] + 1
      uklottery_results$winning_scale[i] = 1
    }else if(first_six==3)
    {
      payout_results[2] = payout_results[2] + 1
      uklottery_results$winning_scale[i] = 2
    }else if(first_six==4)
    {
      payout_results[3] = payout_results[3] + 1
      uklottery_results$winning_scale[i] = 3
    }else if(first_six==5&last_two==0)
    {
      payout_results[4] = payout_results[4] + 1
      uklottery_results$winning_scale[i] = 4
    }else if(first_six==5&last_two==1)
    {
      payout_results[5] = payout_results[5] + 1
      uklottery_results$winning_scale[i] = 5
    }else if(first_six==6)
    {
      payout_results[6] = payout_results[6] + 1
      uklottery_results$winning_scale[i] = 6
    }
  }
  return(uklottery_results)
}