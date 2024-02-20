#' Backtests the numbers the user has selected against the full (or the specified) history of Euromillions/EuroJackpot results 
#' @title  Euromillions/EuroJackpot Backtesting
#' @param euroLottery_results The full list of EuroMillions/EuroJackpot results
#' @param date_since The date after which the analysis is to be performed, i.e. 2022-12-22 
#' @param user_input The seven numbers the user has selected
#' @return The backtested results
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' euromillions_results = EuroMillionsResults()
#' user_input = c(10,20,30,40,50,5,10)
#' backtested_results = EuroLotteryBacktesting(euromillions_results, '2005-01-01', user_input)

EuroLotteryBacktesting <- function(euroLottery_results, date_since, user_input)  {
  
  if(!missing(date_since))
  {    euroLottery_results = euroLottery_results[dates_parsed>date_since]}
  matched_numbers = list()
  euroLottery_results$first_five = ""
  euroLottery_results$last_two      = ""
  euroLottery_results$winning_scale = 0
  
  for(i in 1:nrow(euroLottery_results))
  {
    first_five = sum(user_input[1:5]%in% euroLottery_results[i,2:6])
    last_two   = sum(user_input[6:7]%in% euroLottery_results[i,7:8])
    euroLottery_results$first_five[i] = ifelse(length(which(euroLottery_results[i,2:6]%in%user_input[1:5]) )==0,"",paste0(which(euroLottery_results[i,2:6]%in%user_input[1:5]), collapse=","))
    euroLottery_results$last_two[i]   = ifelse(length(which(euroLottery_results[i,7:8] %in%user_input[6:7] ))==0,"",paste0(which(euroLottery_results[i,7:8] %in%user_input[6:7] ), collapse=","))
    payout_results = rep(0,13)
    
    if(first_five==2&last_two==0)
    {
      payout_results[1] = payout_results[1] + 1
      euroLottery_results$winning_scale[i] = 1
    }else if(first_five==2&last_two==1)
    {
      payout_results[2] = payout_results[2] + 1
      euroLottery_results$winning_scale[i] = 2
    }else if(first_five==1&last_two==2)
    {
      payout_results[3] = payout_results[3] + 1
      euroLottery_results$winning_scale[i] = 3
    }else if(first_five==3&last_two==0)
    {
      payout_results[4] = payout_results[4] + 1
      euroLottery_results$winning_scale[i] = 4
    }else if(first_five==3&last_two==1)
    {
      payout_results[5] = payout_results[5] + 1
      euroLottery_results$winning_scale[i] = 5
    }else if(first_five==2&last_two==2)
    {
      payout_results[6] = payout_results[6] + 1
      euroLottery_results$winning_scale[i] = 6
    }else if(first_five==4&last_two==0)
    {
      payout_results[7] = payout_results[7] + 1
      euroLottery_results$winning_scale[i] = 7
    }else if(first_five==3&last_two==2)
    {
      payout_results[8] = payout_results[8] + 1
      euroLottery_results$winning_scale[i] = 8
    }else if(first_five==4&last_two==1)
    {
      payout_results[9] = payout_results[9] + 1
      euroLottery_results$winning_scale[i] = 9
    }else if(first_five==4&last_two==2)
    {
      payout_results[10] = payout_results[10] + 1
      euroLottery_results$winning_scale[i] = 10
    }else if(first_five==5&last_two==0)
    {
      payout_results[11] = payout_results[11] + 1
      euroLottery_results$winning_scale[i] = 11
    }else if(first_five==5&last_two==1)
    {
      payout_results[12] = payout_results[12] + 1
      euroLottery_results$winning_scale[i] = 12
    }else if(first_five==5&last_two==2)
    {
      payout_results[13] = payout_results[13] + 1
      euroLottery_results$winning_scale[i] = 13
    }
  }
  
  return(euroLottery_results)
}