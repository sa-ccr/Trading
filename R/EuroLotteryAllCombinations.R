#' Returns all the possible number combinations for EuroMillions/EuroJackpot
#' @title  Returns all the possible number combinations for EuroMillions/EuroJackpot
#' @return PnL figures
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' # returns all the 139,838,160 possible combinations, can create memory issues.
#' # all_combinations = EuroLotteryAllCombinations()
 
EuroLotteryAllCombinations <- function()  {

all_combs = comboGrid(1:50,1:50,1:50,1:50,1:50, repetition = F)
all_combs2 = comboGrid(1:12,1:12, repetition = F)

all_combs = OuterJoinMerge(all_combs, all_combs2)

return(all_combs)
}