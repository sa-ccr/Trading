#' Calculates the PnL for a pay out structure created during backtesting 
#' @title  PnL calculation for UKLottery backtesting
#' @param backtested_results The UKLottery results backtested against the user input 
#' @param plot_results (Optional) If TRUE, the P&L historical graphs are plotted, default FALSE 
#' @return PnL figures
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' uklottery_results = UKLotteryResults()
#' user_input = c(5,10,20,30,40,50)
#' backtested_results = UKLotteryBacktesting(uklottery_results,, user_input)
#' pnl_result = CalcUKLotteryPnL(backtested_results, plot_results = FALSE)

CalcUKLotteryPnL <- function(backtested_results, plot_results = FALSE)  {
  
  payout_categories =c("two","three","four","five","fiveone","six")
  payout_structure = data.frame(seq(0,length(payout_categories)))
  payout_prizes = c(0,0,30,140,1750,1000000,100000000)
  payout_structure = data.table(cbind(payout_structure, payout_prizes))
  names(payout_structure)[1]='winning_scale'
  
  backtested_results = setkey(payout_structure,"winning_scale")[setkey(backtested_results,"winning_scale")]
  backtested_results = backtested_results[order(dates_parsed)]
  backtested_results[,cum_profit:=cumsum(payout_prizes)]
  backtested_results$cost = 2
  backtested_results[,cum_cost := cumsum(cost)]
  backtested_results[,final_pnl:=cum_profit-cum_cost]
  
  if(plot_results)
  {
    a=ggplot()+
      geom_line(data=backtested_results,aes(y=final_pnl,x= dates_parsed,colour="Total P&L"),size=1 )+
      geom_line(data=backtested_results,aes(y=-cum_cost,x= dates_parsed,colour="Total Cost"),size=1) +
      geom_line(data=backtested_results,aes(y=cum_profit,x= dates_parsed,colour="Total Profit"),size=1) +
      scale_color_manual(name = "P&L Graphs", values = c("Total P&L" = "darkblue", "Total Cost" = "red", "Total Profit" = "green"))+xlab("Dates")+ylab("Cumulative P&L Results")
    print(a)
  }
  
  return(backtested_results)
  
}