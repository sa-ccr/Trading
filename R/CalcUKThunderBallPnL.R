#' Calculates the PnL for a pay out structure created during backtesting 
#' @title  PnL calculation for UKThunderBall backtesting
#' @param backtested_results The UKThunderBall results backtested against the user input 
#' @param plot_results (Optional) If TRUE, the P&L historical graphs are plotted, default FALSE 
#' @return PnL figures
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' ukthunderball_results = UKThunderBallResults()
#' user_input = c(10,20,30,31,32,5)
#' backtested_results = UKThunderballBacktesting(ukthunderball_results, user_input = user_input)
#' pnl_result = CalcUKThunderBallPnL(backtested_results, plot_results = FALSE)

CalcUKThunderBallPnL <- function(backtested_results, plot_results = FALSE)  {
  
  payout_categories =c("one","two","twoone","three","threeone","four",
                       "fourone","five","fiveone")
  payout_structure = data.frame(seq(0,length(payout_categories)))
  payout_prizes = c(0,3,5,10,10,20,100,250,5000,500000)
  payout_structure = data.table(cbind(payout_structure, payout_prizes))
  names(payout_structure)[1]='winning_scale'
  
  backtested_results = setkey(payout_structure,"winning_scale")[setkey(backtested_results,"winning_scale")]
  backtested_results = backtested_results[order(dates_parsed)]
  backtested_results[,cum_profit:=cumsum(payout_prizes)]
  backtested_results$cost = 1
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