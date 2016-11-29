#' Returns a list with the populated fields of a Trade Object
#' @title  Returns a list with the populated fields of a Trade Object
#' @param trade        A trade Object
#' @return A list of fields
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' example_trades = ParseTrades()
#' Equity_Index_Future_trade = example_trades[[18]]
#' populated_fields = GetTradeDetails(Equity_Index_Future_trade)

GetTradeDetails <- function(trade)  {
  
trade_details = list()
field_names = names(trade$getRefClass()@generator$fields())
for(m in 1:length(field_names))
{
  if(length(trade$field(field_names[m]))!=0)
    trade_details[field_names[m]] = trade$field(field_names[m])
}
return(trade_details)
}
