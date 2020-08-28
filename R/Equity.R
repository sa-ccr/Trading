#' 
#' Creates an Equity object 
#' @title Equity Class
#' @param Notional The notional amount of the trade
#' @docType NULL
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param ISIN the ISIN of the Equity
#' @param traded_price the price that trade was done
#' @param Issuer the issuer of the stock
#' @return An object of type Equity
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#'
#' tr1 = Equity(external_id="ext1",Notional=10000,MtM=30,Currency="EUR",BuySell='Buy',
#' traded_price = 10,ISIN = "XS04340432",Issuer='FirmA')
# This is used to represent the Equity asset class
#' @include Trade.R
Equity = setRefClass("Equity",
                 contains="Trade",
                 fields = list( Issuer = "character"),
                 methods = list(
                   initialize = function(...){
                     SubClass<<-' '
                     callSuper(...,TradeGroup='EQ')
                   }
                 ))
#' 
#' Creates an Equity Index Future object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title Equity Index Future Class
#' @param Notional The notional amount of the trade
#' @docType NULL
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param traded_price the price that trade was done
#' @return An object of type EquityIndexFuture
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' example_trades = ParseTrades()
#' Equity_Index_Future_trade = example_trades[[18]]

EquityIndexFuture = setRefClass("EquityIndexFuture",
                     
                     contains="Equity",
                     methods = list(
                       initialize = function(...){
                         callSuper(...,TradeType='Index')
                       }
                       
                     ))

#' 
#' Creates an Equity Option object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title Equity Option Class
#' @param Notional The notional amount of the trade
#' @docType NULL
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param traded_price the price that trade was done
#' @return An object of type EquityOption
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>

EquityOption = setRefClass("EquityOption",
                                
                                contains=c("Equity","Option"),
                                methods = list(
                                  initialize = function(...){
                                    callSuper(...,TradeType='Index')
                                  }
                                  
                                ))
