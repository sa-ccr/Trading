######################################################################
# Create the base FX class
#
# This is used to represent the FX asset class and it is the parent of the FXSwap subclasses
#' @include Trade.R
FX = setRefClass("FX",
                   fields = list(ccyPair = "character"),
                   contains="Trade",
                   methods = list(
                     initialize = function(...){
                       SubClass<<-' '
                       callSuper(...,TradeGroup='FX')
                     }
                   ))
#' 
#' Creates an FX Swap object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title FX Swap Class
#' @param Notional The notional amount of the trade
#' @docType NULL
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency that the input amounts are in
#' @param ccyPair The currency Pair of the trade
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param traded_price the price that trade was done
#' @param fx_near_leg_fields (Optional) In case the near leg hasn't settled yet, its notional, MtM, settlement date should be provided separated via a semicolon
#' @return An object of type FXSwap
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
#' @examples
#' 
#'
#' tr1 = FxSwap(Notional=10000,MtM=30,ccyPair="EUR/USD",Si=0,Ei=10,BuySell='Buy',fx_near_leg_fields='1000;-20;2020-02-11')

FxSwap = setRefClass("FxSwap",
                     fields = list(fx_near_leg_fields      = 'character'
                     ),
                       contains=c("FX","Swap"),
                       
                       methods = list(
                         initialize = function(...){
                           callSuper(...,TradeType='Swap')
                         }
                        
                       ))

#' Creates a FX Forward Object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title FX Forward Class
#' @include Trade.R
#' @param Notional The notional amount of the trade
#' @docType NULL
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency that the input amounts are in
#' @param ccyPair The currency Pair of the trade
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param traded_price the price that trade was done
#' @return An object of type FX Forward
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
#' @examples
#'
#' ## a FX Forward trade 
#' tr1 = FxForward(Notional=10000,MtM=-50,Si=0,Ei=0.75,BuySell='Buy',ccyPair="EUR/USD")

FxForward = setRefClass("FxForward",
                               contains="FX",
                               methods = list(
                                 initialize = function(...){
                                   callSuper(...,TradeGroup='FX')}
                               ))