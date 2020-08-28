#' Creates a Commodity Object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title Commodity Class
#' @include Trade.R
#' @param Notional The notional amount of the trade
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param commodity_type Takes the values of 'Oil/Gas','Silver','Electricity' etc.
#' @return An object of type Commodity
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
#' @examples
#'
#' 
#' tr1 = Commodity(Notional=10000,MtM=-50,
#' BuySell='Buy',SubClass='Energy',commodity_type='Oil')

Commodity = setRefClass("Commodity",
                         fields = list(commodity_type      = 'character'
                         ),
                      contains="Trade",
                      methods = list(
                        initialize = function(...){
                          callSuper(...,TradeGroup='Commodity')}
                      ))
#' Creates a Commodity Swap Object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title Commodity Swap Class
#' @include Swap.R
#' @return An object of type CommSwap
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
CommSwap = setRefClass("CommSwap",
                       fields = list(),
                       contains=c("Swap","Commodity"),
                       methods = list(
                         initialize = function(...){
                           callSuper(...)}
                       ))
#' Creates a Commodity Forward Object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title Commodity Forward Class
#' @include Trade.R
#' @param Notional The notional amount of the trade
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param commodity_type Takes the values of 'Oil','Gas','Silver','Electricity' etc.
#' @param SubClass Defines the relevant hedging set. Possible values: 'Energy','Agriculture','Metal','Other','Climatic'
#' @return An object of type Commodity Forward
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj
#' @examples
#'
#' ## the Commodity Forward trade given in the Basel regulation Commodity example
#' tr1 = CommodityForward(Notional=10000,MtM=-50,Si=0,Ei=0.75,
#' BuySell='Buy',SubClass='Energy',commodity_type='Oil')

CommodityForward = setRefClass("CommodityForward",
                        contains="Commodity",
                        methods = list(
                          initialize = function(...){
                            callSuper(...,TradeGroup='Commodity')}
                        ))