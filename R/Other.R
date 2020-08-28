#' Creates a OtherExposure Object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title OtherExposure Class
#' @include Trade.R
#' @param Notional The notional amount of the trade
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param SubClass Defines the hedging set the relevant trade will belong to
#' @return An object of type OtherExposure
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj
#' @examples
#'
#' 
#' tr1 = OtherExposure(Notional=10000,MtM=-50,Si=0,Ei=10,
#' BuySell='Buy',SubClass='Other_1')

OtherExposure = setRefClass("OtherExposure",
                        fields = list(),
                        contains=c("Trade"),
                        methods = list(
                          initialize = function(...){
                            callSuper(...,TradeGroup='OtherExposure')}
                        ))