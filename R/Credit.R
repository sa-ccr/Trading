######################################################################
# Create the base Credit class
#
# This is used to represent the Credit asset class and it is the parent of the CreditSingle and CreditIndex subclasses
#' @include Trade.R

Credit = setRefClass("Credit",
                   fields = list(RefEntity= "character"),
                   contains="Trade",
                   methods = list(
                     initialize = function(...){
                       callSuper(...,TradeGroup='Credit')
                     }
                   ))

#' Creates a Credit Single Object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title  Credit Single Class
#' @param Notional The notional amount of the trade
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @return An object of type CreditSingle
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
#' @examples
#' 
#' ## the CreditSingle trade given in the Basel regulation Credit example
#' tr1 = CreditSingle(Notional=10000,MtM=20,Currency="USD",Si=0,Ei=3,BuySell='Buy',
#' SubClass='AA',RefEntity='FirmA')

CreditSingle = setRefClass("CreditSingle",
                       
                       contains="Credit",
                       
                       methods = list(
                         initialize = function(...){
                           callSuper(...,TradeType='Single')
                         }
                       ))

#' Creates a Credit Index Object with the relevant info needed to calculate the Exposure-at-Default (EAD) 
#' @title Credit Index Class
#' @param Notional The notional amount of the trade
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the belongs 
#' @param Si The number of years after which the trade will start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @return An object of type CreditIndex
#' @export
#' @examples
#' 
#' ## the CreditIndex trade given in the Basel regulation Credit example
#' tr3 = CreditIndex(Notional=10000,MtM=0,Currency="USD",Si=0,Ei=5,
#' BuySell='Buy',SubClass='IG',RefEntity='CDX.IG')


CreditIndex = setRefClass("CreditIndex",
                           contains="Credit",
                           
                           methods = list(
                             initialize = function(...){
                               callSuper(...,TradeType='Index')
                             }
                           ))


#' Creates a CDO tranche Object with the relevant info needed to calculate the Exposure-at-Default (EAD) 
#' @title CDO tranche Class
#' @param Notional The notional amount of the trade
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the belongs 
#' @param Si The number of years after which the trade will start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param attach_point The attachment point of the tranche
#' @param detach_point The detachment point of the tranche
#' @return An object of type CDOtrance
#' @export
#' @examples
#' 
#' ## a CDO trance object
#' tr3 = CDOTranche(Notional=10000,MtM=0,Currency="USD",Si=0,Ei=5,
#' BuySell='Buy',SubClass='IG',RefEntity='CDX.IG',attach_point=0.3,detach_point=0.5)


CDOTranche = setRefClass("CDOTranche",
                          contains="Credit",
                         
                         fields = list(attach_point = "numeric",
                                       detach_point = "numeric"), 
                         
                          methods = list(
                            initialize = function(...){
                              callSuper(...)
                            },
                            CalcSupervDelta = function()
                            {
                              if(BuySell=="Buy")
                              { superv_delta = 15/((1+14*attach_point)*(1+14*detach_point))   
                              }else
                              { superv_delta = -15/((1+14*attach_point)*(1+14*detach_point)) }
                            }
                              
                          ))
