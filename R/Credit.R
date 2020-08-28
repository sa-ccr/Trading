######################################################################
# Create the base Credit class
#
# This is used to represent the Credit asset class and it is the parent of the CDS and CDX subclasses
#' @include Trade.R

Credit = setRefClass("Credit",
                   fields = list(RefEntity= "character"),
                   contains="Trade",
                   methods = list(
                     initialize = function(...){
                       callSuper(...,TradeGroup='Credit')
                     }
                   ))

#' Creates a CDS Object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title  CDS Class
#' @param Notional The notional amount of the trade
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param SubClass Specifies the rating of the underlying entity (possible values are A, AA, BB etc)
#' @param RefEntity The name of the underlying entity
#' @return An object of type CDS
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
#' @examples
#' 
#' ## the CDS trade given in the Basel regulation Credit example
#' tr1 = CDS(Notional=10000,MtM=20,Currency="USD",Si=0,Ei=3,BuySell='Buy',
#' SubClass='AA',RefEntity='FirmA')

CDS = setRefClass("CDS",
                       
                       contains="Credit",
                       
                       methods = list(
                         initialize = function(...){
                           callSuper(...,TradeType='Single')
                         }
                       ))

#' Creates a Credit Index Object with the relevant info needed to calculate the Exposure-at-Default (EAD) 
#' @title CDX Class
#' @param Notional The notional amount of the trade
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the belongs 
#' @param Si The number of years after which the trade will start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param SubClass Specifies if the underlying Index is investment grade or not (possible values are IG & SG)
#' @param RefEntity The name of the underlying Index
#' @return An object of type CDX
#' @export
#' @examples
#' 
#' ## the CDX trade given in the Basel regulation Credit example
#' tr3 = CDX(Notional=10000,MtM=0,Currency="USD",Si=0,Ei=5,
#' BuySell='Buy',SubClass='IG',RefEntity='Portfolio_1')


CDX = setRefClass("CDX",
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
#' @return An object of type CDOTrance
#' @export
#' @examples
#' 
#' ## a CDO trance object
#' tr3 = CDOTranche(Notional=10000,MtM=0,Currency="USD",Si=0,Ei=5,
#' BuySell='Buy',SubClass='IG',RefEntity='CDX.IG',cdo_attach_point=0.3 ,cdo_detach_point=0.5)


CDOTranche = setRefClass("CDOTranche",
                          contains="Credit",
                         
                         fields = list(cdo_attach_point = "numeric",
                                       cdo_detach_point = "numeric"), 
                         
                          methods = list(
                            initialize = function(...){
                              callSuper(...)
                            },
                            CalcSupervDelta = function()
                            {
                              if(BuySell=="Buy")
                              { superv_delta = 15/((1+14*cdo_attach_point)*(1+14*cdo_detach_point))   
                              }else
                              { superv_delta = -15/((1+14*cdo_attach_point)*(1+14*cdo_detach_point)) }
                            }
                              
                          ))
