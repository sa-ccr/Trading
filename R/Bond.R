######################################################################
#'
#' Creates a Bond object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title Bond Class
#' @param Notional The notional amount of the trade
#' @docType NULL
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param yield The yield of the Bond
#' @param ISIN The ISIN of the Bond,
#' @param payment_frequency the frequency that the bond pays coupon (Quarter, SA etc)
#' @param maturity_date the maturity date of the bond
#' @param coupon_type The coupon type of the bond (fixed, floating, flipper etc)
#' @param credit_risk_weight The percentage weight of the exposure of the bond that should be attributed to the 'Credit' asset class
#' @param Issuer The issuer of the bond
#' @return An object of type Bond
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples 
#' 
#' tr1 = Bond(Notional=10000,MtM=30,Currency="EUR",Si=0,maturity_date="2026-04-04",
#' BuySell='Buy',payment_frequency="SA",
#' credit_risk_weight=0.2,coupon_type="Fixed",Issuer="FirmA",ISIN = "XS0943423")

#' @include IRD.R
Bond = setRefClass("Bond",
                   fields = list(isin = "character", trade_price = "numeric",yield="numeric",
                                 payment_frequency="character", maturity_date="character",coupon_type="character",credit_risk_weight="numeric",Issuer="character"),
                   contains=c("IRD"),
                   methods = list(
                     initialize = function(...){
                       SubClass<<-' '
                       callSuper(..., TradeType='Bond')}
                     ,
                     ValidateCreditRiskWeight = function()
                     {
                       if((!is.numeric(credit_risk_weight)))
                         stop("The credit risk weight has to be a percentage (numeric)")

                       if(credit_risk_weight>1||credit_risk_weight<0)
                         stop("The credit risk weight has to be a percentage..can't be negative or bigger than one.")
                     },
                     SplitBondExposure = function()
                     {
                     if(coupon_type=='Fixed'&& BuySell=='Buy')
                      {   ir_direction = 'Sell'
                     }else
                     {
                       ir_direction='Buy'
                     }
                     tr1 = IRDSwap(Notional=credit_risk_weight*Notional,MtM=credit_risk_weight*MtM,Currency=Currency,Si=Si,Ei=Ei,BuySell=ir_direction)
                     tr2 = CreditSingle(Notional=credit_risk_weight*Notional,MtM=credit_risk_weight*MtM,Si=Si,Ei=Ei,BuySell=ifelse(BuySell=='Buy','Sell','Buy'),SubClass='AA',RefEntity=Issuer)
                     trades = list(tr1,tr2)
                     return(trades)
                     }

                   ))


#'
#' Creates a Bond Future object with the relevant info needed to calculate the Exposure-at-Default (EAD)
#' @title Bond Future Class
#' @param Notional The notional amount of the trade
#' @docType NULL
#' @param MTM      The mark-to-market valuation of the trade
#' @param Currency The currency set that the trade belongs to
#' @param Si The number of years that the trade will take to start (zero if already started)
#' @param Ei The number of years that the trade will expire
#' @param BuySell Takes the values of either 'Buy' or 'Sell'
#' @param yield The yield of the Underlying Bond
#' @param isin The ISIN of the Underlying Bond,
#' @param payment_frequency the frequency that the bond pays coupon (Quarter, SA etc)
#' @param maturity_date the maturity date of the bond
#' @param coupon_type The coupon type of the bond (fixed, floating, flipper etc)
#' @param Issuer The issuer of the bond
#' @return An object of type Bond
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#'
#' example_trades = ParseTrades()
#' bondfuture_trade = example_trades[[17]]
#' tr1 = BondFuture(Notional=10000,MtM=30,Currency="EUR",Si=0,Ei=10,BuySell='Buy',
#' payment_frequency="SA",coupon_type="Fixed",Issuer="CountryA",ISIN = "XS0943423")

BondFuture = setRefClass("BondFuture",
                   contains=c("Bond"))
