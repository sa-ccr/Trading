Trade = setRefClass("Trade",
                    fields = list(     Notional   = "numeric",
                                       MtM        = "numeric",
                                       Currency   = "character",
                                       Si         = "numeric",
                                       Ei         = "numeric",
                                       BuySell    = "character",
                                       TradeGroup = "character",
                                       TradeType  = "character",
                                       SubClass   = "character",
                                       ccyPair    = "character",
                                       ISIN = "character",
                                       traded_price = "numeric",
                                       external_id = "character",
                                       Counterparty = "character",
                                       nickname = "character",
                                       Exotic_Type = "character",
                                       Netting_Set = "character",
                                       Underlying_Instrument= "character",
                                       simplified = "logical",
                                       ccy_paying= "character",
                                       amount_paying= "numeric",
                                       ccy_receiving= "character",
                                       amount_receiving= "numeric",
                                       base_ccy ="character"
                    ),
                    methods = list(
                      CalcAdjNotional = function() {
                        ## calculates the adjusted notional by multiplying the notional amount with the
                        ## supervisory duration
                        if (TradeGroup=='IRD'||TradeGroup=='Credit')
                        {
                          AdjustedNotional = Notional * CalcSupervDuration() ;
                        } else
                        {
                          AdjustedNotional = Notional;
                        }
                        
                        return(AdjustedNotional)
                      },
                      CalcSupervDuration = function() {
                        ## calculates the supervisory duration (applicable for IRDs and Credit derivatives)
                        if(length(simplified)!=0)
                        {
                          if(simplified)
                            return(Ei-Si)
                        }
                        return((exp(-0.05*Si)-exp(-0.05*Ei))/0.05);
                        
                      },
                      CalcMaturityFactor = function(del_type) {
                        ## calculates the maturity factor
                        if(missing(del_type))
                        {Mi=Ei
                        }else
                        {
                          if(del_type=="Physical")
                            Mi=Ei
                          else
                            Mi=Si
                        }
                        if(Mi<1)
                        {MaturityFactor = sqrt(Mi)
                        }else MaturityFactor = 1;
                        
                        if("Future" %in% getClassDef(class(.self))@refSuperClasses) MaturityFactor=10/252
                        
                        return(max(10/252,MaturityFactor))
                      },
                      CalcSupervDelta = function(Superv_Vol) {
                        
                        if(length(simplified)!=0)
                        {
                          if(simplified)
                          {
                            if(toupper(BuySell)=="BUY")   return(1)
                            if(toupper(BuySell)=="SELL")  return(-1)
                          }
                        }
                        if (missing(Superv_Vol))
                        {
                          if(toupper(BuySell)=="BUY")   return(1)
                          if(toupper(BuySell)=="SELL")  return(-1)
                        } else
                        {
                          if(all(c("Swap","Option") %in% getClassDef(class(.self))@refSuperClasses))
                            option_mat = Si
                          else
                            option_mat = Ei
                          
                          # in the option case the supervisory volatility is being populated
                          # the delta calculation is based on the Black-Scholes formula
                          if(UnderlyingPrice*StrikePrice<0){
                            lamda = max(0.001 - min(UnderlyingPrice,StrikePrice),0)
                            temp = (log((UnderlyingPrice+lamda)/(StrikePrice+lamda))+0.5*Superv_Vol^2*option_mat)/(Superv_Vol*option_mat^0.5);
                          }else
                          {  temp = (log(UnderlyingPrice/StrikePrice)+0.5*Superv_Vol^2*option_mat)/(Superv_Vol*option_mat^0.5);}
                          
                          if(toupper(BuySell)=="BUY")
                          {
                            if(OptionType=='Call') return(pnorm(temp))
                            if(OptionType=='Put')  return(-pnorm(-temp))
                          }
                          if(toupper(BuySell)=="SELL")
                          {
                            if(OptionType=='Call') return(-pnorm(temp))
                            if(OptionType=='Put')  return(pnorm(-temp))
                          }
                        }
                      },
                      setFXDynamic = function() {
                        if(TradeGroup=="FX")
                        {
                          if(length(ccyPair)==0||length(BuySell)==0)
                          {
                            priority_ccies = c("EUR","JPY","USD","CZK","HRK","HUF","BAM","RSD","RUB")
                            if(length(ccy_receiving)!=0&&length(ccy_paying)!=0&&length(amount_paying)!=0&&length(amount_receiving)!=0)
                            {
                              if(ccy_receiving %in% priority_ccies||ccy_receiving %in% priority_ccies)
                              {
                                receiving_spot = ifelse((ccy_receiving %in% priority_ccies==FALSE),100,which(ccy_receiving == priority_ccies))
                                paying_spot = ifelse((ccy_paying %in% priority_ccies==FALSE),100,which(ccy_paying == priority_ccies))
                                
                                starting_ccy = min(receiving_spot,paying_spot)
                                
                                ifelse(starting_ccy==receiving_spot,ccyPair<<-paste0(ccy_receiving,"/",ccy_paying),ccyPair<<-paste0(ccy_paying,"/",ccy_receiving))
                                
                                if(receiving_spot<paying_spot)
                                {BuySell<<-"Buy"
                                }else
                                {BuySell<<-"Sell"}
                                
                              }else
                              {ccyPair<<-paste0(ccy_receiving,"/",ccy_paying)
                              BuySell<<-"Buy"
                              }
                            }
                            if(length(Notional)==0)
                            {
                              if(ccy_receiving==base_ccy)
                              {Notional <<- amount_paying
                              }else if(ccy_paying==base_ccy)
                              {Notional <<-amount_receiving
                              }else
                              {Notional<<-max(amount_paying,amount_receiving)}
                            }
                          }
                          
                        }
                      }
                    ))
