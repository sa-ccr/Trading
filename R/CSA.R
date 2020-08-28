#' Creates a collateral agreement Object containing all the relevant data and methods regarding the maturity factor
#'  and the calculation of the exposures after applying the relevant threshold
#' @title  CSA Class
#' @param ID The ID of the CSA ID
#' @param Counterparty The counterparty the CSA is linked to
#' @param Currency The currency that the CSA applies to (can be a list of different currencies)
#' @param TradeGroups The trade groups that the CSA applies to
#' @param Values_type The type of the numerical values (can be "Actual" or "Perc" whereby the values are percentages of the MtM)
#' @param thres_cpty The maximum exposure that the counterparty can generate before collateral will need to be posted
#' @param thres_PO   The maximum exposure that the processing organization can generate before collateral will need to be posted
#' @param MTA_cpty   The minimum transfer amount for the counterparty
#' @param MTA_PO     The minimum transfer amount for the processing organization
#' @param IM_cpty    The initial margin that is posted by the counterparty
#' @param IM_PO      The initial margin that is posted by the processing organization
#' @param mpor_days  The margin period of risk in days
#' @param remargin_freq The frequency of re-margining the exposure in days
#' @param rounding   The rounding amount of the transfers
#' @return An object of type CSA
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
#' @examples
#'
#'
#'   csa_raw = read.csv(system.file("extdata", "CSA.csv", package = "Trading"),
#'   header=TRUE,stringsAsFactors = FALSE)
#'
#' csas = list()
#' for(i in 1:nrow(csa_raw))
#' {
#'  csas[[i]] = CSA()
#'  csas[[i]]$PopulateViaCSV(csa_raw[i,])
#' }

CSA = setRefClass("CSA",

                  fields = list(ID         = "character",
                                thres_cpty = "numeric",
                                thres_PO   = "numeric",
                                MTA_cpty   = "numeric",
                                MTA_PO     = "numeric",
                                IM_cpty    = "numeric",
                                IM_PO      = "numeric",
                                mpor_days  = "numeric",
                                remargin_freq   = "numeric",
                                rounding   = "numeric",
                                Counterparty = "character",
                                Currency   = "list",
                                TradeGroups ="list",
                                Values_type = "character"

                  ),

                  methods = list(
                    ApplyThres = function(MtM_vector)
                    {

                      MtM_len = length(MtM_vector)
                      coll_MtM    = rep(0,MtM_len)
                      collateral = 0
                      coll_MtM[1] = MtM_vector[1]

                      for (i in 1:(MtM_len/2))
                      {

                        if(coll_MtM[2*(i-1)+1]>thres_cpty+MTA_cpty)
                        { collateral = collateral + coll_MtM[2*(i-1)+1]-thres_cpty-MTA_cpty
                        } else if(coll_MtM[2*(i-1)+1]< (-thres_PO-MTA_PO))
                        { collateral = collateral + coll_MtM[2*(i-1)+1]-(-thres_PO-MTA_PO)
                        }   else if(i>2&&collateral>0&&MtM_diff<0)
                        { collateral = max(collateral+MtM_diff,0)
                        } else if(i>2&&collateral<0&&MtM_diff>0)
                        { collateral = min(collateral+MtM_diff,0)
                        }

                        coll_MtM[2*i]   = MtM_vector[2*i]   - collateral
                        coll_MtM[2*i+1] = MtM_vector[2*i+1] - collateral
                        MtM_diff = MtM_vector[2*i+1] - MtM_vector[2*i-1]
                      }
                      return(coll_MtM)
                    },
                    CalcMF = function(simplified)
                    {
                      if(simplified)
                      {
                        return(0.42)
                      }else
                      {
                        MPOR = 10 + remargin_freq -1
                        return(1.5*sqrt(MPOR/250))
                      }
                    },
                    PopulateViaCSV = function(csa_raw)
                    {
                      if(missing(csa_raw))
                      {
                        raw_data <- read.csv(system.file("extdata", 'CSA.csv', package = "Trading"))
                      }else
                      {
                        raw_data <- csa_raw
                      }


                      # raw_data <- read.csv("CSA.csv",header=TRUE,stringsAsFactors = FALSE,strip.white=TRUE)
                      non_numeric= c("Counterparty","Currency","TradeGroups","ID","Values_type")

                      names_raw_data = names(raw_data)

                      for(i in 1:length(names_raw_data))
                      {
                        if(names_raw_data[i] %in% non_numeric) {
                          if(names_raw_data[i] %in% c("Currency","TradeGroups"))
                          {
                            temp_value = paste0("'",as.character(eval(parse(text=paste0("raw_data$",names_raw_data[i])))),"'")
                            semicol_pos = gregexpr(';',temp_value)
                            if(semicol_pos[[1]][1]!=-1)
                            {
                              temp_list = list()
                              for(j in 1:(length(semicol_pos[[1]])+1))
                              {
                                if(j==1)
                                {
                                  temp_list[[j]] = substr(temp_value,2,semicol_pos[[1]][j]-1)
                                } else if(j==(length(semicol_pos[[1]])+1))
                                {
                                  temp_list[[j]] = substr(temp_value,semicol_pos[[1]][j-1]+1,nchar(temp_value)-1)
                                } else
                                {
                                  temp_list[[j]] = substr(temp_value,semicol_pos[[1]][j-1]+1,semicol_pos[[1]][j]-1)
                                }
                              }
                              if(names_raw_data[i] == "Currency")
                              {
                                Currency <<- temp_list
                                next
                              } else if(names_raw_data[i] == "TradeGroups")
                              {
                                TradeGroups <<- temp_list
                                next
                              }
                            }

                          }else
                          {      temp_value = paste0("'",as.character(eval(parse(text=paste0("raw_data$",names_raw_data[i])))),"'")}
                        }else
                        { temp_value = as.numeric(eval(parse(text=paste0("raw_data$",names_raw_data[i]))))}

                        if(names_raw_data[i] == "Currency")
                        {
                          Currency <<- list(temp_value)
                          next
                        } else if(names_raw_data[i] == "TradeGroups")
                        {
                          TradeGroups <<- list(temp_value)
                          next
                        }

                          eval(parse(text=paste(names_raw_data[i],"<<-",temp_value)))
                      }
                    }
))
