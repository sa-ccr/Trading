#' Parse trades through a .csv file. In case no file name is given, an example file is automatically loaded containing trades corresponding to Basel's SA-CCR regulation (the example trades file can be found on the extdata folder in the installation library path)
#' @title Parse trades through a .csv file.
#' @param csvfilename the name of csv file containing the trades
#' @return A list of trades
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#'
#' @examples
#'
#' ## calling ParseTrades() without an argument loads a test file containing all
#' ## the different trade types supported
#' example_trades = ParseTrades()
#'
ParseTrades = function(csvfilename)
{
  if(missing(csvfilename))
  {
    trades = read.csv(system.file("extdata", 'example_trades.csv', package = "Trading"),stringsAsFactors = FALSE,strip.white=TRUE)
  }else{
    trades = read.csv(csvfilename,stringsAsFactors = FALSE,strip.white=TRUE)
  }

  trades_list = list()
  numerics=c( "Notional" ,"MtM","Si","Ei","UnderlyingPrice","StrikePrice","vol_strike","annualization_factor","credit_risk_weight","traded_price","maturity","cdo_attach_point","cdo_detach_point")

  digital_option_indices = which(trades$Exotic_Type=='Digital')
  
  added_trades = data.frame()
  if(length(digital_option_indices)!=0)
  {
    for(i in 1:length(digital_option_indices))
    {
      trades_temp = trades[digital_option_indices[i],]
      trades_temp_long  = trades_temp
      trades_temp_long$BuySell     = 'Buy'
      trades_temp_long$StrikePrice = 0.95*trades_temp_long$StrikePrice
      trades_temp_long$Exotic_Type = paste0(trades_temp_long$Exotic_Type,as.character(digital_option_indices[i]))
      trades_temp_long$external_id = paste0(trades_temp_long$external_id,'L')
      trades_temp_short = trades_temp
      trades_temp_short$BuySell     = 'Sell'
      trades_temp_short$StrikePrice = 1.05*trades_temp_short$StrikePrice
      trades_temp_short$Exotic_Type = paste0(trades_temp_short$Exotic_Type,as.character(digital_option_indices[i]))
      trades_temp_short$external_id = paste0(trades_temp_short$external_id,'S')
      added_trades = rbind(added_trades,trades_temp_long,trades_temp_short)
    }
    trades= trades[-digital_option_indices,]
    trades = rbind(trades, added_trades)
  }
  for(i in 1:nrow(trades))
  {
    trades_temp = trades[i,]
    trades_temp= trades_temp[,-which(is.na(trades_temp)|trades_temp=="")]
    
    to_be_eval = paste(trades_temp$TradeObj,"(")
    for(j in 2:ncol(trades_temp))
    {
      if(names(trades_temp)[j] %in% numerics)
      {
        if(j==ncol(trades_temp))
        {
          to_be_eval = paste(to_be_eval,names(trades_temp)[j],"=",as.character(trades_temp[j]),sep="")
        }else
        {
          to_be_eval = paste(to_be_eval,names(trades_temp)[j],"=",as.character(trades_temp[j]),",",sep="")
        }
      }else
      {
        if(j==ncol(trades_temp))
        {
          to_be_eval = paste(to_be_eval,names(trades_temp)[j],"='",as.character(trades_temp[j]),"'",sep="")
        }else
        {
          to_be_eval = paste(to_be_eval,names(trades_temp)[j],"='",as.character(trades_temp[j]),"'",",",sep="")
        }

      }
    }


    to_be_eval = paste(to_be_eval,")")
    trades_list[[i]] = eval(parse(text=to_be_eval))
  }
  return(trades_list)
}
