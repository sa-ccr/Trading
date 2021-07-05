#' Select the derivatives out of a trades' list which will be utilized to calculate the CCR Exposure.
#' @title Select the derivatives out of a trades' list
#' @param trades_list the file holding the trades of the portfolio
#' @return The derivatives out of a trades' list
#' @export
#' @author Tasos Grivas <info@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj

SelectDerivatives <- function(trades_list)  {
  
  derivs = c("Swap", "Forward", "CDS","CDX", "CDO","Future","OtherExposure","Option")
  
  matches <- unique (grep(paste(derivs ,collapse="|"), unlist(lapply(trades_list,class)), value=FALSE))
  
  return(trades_list[matches])
}