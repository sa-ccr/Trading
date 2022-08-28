#' @description Returns the Total carbon emissions for a portfolio normalized by the market value of the portfolio,
#' expressed in tons CO2e / $M invested.Scope 1 and Scope 2 GHG emissions are allocated to investors based on an equity
# ownership approach while the current portfolio value is used to normalize the data.                                                                  
# 
#' @title Carbon Footprint
#' @param portfolio_exposure The exposure per issuer in the portfolio
#' @param emissions_capitalization_data The capitalization and the Scope 1 & 2 GHG emissions per issuer
#' @return Total carbon emissions for a portfolio normalized by the market value of the portfolio, expressed in tons CO2e / $M invested.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://www.tcfdhub.org/Downloads/pdfs/E09%20-%20Carbon%20footprinting%20-%20metrics.pdf
#' 
#' @examples
#'  portfolio_exposure     = data.table::data.table(Issuers = c('A','B','C'),
#'  exposures = c(100, 200, 50))
#'  emissions_capitalization_data = data.table::data.table(Issuers = c('A','B','C'), 
#'  emissions = c(1000, 5000, 6000),  Capitalization = c(20000, 10000, 30000))
#'  Carbon_Footprint(portfolio_exposure, emissions_capitalization_data)
Carbon_Footprint <- function(portfolio_exposure, emissions_capitalization_data)  
{
  merged_data = setkey(portfolio_exposure,'Issuers')[setkey(emissions_capitalization_data,'Issuers')]
  #   that's the efficient way to do it - won't work with CRAN checks though...
  #   return(sum(merged_data[,exposures/Capitalization*emissions])/sum(merged_data$exposures))
  return(sum(merged_data$exposures/merged_data$Capitalization*merged_data$emissions)/sum(merged_data$exposures))  
  
}