#' @description Returns the Volume of carbon emissions per million dollars of revenue expressed in tons CO2e / $M revenue.
#' Scope 1 and Scope 2 GHG emissions are allocated to investors based on an equity ownership approach.
#' The company's (or issuer's) revenue is used to adjust for company size to provide a measurement of the efficiency of output.
#' @title Carbon Intensity
#' @param portfolio_exposure The exposure per issuer in the portfolio
#' @param emissions_capitalization_revenue_data The capitalization, revenue and the Scope 1 & 2 GHG emissions per issuer
#' @return Volume of carbon emissions per million dollars of revenue expressed in tons CO2e / $M revenue.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://www.tcfdhub.org/Downloads/pdfs/E09%20-%20Carbon%20footprinting%20-%20metrics.pdf
#' 
#' @examples
#' portfolio_exposure     = data.table::data.table(Issuers = c('A','B','C'),
#'  exposures = c(100, 200, 50))
#' emissions_capitalization_revenue_data = data.table::data.table(Issuers = c('A','B','C'),
#'  emissions = c(1000, 5000, 6000), revenue = c(2000, 5000, 3000),Capitalization = 
#'  c(20000, 10000, 15000))
#' Carbon_Intensity (portfolio_exposure, emissions_capitalization_revenue_data)
Carbon_Intensity <- function(portfolio_exposure, emissions_capitalization_revenue_data)  
{
  merged_data = setkey(portfolio_exposure,'Issuers')[setkey(emissions_capitalization_revenue_data,'Issuers')]
  
  # that's the efficient way to do it - won't work with CRAN checks though...
  # merged_data[, weights=exposures/Capitalization]
  # return(sum(merged_data[,weights*emissions])/sum(merged_data[,weights*revenue]))
  merged_data$weights=merged_data$exposures/merged_data$Capitalization
  return(sum(merged_data$weights*merged_data$emissions)/sum(merged_data$weights*merged_data$revenue))
}