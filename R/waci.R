#' @description Returns the portfolio's exposure to each issuer expressed in tons CO2e / $M revenue.
#' Scope 1 and Scope 2 GHG emissions are allocated based on portfolio weights (the current value of
#' the investment relative to the current portfolio value), rather than the equity ownership approach                                                               
#'  
#' @title Weighted Average Carbon Intensity
#' @param portfolio_exposure The exposure per issuer in the portfolio
#' @param emissions_revenue_data The capitalization, revenue and the Scope 1 & 2 GHG emissions per issuer
#' @return Total carbon emissions for a portfolio normalized by the market value of the portfolio, expressed in tons CO2e / $M invested.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://www.tcfdhub.org/Downloads/pdfs/E09%20-%20Carbon%20footprinting%20-%20metrics.pdf
#' 
#' @examples
#'  portfolio_exposure     = data.table::data.table(Issuers = c('A','B','C'),
#'   exposures = c(100, 200, 50))
#'  emissions_revenue_data = data.table::data.table(Issuers = c('A','B','C'), 
#'  emissions = c(1000, 5000, 2000),
#'   revenue = c(2000, 5000, 3000))
#'  Weighted_Average_Carbon_Intensity(portfolio_exposure, emissions_revenue_data)
Weighted_Average_Carbon_Intensity <- function(portfolio_exposure, emissions_revenue_data)  
{
  portfolio_exposure$weights = portfolio_exposure$exposures/sum(portfolio_exposure$exposures)
  merged_data = setkey(portfolio_exposure,'Issuers')[setkey(emissions_revenue_data,'Issuers')]
  #   that's the efficient way to do it - won't work with CRAN checks though...
  #   portfolio_exposure[,weights:= exposures/sum(exposures)]
  #  return(sum(merged_data[,weights*emissions/revenue]))
  return(sum(merged_data$weights*merged_data$emissions/merged_data$revenue))
  
}