#' @description Returns the absolute greenhouse gas emissions associated with a portfolio, expressed in tons CO2e.
#' Under this approach, if an investor owns 5 percent of a company's total market capitalization, then the investor 
#' owns 5 percent of the company as well as 5 percent of the company's GHG (or carbon) emissions.
#'  
#' @title Total Carbon Emissions
#' @param portfolio_exposure The exposure per issuer in the portfolio
#' @param emissions_capitalization_data The capitalization and the Scope 1 & 2 GHG emissions per issuer
#' @return The absolute greenhouse gas emissions associated with a portfolio, expressed in tons CO2e
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://www.tcfdhub.org/Downloads/pdfs/E09%20-%20Carbon%20footprinting%20-%20metrics.pdf
#' 
#' @examples
#'  portfolio_exposure     = data.table::data.table(Issuers = c('A','B','C'),
#'  exposures = c(100, 200, 50))
#'  emissions_capitalization_data = data.table::data.table(Issuers = c('A','B','C'),
#'   emissions = c(1000, 5000, 6000),
#'   Capitalization = c(20000, 10000, 30000))
#'  Total_Carbon_Emissions(portfolio_exposure, emissions_capitalization_data)
Total_Carbon_Emissions <- function(portfolio_exposure, emissions_capitalization_data)  
{
  merged_data = setkey(portfolio_exposure,'Issuers')[setkey(emissions_capitalization_data,'Issuers')]
  
  # that's the efficient way to do it - won't work with CRAN checks though...
  #return(sum(merged_data[,exposures/Capitalization*emissions]))
  return(sum(merged_data$exposures/merged_data$Capitalization*merged_data$ emissions))
}