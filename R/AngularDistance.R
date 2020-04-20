#' Calculates the angular distance between a matrix of the track records of various assets/strategies. The sign of the correlation can be ignored for long/short portfolios.
#' @title Angular distance metrics
#' @param returns_matrix a matrix containing the track records of the underlying assets/strategies.
#' @param long_short a boolean value which results in the sign of the correlation being ignored, default value is FALSE
#' @return A matrix containing the angular distance values.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Lopez de Prado, Marcos, Codependence (Presentation Slides) (January 2, 2020). Available at SSRN: https://ssrn.com/abstract=3512994
#'
#' @examples
#'
#' ## calling AngularDistance() without an argument loads the historical edhec data
#' ##  for the "Short Selling" and "Convertible Arbitrage" strategies
#' returns_matrix = PerformanceAnalytics::edhec[,c("Short Selling","Convertible Arbitrage")]
#' angular_distance = AngularDistance(returns_matrix, long_short=FALSE)
#'
AngularDistance = function(returns_matrix, long_short=FALSE)
{

  if(missing(returns_matrix))
  {    returns_matrix = PerformanceAnalytics::edhec[,c("Short Selling","Convertible Arbitrage")]
  }else
  {
       if(ncol(returns_matrix)<2)   stop("Error: You need to provide at least two vectors of returns")
       if(!all(apply(returns_matrix,2,is.numeric))) stop("The input of returns have to be numeric (probably date column was included?)")
  }
  corr_matrix = cor(returns_matrix)

  if(long_short)    corr_matrix = abs(corr_matrix)

  return(sqrt((1-corr_matrix)/2))
}
