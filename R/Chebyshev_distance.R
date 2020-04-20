#' Calculates the Chebyshev distance 
#' @title Chebyshev distance
#' @param x a vector containing the track record of the underlying asset/strategy
#' @param y a vector containing the track record of the underlying asset/strategy
#' @return The Chebyshev distance of the two vectors
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://en.wikipedia.org/wiki/Chebyshev_distance
#'
#' @examples
#'
#' x = rnorm(1000)
#' y = rnorm(1000)
#' 
#' chebyshev_dist = Chebyshev_distance(x, y)
#'
Chebyshev_distance = function(x,y)
{  return(max(abs(x-y))) }