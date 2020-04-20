#' Calculates the sample entropy of a track record. Sample entropy is an improvement of the approximate entropy and should produce accurate results
#' for timeseries of smaller length like historical returns of strategies
#' @title Sample Entropy
#' @param returns a vector containing the track record of the underlying asset/strategy, these will be normalized during the algorithm
#' @param m an integer value defining the embedding dimension , default value is 2
#' @param r a  double  value defining the tolerance, default value is 0.2
#' @return The sample Entropy of the input returns
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://en.wikipedia.org/wiki/Sample_entropy
#'
#' @examples
#'
#' ## calling SampleEntropy() without an argument loads the historical edhec
#' ## data for the "Short Selling" strategy
#' returns = PerformanceAnalytics::edhec[,c("Short Selling")]
#' Sample_Entropy = SampleEntropy(returns,m=2,r=0.2)
#'
SampleEntropy = function(returns,m=2,r=0.2)
{

  if(missing(returns))
  {    returns = PerformanceAnalytics::edhec[,c("Short Selling")]
  }else
  {
    if(ncol(returns)!=1)   stop("Error: Just one column of returns is needed")
    if(!all(apply(returns,2,is.numeric))) stop("The input of returns have to be numeric (probably date column was included?)")
  }

  returns   = as.numeric(returns)
  TR_length = length(returns)

  normalized_returns = (returns-mean(returns))/sd(returns)

  normalized_returns_m  = list()
  normalized_returns_m1 = list()

  for(i in 1:(TR_length-m+1))
  {  normalized_returns_m[[i]] = normalized_returns[i:(i+m-1)] }

  for(i in 1:(TR_length-m))
  {  normalized_returns_m1[[i]] = normalized_returns[i:(i+m)]  }

  B = 0

  for(i in 1:(TR_length-m))
  {
    chebyshev_distances = lapply(normalized_returns_m[(i+1):(TR_length-m+1)], Chebyshev_distance,normalized_returns_m[[i]])
    B = B + sum(chebyshev_distances<r)
  }


  A = 0

  for(i in 1:(TR_length-m-1))
  {
    chebyshev_distances = lapply(normalized_returns_m1[(i+1):(TR_length-m)], Chebyshev_distance,normalized_returns_m1[[i]])
    A = A + sum(chebyshev_distances<r)
  }

  return(-log(A/B))
}
