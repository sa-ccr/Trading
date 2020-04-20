#' Calculates the cross sample entropy between two track records of various assets/strategies.
#' @title Angular distance metrics
#' @param returns_matrix a matrix containing the track records of the underlying assets/strategies. These will be normalized during the algorithm
#' @param m an integer value defining the embedding dimension , default value is 2
#' @param r a  double  value defining the tolerance, default value is 0.2
#' @return The value of cross sample entropy
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://physoc.onlinelibrary.wiley.com/doi/epdf/10.1113/expphysiol.2007.037150
#'
#' @examples
#'
#' ## calling CrossSampleEntropy() without an argument loads the historical edhec data
#' ## for the "Short Selling" and "Convertible Arbitrage" strategies
#' returns_matrix = PerformanceAnalytics::edhec[,c("Short Selling","Convertible Arbitrage")]
#' Cross_Sample_Entropy = CrossSampleEntropy(returns_matrix,m=2,r=0.2)
#'
CrossSampleEntropy = function(returns_matrix,m=2,r=0.2)
{

  if(missing(returns_matrix))
  {    returns_matrix = PerformanceAnalytics::edhec[,c("Short Selling","Convertible Arbitrage")]
  }else
  {
    if(ncol(returns_matrix)!=2)   stop("Error: You need to provide at least two vectors of returns")
    if(!all(apply(returns_matrix,2,is.numeric))) stop("The input of returns have to be numeric (probably date column was included?)")
  }

  x = as.numeric(returns_matrix[,1])
  y = as.numeric(returns_matrix[,2])
  TR_length = nrow(returns_matrix)

  normalized_x = (x-mean(x))/sd(x)
  normalized_y = (y-mean(y))/sd(y)

  normalized_x_m  = list()
  normalized_y_m  = list()
  normalized_x_m1 = list()
  normalized_y_m1 = list()

  for(i in 1:(TR_length-m+1))
  {
    normalized_x_m[[i]] = normalized_x[i:(i+m-1)]
    normalized_y_m[[i]] = normalized_y[i:(i+m-1)]
  }

  for(i in 1:(TR_length-m))
  {
    normalized_x_m1[[i]] = normalized_x[i:(i+m)]
    normalized_y_m1[[i]] = normalized_y[i:(i+m)]
  }

  temp_sum = 0

  for(i in 1:(TR_length-m+1))
  {
    chebyshev_distances = lapply(normalized_y_m, Chebyshev_distance,normalized_x_m[[i]])

    temp_sum = temp_sum + sum(chebyshev_distances<0.2)/(TR_length-m+1)
  }

  B = temp_sum/(TR_length-m+1)

  temp_sum = 0

  for(i in 1:(TR_length-m))
  {
    chebyshev_distances = lapply(normalized_y_m1, Chebyshev_distance,normalized_x_m1[[i]])

    temp_sum = temp_sum + sum(chebyshev_distances<0.2)/(TR_length-m)
  }

  A = temp_sum/(TR_length-m)

  return(-log(A/B))
}
