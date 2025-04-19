#' Calculates the Information-Adjusted Beta between the track records of two assets/strategies which covers for cases whereby the 'typical' linearity and Gaussian I.I.D
#' assumptions do not hold. The normalized cross sample entropy has been utilized for the mutual information estimation. 
#' @title Information Adjusted Beta
#' @param x a vector containing the track record of the underlying asset/strategy (can be a data.table, data.frame, vector etc)
#' @param y a vector containing the track record of the underlying asset/strategy (can be a data.table, data.frame, vector etc)
#' @param m an integer value defining the embedding dimension for the sample entropy calculation, default value is 2
#' @param r a  double  value defining the tolerance for the sample entropy calculation, default value is 0.2
#' @return The information adjusted Beta
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://github.com/devisechain/Devise/blob/master/yellow_paper.pdf
#' 
#' @examples
#'
#' x = PerformanceAnalytics::edhec[,c("Short Selling")]
#' y = PerformanceAnalytics::edhec[,c("Convertible Arbitrage")]
#' Information_Adjusted_Beta = InformationAdjustedBeta(x, y, m=2, r=0.2)
#'
InformationAdjustedBeta = function(x, y, m=2, r=0.2)
{
  inf_adj_beta = InformationAdjustedCorr(x, y, m=2, r=0.2)*sqrt(var(x)/var(y))
  return(inf_adj_beta)
}