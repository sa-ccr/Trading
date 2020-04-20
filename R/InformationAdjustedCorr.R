#' Calculates the Information-Adjusted Correlation between the track records of various assets/strategies which covers for cases whereby the 'typical' Pearson's correlation
#' assumptions do not hold. The normalized cross sample entropy has been utilized for the mutual information estimation.  
#' @title Information Adjusted Correlation
#' @param x a vector containing the track record of the underlying asset/strategy (can be a data.table, data.frame, vector etc)
#' @param y a vector containing the track record of the underlying asset/strategy (can be a data.table, data.frame, vector etc)
#' @param m an integer value defining the embedding dimension for the sample entropy calculation, default value is 2
#' @param r a  double  value defining the tolerance for the sample entropy calculation, default value is 0.2
#' @return The information adjusted correlation
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://github.com/devisechain/Devise/blob/master/yellow_paper.pdf
#' 
#' @examples
#'
#' x = PerformanceAnalytics::edhec[,c("Short Selling")]
#' y = PerformanceAnalytics::edhec[,c("Convertible Arbitrage")]
#' Information_Adjusted_Corr = InformationAdjustedCorr(x, y, m=2, r=0.2)
#'
InformationAdjustedCorr = function(x, y, m=2, r=0.2)
{
  if(missing(x)|missing(y))
  {    
    x = PerformanceAnalytics::edhec[,"Short Selling"]
    y = PerformanceAnalytics::edhec[,"Convertible Arbitrage"]
  }
  
  MutualInformation = NormXASampEn(x, y, m=2, r=0.2)
  pearson_corr = cor(x, y)
  inf_adj_corr = sign(pearson_corr)*sqrt(1-2^(-2/(1/MutualInformation)))
  return(inf_adj_corr)
}