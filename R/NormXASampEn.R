#' Calculates the Normalized Cross Sample Entropy of the track records of two assets/strategies based on the sample entropy.
#' @title Normalized Cross Sample Entropy
#' @param x a vector containing the track record of the underlying asset/strategy, this will be normalized during the algorithm
#' @param y a vector containing the track record of the underlying asset/strategy, this will be normalized during the algorithm
#' @param m an integer value defining the embedding dimension , default value is 2
#' @param r a  double  value defining the tolerance, default value is 0.2
#' @return A value containing the NormXASampEn
#' @references Lopez de Prado, Marcos, Codependence (Presentation Slides) (January 2, 2020). Available at SSRN: https://ssrn.com/abstract=3512994
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#'
#' @examples
#'
#' x = PerformanceAnalytics::edhec[,c("Short Selling")]
#' y = PerformanceAnalytics::edhec[,c("Convertible Arbitrage")]
#' Normalized_Cross_Sample_Entropy = NormXASampEn(x, y, m=2, r=0.2)
#'
NormXASampEn = function(x, y, m=2, r=0.2)
{
  Cross_Sample_Entropy = CrossSampleEntropy(data.frame(x = x, y = y),m=2,r=0.2)

  sample_entropy_x = SampleEntropy(x,m=2,r=0.2)
  sample_entropy_y = SampleEntropy(y,m=2,r=0.2)

  return(Cross_Sample_Entropy/min(sample_entropy_x,sample_entropy_y))
}
