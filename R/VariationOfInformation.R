#' Calculates the variation of information of the track records of two assets/strategies based on the sample entropy.
#' @title Variation of Information
#' @param x a vector containing the track record of the underlying asset/strategy, this will be normalized during the algorithm
#' @param y a vector containing the track record of the underlying asset/strategy, this will be normalized during the algorithm
#' @param m an integer value defining the embedding dimension , default value is 2
#' @param r a  double  value defining the tolerance, default value is 0.2
#' @param normalized a boolean value so as to bound the return value between 0 and 1, default value is TRUE
#' @return A value containing the variation of information
#' @references Lopez de Prado, Marcos, Codependence (Presentation Slides) (January 2, 2020). Available at SSRN: https://ssrn.com/abstract=3512994
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#'
#' @examples
#'
#' x = PerformanceAnalytics::edhec[,c("Short Selling")]
#' y = PerformanceAnalytics::edhec[,c("Convertible Arbitrage")]
#' variation_of_information = VariationOfInformation(x, y, m=2, r=0.2, normalized = TRUE)
#'
VariationOfInformation = function(x, y, m=2, r=0.2, normalized = TRUE)
{
  Cross_Sample_Entropy = CrossSampleEntropy(data.frame(x = x, y = y),m=2,r=0.2)

  sample_entropy_x = SampleEntropy(x,m=2,r=0.2)
  sample_entropy_y = SampleEntropy(y,m=2,r=0.2)

  VariationOfInformation = sample_entropy_x + sample_entropy_y - 2 * Cross_Sample_Entropy  # Variation of information

  if(normalized)
  {
    joint_dist = sample_entropy_x + sample_entropy_y - Cross_Sample_Entropy
    VariationOfInformation = VariationOfInformation/joint_dist
  }
  return(VariationOfInformation)
}
