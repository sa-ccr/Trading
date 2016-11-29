#' Creates a Curve Object containing pairs of Tenors with relevant rates and the interpolation function.
#' Also, methods for populating the object via a .csv file and the generation of the interpolation function via cubic splines are included.
#' @title  Curve Class
#' @param Tenors          The Tenors of the curve
#' @param Rates           The rates on the corresponding tenors
#' @param interp_function (Optional) The interpolation function of the curve. Can be populated via the 'CalcInterpPoints' method
#' @return An object of type Curve
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples 
#' 
#' ## generating a curve either directly or through a csv - 
#' ## the spot_rates.csv file can be found on the extdata folder in the installation library path
#' funding_curve =  Curve(Tenors=c(1,2,3,4,5,6,10),Rates=c(4,17,43,47,76,90,110))
#' spot_rates = Curve()
#' spot_rates$PopulateViaCSV('spot_rates.csv')
#' time_points = seq(0,5,0.01)
#' spot_curve     = spot_rates$CalcInterpPoints(time_points)

Curve = setRefClass("Curve",
                  # the timebuckets grouping is only relevant for IRDs
                  fields = list(Tenors = "numeric",
                                Rates  = "numeric",
                                interp_function = "function"
                                ),

                  methods = list(
                    CalcInterpPoints = function(time_points)
                    {
                      interp_function <<- splinefun(Tenors, Rates, method="natural")
                      return(interp_function(time_points))
                    },
                    PopulateViaCSV = function(csvfilename)
                    {
                      raw_data <- read.csv(system.file("extdata", csvfilename, package = "Trading"),header=TRUE,stringsAsFactors = FALSE,strip.white=TRUE)
                      Tenors <<- as.numeric(raw_data[,1])
                      Rates <<- as.numeric(raw_data[,2])/100
                    }
                  )
                  )
