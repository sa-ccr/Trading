######################################################################
# Create the base Vol class
#
# This is used to represent the vol-based products and it will be contained to all the volatility-related classes

Vol = setRefClass("Vol",
                  fields = list(vol_strike   = "numeric",
                                annualization_factor = "numeric",
                                vega_notional = "numeric",
                                Underlying_Instrument = "character"
                  ),
                  methods = list(
                    ComputeVarianceUnits = function() {
                     return(vega_notional/(2*vol_strike))
                    }
                  ))