######################################################################
# Create the base option class
#
# This is used to represent the option products and it will be contained to all the Option-related classes

Option = setRefClass("Option",
                  fields = list(OptionType        = 'character',
                                UnderlyingPrice   = 'numeric',
                                StrikePrice       = 'numeric',
                                del_type          = 'character',
                                maturity          = 'numeric',
                                exotic_opt_type   = 'character'
                  ),
                  methods = list(
                    CalcMaturity =function()
                    {
                    if(del_type=='Cash')
                      return(maturity)
                    }
                  ))