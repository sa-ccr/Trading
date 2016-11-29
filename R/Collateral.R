#' Creates a Collateral amount object which needs to be linked with a CSA ID
#' @title  Collateral Class
#' @param ID The ID of each object
#' @param  Amount   The collateral amount
#' @param csa_id   The csa_id that this object is linked with
#' @param type     Describes the type of the collateral: can be "ICA", "VariationMargin" etc
#' @return An object of type Collateral
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
#' @examples
#'
#'
#' colls = list()
#'coll_raw = read.csv(system.file("extdata", "coll.csv", package = "Trading"),header=TRUE,
#'stringsAsFactors = FALSE)
#'
#'for(i in 1:nrow(coll_raw))
#'{
#'  colls[[i]] = Collateral()
#'  colls[[i]]$PopulateViaCSV(coll_raw[i,])
#'}


Collateral = setRefClass("Collateral",

                         fields = list(ID = "character",
                                       Amount   = "numeric",
                                       csa_id = "character",
                                       type = "character"
                         ),
                         methods = list(
                           PopulateViaCSV = function(coll_raw)
                           {
                             if(missing(coll_raw))
                             {
                               raw_data <- read.csv(system.file("extdata", 'coll.csv', package = "Trading"))
                             }else
                             {
                               raw_data <- coll_raw
                             }

                             non_numeric= c("csa_id","type","TradeGroups","ID")

                             names_raw_data = names(raw_data)

                             for(i in 1:length(names_raw_data))
                             {
                               if(names_raw_data[i] %in% non_numeric) {
                                 temp_value = paste0("'",as.character(eval(parse(text=paste0("raw_data$",names_raw_data[i])))),"'")
                               }else
                               { temp_value = as.numeric(eval(parse(text=paste0("raw_data$",names_raw_data[i]))))}

                               eval(parse(text=paste(names_raw_data[i],"<<-",temp_value)))
                             }
                           }


)
)
