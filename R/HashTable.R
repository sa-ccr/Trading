#' Creates a hashtable-like object so as to represent data with a key structure (for example addon tables, rating-based factors etc).
#' Also, it includes methods for populating the object via a .csv file and finding a value based on a specific key on an interval of keys
#' For examples of the format of the CSVs files, please view RatingsMapping.csv or AddonTable.csv on the extdata folder in the installation folder of the library
#' @title  Hashtable Class
#' @param keys          A vector of keys
#' @param values        A vector of values mapping to the keys
#' @param keys_type     The type of the keys
#' @param values_type  The type of the values
#' @return An object of type HashTable
#' @export
#'
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#'
#' ## loading a ratings' mapping matrix from the extdata folder
#' rating_table = HashTable('RatingsMapping.csv',"character","numeric")
#'reg_weight =rating_table$FindValue("AAA")
#'
HashTable = setRefClass("HashTable",
                        fields = list(keys ="character",
                                      values  = "numeric",
                                      keys_type = "character",
                                      values_type = "character"
                        ),

                        methods = list(
                          initialize = function(csvfilename,keys_type,values_type) {

                            raw_data <- read.csv(system.file("extdata", csvfilename, package = "Trading"),header=TRUE,stringsAsFactors = FALSE,strip.white=TRUE)

                            keys <<- as.character(raw_data[,1])
                            perc = grep("%",raw_data[,2])
                            raw_data[perc,2] = sub("%","",raw_data[perc,2])
                            raw_data[,2]  = as.numeric(raw_data[,2] )
                            raw_data[perc,2] = raw_data[perc,2]/100
                            values <<- raw_data[,2]
                            keys_type <<- keys_type
                            values_type <<- values_type
                          }
                          ,
                          FindValue = function(key)
                          {
                            if(keys_type=="numeric")
                              keys_temp = as.numeric(keys)
                            else
                              keys_temp = keys

                            return(values[keys_temp==key])
                          }
                          ,
                          FindIntervalValue = function(value)
                          {
                            if(keys_type=="numeric")
                              keys_temp = as.numeric(keys)

                            return(values[which(keys_temp==keys_temp[findInterval(value,keys_temp)])+1])
                          }
                        )
)
