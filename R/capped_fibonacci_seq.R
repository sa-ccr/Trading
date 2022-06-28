#' @description Generates the Fibonacci sequence up to a specified maximum number
#'  
#' @title Fibonacci sequence up to a specified maximum number
#' @param max_number The maximum number up to which the sequence should be generated
#' @return A vector containing the Fibonacci sequence
#' 
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://en.wikipedia.org/wiki/Fibonacci_number
#' 
#' @examples
#' 
#' fibonacci_seq = capped_fibonacci_seq(max_number = 6000)
 
capped_fibonacci_seq <- function(max_number)  {
  
  sequence = c(0, 1)
  max_length = 2

  while (sequence[max_length]+sequence[max_length-1] < max_number) {
    sequence = c(sequence,sequence[max_length-1]+sequence[max_length])
    max_length = max_length + 1
  }
  
  return(sequence)
}
