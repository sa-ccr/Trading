#' Returns all possible combinations of two dataframes
#' @title  Returns all possible combinations of two dataframes
#' @param df_a The first dataframe
#' @param df_b The second dataframe
#' @return A dataframe with all combinations
#' @export
#' 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @examples
#' 
#' df_a = data.frame(matrix(seq(1,20),nrow = 5, ncol = 4))
#' df_b = data.frame(matrix(seq(21,40),nrow = 5, ncol = 4))
#' joined_df = OuterJoinMerge(df_a, df_b)

OuterJoinMerge <- function(df_a, df_b)  {
  
  df_a_rep = do.call("rbind", replicate(nrow(df_b), df_a, simplify = FALSE))
  df_b_rep = do.call("rbind", replicate(nrow(df_a), df_b, simplify = FALSE))
  df = cbind(df_a_rep, df_b_rep)
  return(df)
}
