# utils.r
# mtta17@gmail.com
#
##### read_clippy() #####
#
#' Convenient wrapper to get data from your clipboard into your R session.
#'
#' Works well when needing to quickly move small data from Excel into R.
#'
#' @param expected_result String of desired data type to be read into session. Currently, only `df` and `vector` are supported. Defaults to `df`
#' @return A dataframe or vector, depending on dimension of the data being read in
#' @examples x <- read_clippy()
#' @export
read_clippy <- function(expected_result = "df"){
    if(expected_result == "df"){
        readr::read_delim(clipboard(), delim = "\t")
    } else if(result == "vector"){
        clipboard()
    } else{
        stop("Must choose desired output of \"df\" (default) or \"vector\"")
    }
} # read_clippy()

##### write_clippy() #####
#
#' Transfer R data from your environment to your clipboard.
#'
#' Works well when needing to quickly move small data from R into Excel.
#'
#' @param df Tabular data that you want to write to clipboard
#' @param col_names Bool. Do you want column names written to the clipboard as well? Defaults to TRUE
#' @return Clipboard buffer updates with the data passed to this function.
#' @examples x <- mpg
#' write_clippy(x)
#' @export
write_clippy <- function(df, col_names = T){
    df %>%
        write.table("clipboard", sep = "\t", row.names = F, col.names = col_names, na = "")
} # write_clippy()
