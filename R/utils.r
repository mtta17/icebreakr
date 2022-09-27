# utils.r
#
# **Description ---------------------------------------------------------------
# Additional user-defined functions for generalistic utility.
# author - mtta17@gmail.com
#
# ***********************************************
# ***************** thoughts ********************
# ***********************************************
#
# finish documentation you silly willy
#
# ***********************************************
#
# read_clippy()
# write_clippy()
#
# read_clippy ################################################################
# **Arguments -----------------------------------------------------------------
#
# **Output --------------------------------------------------------------------
#
# **Dependencies --------------------------------------------------------------
#
# write_clippy ################################################################
# **Arguments -----------------------------------------------------------------
#
# **Output --------------------------------------------------------------------
#
# **Dependencies --------------------------------------------------------------
#

read_clippy <- function(expected_result = "df"){
    if(expected_result == "df"){
        readr::read_delim(clipboard(), delim = "\t")
    } else if(result == "vector"){
        clipboard()
    } else{
        stop("Must choose desired output of \"df\" (default) or \"vector\"")
    }
}

write_clippy <- function(df, col_names = T){
    df %>%
        write.table("clipboard", sep = "\t", row.names = F, col.names = col_names, na = "")
}
