# html_tables.r
#
# **Description ---------------------------------------------------------------
# A clean wrapper(s)(?) for creating a friendly HTML table in a markdown doc.
# author - mtta17@gmail.com
#
# ***********************************************
# ************** thoughts ***********************
# ***********************************************
#
# title quoting is still broken
#
# ***********************************************
#
# html_tables
#
# fancy_table ############################################################
# **Arguments -----------------------------------------------------------------
# df		data frame that you want as a table
# title		string to give a title to your table
# **Output --------------------------------------------------------------------
# A nice, formatted HTML table to insert into a document
# **Dependencies --------------------------------------------------------------
# DT, htmltools

#' @export
fancy_table <- function(df, title = quote()){
    df %>%
        DT::datatable(class = 'display compact nowrap',
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;', htmltools::h3(title)),
                      filter = "top",
                      extensions = 'Scroller',
                      options = list(
                          deferRender = TRUE,
                          scrollX = T,
                          scrollY = 400,
                          scroller = TRUE))
}
