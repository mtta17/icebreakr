# html_tables.r
# mtta17@gmail.com
#
##### fancy_table() #####
#
#' Format tabular data into a nice interactive HTML table.
#'
#' This version supports column filters, a search bar, and a scrollable Y to flexibly fit in most Markdown documents.
#'
#' @param df Dataframe of the data you want fancied
#' @param title String of the title you want to give to your table
#' @return A formatted, interactive HTML table
#' @examples fancy_table(mpg, title = "My Nice HTML Table")
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
# quoting is still messed up
