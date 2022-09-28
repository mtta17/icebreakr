# epi.r
# mtta17@gmail.com
#
##### risk() #####
#
#' function desc one-liner
#'
#' more info
#'
#' @param df
#' @param exposure
#' @param outcome
#' @return stuff
#' @examples risk(linelist, pizza, salmonella)
#' @export
risk <- function(df, exposure, outcome){
    name <- deparse(substitute(exposure))
    out <- deparse(substitute(outcome))
    x <- df %>%
        tabyl(!!enquo(exposure), !!enquo(outcome)) %>%
        select(-1) %>%
        as.matrix() %>%
        unname() %>%
        riskratio.wald()
    tibble(exposure = name,
           RR = round(x$measure[2], 3),
           CI = paste0("[", round(x$measure[4], 3), ",", round(x$measure[6], 3), "]"),
           p_value = round(x$p.value[6], 3),
           interpretation = case_when(
               round(x$p.value[6], 3) >= .05 ~ "Not statistically significant",
               round(x$measure[2], 3) == 1 ~ paste0("The exposure [", name, "] had no effect on the outcome [", out, "]"),
               round(x$measure[2], 3) > 1 ~ paste0("Those that had the exposure [", name, "] are ", round(x$measure[2], 3), " times as likely to develop the outcome [", out, "] compared to those that did not have the exposure"),
               between(round(x$measure[2], 3), 0, 1) ~ paste0("Those that had the exposure [", name, "] are ", 1+(1-round(x$measure[2], 3)), " times less likely to develop the outcome [", out, "] compared to those that did not have the exposure"),
               T ~ "Error: undefined"
           )) %>%
        return()
} # risk()
