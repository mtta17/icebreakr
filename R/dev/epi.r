# epi.r
# mtta17@gmail.com
#
##### tidy_risk() #####
#
#' function desc one-liner
#'
#' more info
#'
#' @param df
#' @param exposure
#' @param outcome
#' @return stuff
#' @examples tidy_risk(linelist, pizza, salmonella)
#' @export
tidy_risk <- function(df, exposure, outcome){
    name <- deparse(substitute(exposure))
    out <- deparse(substitute(outcome))
    x <- df %>%
        janitor::tabyl(!!dplyr::enquo(exposure), !!dplyr::enquo(outcome)) %>%
        dplyr::select(-1) %>%
        as.matrix() %>%
        unname() %>%
        epitools::riskratio.wald()
    dplyr::tibble(exposure = name,
           RR = round(x$measure[2], 3),
           CI = paste0("[", round(x$measure[4], 3), ",", round(x$measure[6], 3), "]"),
           p_value = round(x$p.value[6], 3),
           interpretation = dplyr::case_when(
               round(x$p.value[6], 3) >= .05 ~ "Not statistically significant",
               round(x$measure[2], 3) == 1 ~ paste0("The exposure [", name, "] had no effect on the outcome [", out, "]"),
               round(x$measure[2], 3) > 1 ~ paste0("Those that had the exposure [", name, "] are ", round(x$measure[2], 3), " times as likely to develop the outcome [", out, "] compared to those that did not have the exposure"),
               between(round(x$measure[2], 3), 0, 1) ~ paste0("Those that had the exposure [", name, "] are ", 1+(1-round(x$measure[2], 3)), " times less likely to develop the outcome [", out, "] compared to those that did not have the exposure"),
               T ~ "Error: undefined"
           )) %>%
        return()
} # tidy_risk()
# allow for multiple exposures and outcomes to avoid user having to purrr::map
# add the outcome to the output table as well
# force user to one hot encode the fields
