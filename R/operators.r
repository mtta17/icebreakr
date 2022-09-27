# operators.r
# mtta17@gmail.com
#
##### %notin% #####
#
#' Negation of %in% operator.
#'
#' Yeah, that's really all it is. This seems way more intuitive
#'
#' @return The complement of %in%
#' @examples 4 %notin% c(1, 2, 3)
#' [1] TRUE
#' @export
`%notin%` <- Negate(`%in%`)
