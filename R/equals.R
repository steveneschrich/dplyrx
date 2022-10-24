#' @describeIn equals Operator for equality
#' @export
`%==%` <- function(a,b) {

  equals(a,b)

}


#' Vector equality assuming NA matches a value
#'
#' Provides an alternate equality assuming NA is equal to anything,
#' including another NA.
#'
#' @param a Vector to compare
#' @param b Vector to compare
#'
#' @return Logical if `a` and `b` are equal, assuming NA is equal to anything.
#' @export
#'
#' @examples
#' \dontrun{
#' c(1,2,3) %==% c(1,NA,3)
#' # [1] TRUE TRUE TRUE
#' }
equals <- function(a,b) {
  # Equality is:
  #  - na (either or both)
  #  - equal
  (is.na(a) | is.na(b)) | (a == b)
}
