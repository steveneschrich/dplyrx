
#' Identify duplicated column names from a join
#'
#' @description Identifies column names that exist as ending in elements
#' of `suffix` (default is `.x` and `.y`). If the variable is duplicated
#' and has a suffix, this suggests that they had the same column name
#' during a dplyr join.
#'
#' @details The purpose of this function is to identify column names that
#' have been joined from two tables, but have the same name. By default,
#' [dplyr::left_join()] and the like will add a `.x` and `.y` to the
#' name. Often, I've had the problem that I cannot join by them (because
#' they are incomplete). So I'd rather clean them up after the join (using
#' meld). This function identifies the column names that need to be cleaned
#' up.
#'
#' @param x A data frame
#' @param suffix A list of suffixes to consider (default is c(".x",".y"))
#'
#' @return A named list of column names. The names represent the unique base in
#' common. The values of the list represent the individual (collisions) column names.
#' @export
#'
#' @examples
#' \dontrun{
#' identify_collisions(data.frame(A.x=c(1,2,3), A.y=c(2,3,4)), suffix=c(".x",".y"))
#' # $A
#' # [1] "A.x" "A.y"
#' }
identify_collisions<-function(x, suffix=c(".x",".y")) {

  stopifnot("x is not a data frame!"=is.data.frame(x))

  # The implementation operates on the column names of the data frame
  # as a data frame itself, since there are several convenient routines
  # to use.  This could also be done with vctrs and stringr only, but
  # at present this works.
  collisions <- colnames(x) |>
    tibble::enframe() |>
    # Operate rowwise, since we have a vector of values (colnames) and a
    # vector of suffixes.
    dplyr::rowwise() |>
    # Filter to only those with a suffix
    dplyr::filter(any(endsWith(.data$value, suffix))) |>
    # Record the suffix used
    dplyr::mutate(suffix = suffix[endsWith(.data$value, suffix)]) |>
    # Trim off the suffix
    dplyr::mutate(base = stringr::str_sub(.data$value, 1, stringr::str_length(.data$value) - stringr::str_length(suffix))) |>
    # Convert into var  = c(collide1, collide2)
    dplyr::select(.data$base, .data$value) |>
    dplyr::group_by(.data$base) |>
    tidyr::chop(.data$value)|>
    tibble::deframe() |>
    as.list()

  # Special case of empty list.
  if ( length(collisions) == 0 ) return(NULL)

  collisions
}





#' Coalesce columns that collided if possible
#'
#' @description When joining two data frames, columns of the same name will
#' be renamed as `.x` and `.y`. These collisions can be coalesced into a
#' single column provided they do not conflict. This function performs that
#' operation.
#'
#' @param x A data frame with collisions
#' @param suffix Suffix values expected for collisions (not all suffixes need to be
#' in the collisions).
#'
#' @return A data frame with all possible collision columns coalesced. Remaining
#' columns that collided are kept as is.
#' @export
#'
#' @examples
coalesce_collisions <- function(x,  suffix=c(".x",".y")) {

  stopifnot(length(x) == length(suffix))

  collisions<-identify_collisions(x, suffix)

  # Coalesce the variables (where possible).  Note this package has a coalesce, not
  # the default dplyr coalescce.
  x <- coalesce_data_frame(x, !!!collisions, warn_only = TRUE, pick_first=FALSE)

  x
}

