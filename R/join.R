#' Join a list of tables together on a common key
#'
#' @description Join a list of tables using `which` join method, by a common
#' `key` value. If variables exist in common across tables, the `suffix` list
#' will be appended to the column name. The result is a joined table from across
#' all list elements of `x`.
#'
#' @param x A list of data frames
#' @param by A join key (must be common across all tables)
#' @param which The type of join (left, right, inner, full). See [dplyr::left_join()].
#' @param suffix A list of suffixes to add to column names that collide between tables.
#' @param ... Any other parameters for the join function.
#'
#' @return A data frame with all tables joined on `by`
#' @export
#'
#' @examples
#' \dontrun{
#' list_join(list(iris,iris), by="Species", which="full", suffix=c(".1",".2"))
#' }
list_join <- function(x, by = NULL, which="full", suffix=NULL, ...) {

  args <- rlang::list2(...)

  # If no suffixes provided, we need our own version of .x, .y. But
  # for an arbitrary number of entries.
  if ( is.null(suffix) )
    suffix <- paste0(".t",1:length(x))

  # Filter out any NULL values in the list
  null_tables <- purrr::map_lgl(x,is.null)
  if (any(null_tables)) {
    suffix <- suffix[!null_tables]
    x <- purrr::compact(x)
  }

  # Setup which join to use
  join_fun <- switch(
    which,
    full = dplyr::full_join,
    left = dplyr::left_join,
    right = dplyr::right_join,
    inner = dplyr::inner_join
  )

  # Rename colnames if collisions could occur.
  x <- rename_common_columns(x, suffix = suffix, ignore = by)

  # Join on all columns
  newx <- purrr::reduce(x, ~join_fun(.x, .y, by = by, !!!args))

  # Cleanup collision columns where possible
  newx <- coalesce_collisions(newx, suffix)

  newx

}

#' @describeIn list_join Include all rows from cumulatively joined table
#' @export
left_mjoin <- function(...) {
  list_join(..., which="left")
}
#' @describeIn list_join Include all rows from each additional table
#' @export
right_mjoin <- function(...) {
  list_join(..., which="right")
}

#' @describeIn list_join Include rows in common between all tables
#' @export
inner_mjoin <- function(...) {
  list_join(..., which="inner")
}

#' @describeIn list_join Include rows in any table
#' @export
full_mjoin <- function(...) {
  list_join(..., which="full")
}


#' Left join with coalesce of column collisions
#'
#' When joining two tables, collisions can occur and the column names will
#' be appended with a suffix. This function then tries to re-combine these
#' columns based on equality or NA.
#'
#' @param x The first table
#' @param y The second table
#' @param by A join variable (see [dplyr::left_join()])
#' @param suffix A two-element list of suffixes to append if colnames collide.
#' @param ... Any other parameters (see [dplyr::left_join()])
#'
#' @return A data frame representing the join of the two parameters, followed by an attempt
#'  to coalesce columns that collided (same column name). If they cannot be coalesced, they
#'  are left as is with a warning printed.
#' @export
#'
#' @examples
left_join <- function(x, y, by=NULL, suffix=c(".x",".y"), ...) {

  newx <- dplyr::left_join(x = x,y = y,by = by,suffix = suffix, ...)

  coalesce_collisions(newx, suffix)
}
#' @describeIn left_join Right join
#' @export
right_join <- function(x, y, by=NULL, suffix=c(".x",".y"), ...) {

  newx <- dplyr::right_join(x = x,y = y,by = by,suffix = suffix, ...)

  coalesce_collisions(newx, suffix)
}
#' @describeIn left_join Inner join
#' @export
inner_join <- function(x, y, by=NULL, suffix=c(".x",".y"), ...) {

  newx <- dplyr::inner_join(x = x,y = y,by = by,suffix = suffix, ...)

  coalesce_collisions(newx, suffix)
}
#' @describeIn left_join Full join
#' @export
full_join <- function(x, y, by=NULL, suffix=c(".x",".y"), ...) {

  newx <- dplyr::full_join(x = x,y = y,by = by,suffix = suffix, ...)

  coalesce_collisions(newx, suffix)
}

