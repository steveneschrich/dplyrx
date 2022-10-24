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

  # Setup which join to use
  join_fun <- switch(
    which,
    full = dplyr::full_join,
    left = dplyr::left_join,
    right = dplyr::right_join,
    inner = dplyr::inner_join
  )

  # Record collisions for coalescing
  collisions <- find_collisions(!!!x, ignore = by)

  # Rename colnames if collisions could occur.
  x <- rename_collisions(x, suffix = suffix, ignore = by)

  # Join on all columns
  newx <- purrr::reduce(x, ~join_fun(.x, .y, by = by, !!!args))

  # Cleanup collision columns where possible
  newx <- coalesce_collisions(newx, collisions, suffix)

  newx

}

#' @describeIn list_join Include all rows from cumulatively joined table
#' @export
left_joinl <- function(...) {
  list_join(..., which="left")
}

# This is to do the coalesce afterwards, I tink,
left_join <- function(x, y, by=NULL, ...) {

}

#' @describeIn list_join Include all rows from each additional table
#' @export
right_joinl <- function(...) {
  list_join(..., which="right")
}

#' @describeIn list_join Include rows in common between all tables
#' @export
inner_joinl <- function(...) {
  list_join(..., which="inner")
}

#' @describeIn list_join Include rows in any table
#' @export
full_joinl <- function(...) {
  list_join(..., which="full")
}
