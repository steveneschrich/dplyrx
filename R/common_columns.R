#' Find common colnames between data frames
#'
#' @description Given data.frames (as parameters), check what column names
#' are in common among them.
#'
#' @param tbls A list of data.frames
#' @param ignore A list of column names to ignore when finding common names
#'
#' @return A character vector of column names that are in common between
#'   data frame.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' find_common_colnames(iris,iris)
#' #[1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
#' }
find_common_colnames <- function(tbls=NULL, ignore = NULL) {

  # If empty, return empty
  if ( is.null(tbls) ) return(NULL)
  # If a data frame (not a list), wrap it so the function works.
  if ( is.data.frame(tbls) )  tbls <- list(tbls)

  if (! all(purrr::map_lgl(tbls, ~is.data.frame(.x)||is.null(.x)))) {
    stop(
      "One of the arguments is not a data frame.\n",
      "Check if subsetting a table that drop=F is included.")
  }

  # This might be more complicated than needed, but converting
  # to a data frame allows us to do some nice arranging of information.
  common_fields <- purrr::map(tbls, colnames) |>
    tibble::enframe(name = "data_frame", value="colname") |>
    tidyr::unchop(.data$colname) |>
    dplyr::group_by(.data$colname) |>
    dplyr::filter(dplyr::n()>1) |>
    dplyr::filter(!.data$colname %in% ignore) |>
    tidyr::chop(.data$data_frame) |>
    tibble::deframe() |>
    as.list()

  # A tweak, to return a simple empty list if there are no common
  # fields
  if ( length(common_fields) == 0 )
    common_fields <- unname(common_fields)
  #cols <- purrr::map(tbls, ~colnames(.x)) |> unlist() |> unname()

  #common_fields <- unique(cols[vctrs::vec_duplicate_detect(cols)])

  #common_fields <- setdiff(common_fields, ignore)

  common_fields

}


#' Return colnames common among a list of data frames
#'
#' @param tbls A list of data frames
#' @param ignore colnames to be ignored in common comparison
#'
#' @return A character vector of common colnames, in more than one
#'  data frame of `tbls`.
#' @export
#'
#' @examples
#' \dontrun{
#' common_colnames(list(iris,iris))
#' # [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
#' }
common_colnames <- function(tbls = NULL, ignore=NULL) {
  cn <- find_common_colnames(tbls=tbls, ignore=ignore)
  names(cn)
}

#' Detect duplicate colnames in a data frame
#'
#' @param .x A data frame
#'
#' @return A vector of named logical values indicating if colname
#' is duplicated in data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' detect_duplicate_colnames(iris)
#' # Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species
#' # FALSE        FALSE        FALSE        FALSE        FALSE
#' }
detect_duplicate_colnames <- function(.x) {
  common <- find_common_colnames(list(.x))
  stats::setNames(colnames(.x) %in% names(common), colnames(.x))
}

#
#' Rename potential collisions in column names for a list of data frames
#'
#' @description When considering a list of data frames, some of the column
#'  names may be identical. If combining the data frames, there will be
#'  collisions (same name). This function takes a list and add suffixes to
#'  potential collision column names, to avoid this problem.
#'
#' @param tbls A list of tables
#' @param suffix A list of suffixes to add to colnames of a table (if a collision
#' occurs). Default is c(".t1",".t2",...)
#' @param ignore A list of colnames to ignore (e.g., keys for later joining)
#'
#' @return A list of tables with the colnames appended with suffixes where needed
#' to avoid collisions.
#' @export
#'
#' @examples
#' \dontrun{
#' colnames(rename_collisions(list(iris, iris), suffix=c(".i1",".i2"))[[1]])
#' # [1] "Sepal.Length.i1" "Sepal.Width.i1"  "Petal.Length.i1" "Petal.Width.i1"
#' # [5] "Species.i1"
#' }
rename_common_columns <- function(tbls, suffix=NULL, ignore=NULL) {

  stopifnot("Table list does not contain data frames!"=all(purrr::map_lgl(tbls, ~is.data.frame(.x)|is.null(.x))))
  if ( is.null(suffix) )
    suffix <- paste0(".t",1:length(tbls))

  stopifnot(length(tbls)==length(suffix))
  common_fields <- common_colnames(tbls, ignore=ignore)


  tbls <- purrr::map2(tbls, suffix, function(.x, .y) {
    collisions_in_tbl <- which(colnames(.x) %in% common_fields)
    if (length(collisions_in_tbl) > 0) {
      colnames(.x)[collisions_in_tbl] <- paste0(colnames(.x)[collisions_in_tbl],.y)
    }
    .x
  })
  tbls
}
