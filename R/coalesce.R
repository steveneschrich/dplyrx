#' @describeIn vec_coalesce Binary operator for two vectors
#' @export
`%^%` <- function(a,b) {
  vec_coalesce(a,b)
}

#' Coalesce two vectors, warning if unequal
#'
#' @description The operator [dplyr::coalesce()] is useful for combining
#' vectors, however if the values do not agree then coalesce use the first
#' non-NA result. This function will print a warning if there
#' are value mismatches.
#'
#' @param a Vector to meld
#' @param b Vector to meld
#' @param pick_first Logical. Should the coalesce approach (select the first non-NA) be used? The
#' function will warn if there are unequal values, but use the first non-NA.
#'
#' @return A vector melding a and b (see [dplyr::coalesce()]).
#' @export
#'
#' @examples
#' \dontrun{
#' c("A","B","C") %^% c("A",NA,"C")
#' # [1] "A" "B" "C"
#' }
vec_coalesce <- function(a, b, pick_first=FALSE) {
  if (! all(a %==% b) ) {
    msg <- "vec_coalesce: vectors are not equivalent."
    if ( pick_first ) {
      msg <- paste0(msg, " Using default coalesce behavior (pick first non-NA).")
    } else {
      msg <- paste0(msg, " Default coalesce behavior not specified, cannot continue.")
    }
    logger::log_warn(msg)
    if ( !pick_first )
      stop(msg)
  }
  # Merging involves picking the non-NA value (or using NA)
  dplyr::coalesce(a,b)
}


#' Coalesce variables in data frame
#'
#' Combine variables in a data frame into a single, new variable (coalesce) with warnings
#' unless the fields are equal (or one is NA).
#'
#' @param x A data frame to operate on.
#' @param ... Named lists (e.g., `new1=c("old1","old2)`). Specifies the new variable name (`new1`) that
#'   results from coalescing the two old variables (`old1` and `old2`). List of old variables can be
#'   more than 2.
#' @param remove Logical, should the original variables be removed from the data frame (default is TRUE).
#' @param drop Logical. Should mismatched pairs be dropped (changes number of rows)? Otherwise,
#' a warning is printed.
#' @param pick_first Logical. Should the coalesce approach (select the first non-NA) be used? The
#' function will warn if there are unequal values, but use the first non-NA.
#' @param warn_only Logical. Should a mismatch trigger a warning (TRUE) or stop (FALSE).
#'
#' @return A data frame with the old variables coalesced into new variables and old variables removed (assuming
#' `remove=TRUE`).
#' @export
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' coalesce(data.frame(a=c(1,2,3), b=c(NA,2,3)), new=c("a","b"))
#' #   new
#' # 1   1
#' # 2   2
#' # 3   3
#' }
coalesce_data_frame <- function(x, ..., remove = TRUE, drop = FALSE, pick_first=TRUE, warn_only=TRUE) {

   # The input can be a series of newname = c(oldvalue1, oldvalue2).
   fields <- rlang::list2(...)
   cols <- colnames(x)

   # Coalesce each new field (names)
   for (f in names(fields)) {
     x <- coalesce_columns(x, f, fields[[f]], remove=remove, drop=drop, warn_only=warn_only, pick_first=pick_first)

   }

   # This selects the original columns in the original order.
   x <- dplyr::select(x, dplyr::any_of(cols), dplyr::any_of(names(fields)), dplyr::everything())

   x
}


#' Coalesce columns of data frame into a single new column
#'
#' @note The default operation of this function should mirror [dplyr::coalesce()].
#' @param x A data frame to operate on
#' @param new_var A new variable name for the coalesced fields (can reuse existing variable name)
#' @param var_list A vector of variable names to try and coalesce
#' @param remove Logical (T). Should the old variables be removed from the data frame?
#' @param drop Logical (F). Should mismatches in columns force dropping the associated rows?
#' @param warn_only (T). Should mismatches prompt a warning, but not an error?
#' @param pick_first (T). Should default coalesce behavior be used (pick the first non-NA)? Note that for
#'   `warn_only` to work, the `pick_first` option must be true.
#'
#' @return A data frame with the specified columns coalesced into a single, new column. Note that flags
#'   will change this default result slightly.
#' @export
#'
#' @examples
coalesce_columns <- function(x, new_var, var_list, remove = TRUE, drop=FALSE, warn_only=TRUE, pick_first=TRUE) {

  var_list <- intersect(var_list, colnames(x))

  new_column <- rep(NA, nrow(x))
  if ( length(var_list) <= 0) return(new_column)

  for (v in 1:length(var_list)) {
    # Provide specific warning if field cannot be combined.
    equivalent <- new_column %==% x[[var_list[v]]]

    # This is where our coalesce differs, when it's not equivalent.
    if (! all(equivalent)) {
      msg <- glue::glue("Fields are not equivalent so cannot combine correctly: {paste(var_list,collapse=', ')}.")
      logger::log_warn(msg)

      # Options:
      # 1: Stop if not warn-only
      # 2: Drop rows
      # 2: Pick first non-NA
      if ( ! warn_only ) {
        stop(msg)
      }

      # If drop, subset everything.
      if ( drop ) {
        logger::log_warn("{glue::glue(msg)}\nDropping mismatched rows (drop = TRUE).")
        x <- dplyr::filter(x, equivalent)
        new_column <- new_column[equivalent]
        equivalent <- equivalent[equivalent]
      } else if (pick_first) {
        logger::log_warn("{glue::glue(msg)}\nReverting to coalesce (first non-NA value used).")
      }
    }

    # Now we know if the two vectors are equivalent. If they are, we can just use coalesce and
    # be done. If they are not, then unless specifically flagged to skip mismatches we should
    # not get to this point.
    #
    # NB: This is actually ok, it just doesn't get coalesced. So we don't need an error here, I think.
    # if ( !all(equivalent) && !pick_first ) {
    #   stop("Error: not all elements are equivalent but pick_first was not chosen. This condition",
    #                     "should not have occurred. Please consult the code and/or developer for details.")
    # }

    if ( all(equivalent) || pick_first ) {

      # Coalesce the two columns together.
      new_column <- vec_coalesce(new_column, x[[var_list[v]]], pick_first=pick_first)
      # Then optionally remove the variable.
      if ( remove )
        x <- dplyr::select(x, -.data[[var_list[v]]])

    }

  } # for all old variables
  # Finally we add the new variable in with the new data.
  x <- dplyr::mutate(x, !!new_var := new_column)

  x
}


