#' Cleaning up Snowdata data set if needed
#'
#' Removes rows (times) where no data is available. No data means
#' no height or no value for all levels.
#'
#' @param x the object returned by snowdata_to_zoo
#' @param row.rm logical, defaults to FALSE. If TRUe
#'        rows not having any height or value (in combination)
#'        will be removed. Will change the polygons which will
#'        then bridge these gaps!
#'
#' @return Returns a cleaned \code{zoo} object nearly identical
#' to the input object \code{x} except that rows not containing
#' any meaningful data will be removed.
#'
#' Note that if you use this function the polygons may be different
#' 'briding' gaps. Maybe you don't want that anyways, wherefore this
#' function would be somewhat useless.
#' In addition the code is very similar to large parts of 
#' \code[snowprofi]{snowdata_to_sf}, if you keep it one should
#' try to streamline the two functions!
#'
#' @export
#' @importFrom zoo is.zoo
#' @author Reto Stauffer
snowdata_clean <- function(x, row.rm = FALSE) {
    stopifnot(zoo::is.zoo(x), isTRUE(row.rm) | isFALSE(row.rm))
    if (isFALSE(row.rm)) stop("With row.rm = FALSE) this function makes no sense :)")

    # Check that the names match what we expect and extract level names
    stopifnot(all(grepl("^(height|value)_[0-9]+$", names(x))))
    levs  <- sort(unique(as.integer(regmatches(names(x), regexpr("[0-9]+$", names(x))))))

    # Do we have all the variables we need?
    for (var in c("height", "value")) {
        needed <- sprintf("%s_%d", var, levs)
        check  <- which(needed %in% names(x))
        if (!all(check)) { stop(sprintf("Missing %s", paste(needed[!check], collapse = ", "))) }
    }

    # Remove rows without valid data
    if (row.rm == TRUE) {
        # Extracting height; make sure the order is the same in both
        height <- x[, sprintf("height_%d", levs)]
        value  <- x[, sprintf("value_%d",  levs)]
        # Now multiply. If any of the two is NA we will get an NA. Then
        # search for rows where we only have NA's and remove them.
        tmp  <- height * value
        kill <- which(apply(tmp, 1, function(x) sum(!is.na(x))) == 0)
        if (length(kill) > 0) { x <- x[-kill, ] }
    }

    return(x)
}
