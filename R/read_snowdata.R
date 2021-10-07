
#' Reading CSV File Sascha-Version
#'
#' Reading the demo file you sent me and prepare some
#' things. I also assume a 'date' and use your time
#' information on X as 'hours after initialization'.
#'
#' @param file name of the file to read (format you sent me). Must
#'        be character of length 1 (and exist).
#' @param start object of class POSIXt, length 1. Used as starting
#'        point to generate the datetime variable.
#' @param value.na NULL or vector of the same class as the value column (your Z)
#'        If not NULL, these values are replaced with NA.
#'
#' @return Returns a data.frame with with three variables named
#' \code{datetime} (POSIXct), \code{
#'
#' @examples
#' x <- read_snowdata("data/datasascha.csv", as.POSIXct("2021-10-03"), value.na = 0)
#'
#' @export
#' @author Reto
read_snowdata <- function(file, start, value.na = NULL) {
    stopifnot(is.character(file), length(file) == 1L, file.exists(file))
    stopifnot(inherits(start, "POSIXt"), length(start) == 1L)

    # Trying to read the file
    x <- tryCatch(read.csv(file),
                  warning = function(w) { warning(w); stop("Warning not good, stop.") },
                  error   = function(e) { stop(e) } )

    # Make sure it contains what we expect it to contain for now
    stopifnot(identical(names(x), c("X", "Y", "Z")))
    # Rename and adjust 
    names(x) <- c("datetime", "height", "value")
    x <- transform(x, datetime = start + (datetime - 1) * 3600)

    # Now we have to make sure we have the same number of levels
    # for each date. If not, stop.
    nlevs <- unique(table(x$datetime))
    if (!length(nlevs) == 1L) {
        stop("Not all times have the same number of levels! Stop!")
    }

    # Knowing that we have nlev levels for each of the time step
    # we can now add the levels by repeating 1:nlevs as often
    # as we need.
    x <- transform(x, level = rep(1:nlevs, length.out = nrow(x)))

    # Replace "0" in x$value if existing
    if (!is.null(value.na)) {
        stopifnot(class(value.na) == class(x$value))
        warning(sprintf("Replacing %s in value (Z) with missing values.", paste(value.na, collapse = ", ")))
        x <- transform(x, value = ifelse(value %in% value.na, NA, value))
    }
    return(x)
}
