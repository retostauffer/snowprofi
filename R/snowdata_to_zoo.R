
#' Convert Snowdata to Zoo
#'
#' Takes the object returned by \code[snowprofi]{read_snowdata}
#' and converts this object (data.frame) into a multivariate
#' zoo object. In addition, it performs a series of checks
#'
#' @param x data.frame returned by \code[snowprofi]{read_snowdata}.
#' @param ... currently ignored.
#'
#' @return Returns a multivariate zoo object with a distinct
#' way of naming the variables. This is required to be able
#' to rip it appart again later on when we convert the data
#' into a series of polygons for plotting.
#'
#' @export
#' @importFrom zoo zoo is.regular
#' @importFrom tidyr pivot_wider
#' @author Reto Stauffer
snowdata_to_zoo <- function(x, ...) {
    stopifnot(all(c("datetime", "height", "value", "level") %in% names(x)))
    x <- pivot_wider::pivot_wider(x, names_from = "level", values_from = c("height", "value"))
    class(x) <- "data.frame"
    x <- zoo::zoo(x[, -1], x[, 1])
    stopifnot(is.numeric(x))

    # Check for strict regularity
    if (!zoo::is.regular(x, strict = TRUE)) {
        warning("Just that you know: the time series is not strictly regular and has some gaps!")
    }
    return(x)
}
