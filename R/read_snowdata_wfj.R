
#' Reading Snowprofile Data from fwj ASCII File
#'
#'
#' @param file name of the file to read; expects the file be
#'        in the 'wfj' format you export.
#' @param htol numeric, height tolerance to double-check the
#'        calculation, default is 0.001 (1mm). Must be larger
#'        or equal to 0. See details.
#'
#' @details
#' The columns \code{DZ_SNOW_M} contain the individual heights
#' of all snow layers. We modify the data set calculating the
#' top height of each layer by using \code{cumsum}. In addition
#' the data set contains \code{H_SNOW} which is the total height
#' of the snow pack.
#'
#' We can then check the height above ground of the last snow
#' layer against \code{H_SNOW} which should be the same. Due to
#' the precision of the data in the input file there are small
#' differences. Thus, we use \code{htol} as a tolerance. If the
#' difference is smaller than \code{htol} we consider this
#' as a success, if larger deviations are found an error will
#' be thrown.
#'
#' @return Returns a data.frame with with three variables named
#'
#' @examples
#' x <- read_snowdata_wfj("data/202_snow_output_wfj.txt")
#'
#' @importFrom zoo zoo is.regular
#' @export
#' @author Reto
read_snowdata_wfj <- function(file, htol = 0.001) {
    stopifnot(is.character(file), length(file) == 1L, file.exists(file))
    stopifnot(is.numeric(htol), length(htol) == 1L, htol >= 0)


    # Code provided by Sascha
    x          <- read.table(file, header= FALSE, sep = ",", skip = 13)
    stopifnot(nrow(x) > 0)

    timestring <- strptime(paste(x[,2], x[,3]), format = "%Y%m%d %H:%M")
    x          <- zoo::zoo(x[,-(1:3)], timestring)

    # Check if the time series is strictly regular
    if (!zoo::is.regular(x, strict = TRUE)) {
        warning("Take care, your time series is not strictly regular and contains some gaps!")
    }

    # Replacing missing values with NA
    x[which(x == -99999)] = NA

    # Reading header line; add names to zoo object x
    header     <- read.table(file, skip = 10, nrows = 1, header = FALSE, sep = ",")
    header     <- as.character(header)[-c(1:3)] # Drop first three names
    names(x)   <- header

    # The last column is called 'NA', we drop this here ...
    x <- x[, !grepl("^NA$", names(x))]

    # First we check for two specific varaibles we need to
    # find out how many vertical levels we have. Once for
    # the snow pack, once for ground (sub-serface) temperatures.
    # If these names are not there: stop immediately.
    stopifnot("DZ_SNOW_M" %in% names(x), "T_SO" %in% names(x))

    nlev_snow <- sum(names(x) == "DZ_SNOW_M") # Snow levels
    nlev_grnd <- sum(names(x) == "T_SO")      # Ground levels
    cat(sprintf("Found %d snow levels and %d ground levels\n", nlev_snow, nlev_grnd))

    # Check we got the name expected, and as often as
    # expected.
    expected_names <- c("T_SNOW"        = 1,           # one value, surface temperature (top snow layer)
                        "H_SNOW"        = 1,           # one value, total snow height
                        "N_SNOW_M"      = 1,           # one value, number of 'active' snow levels
                        "DZ_SNOW_M"     = nlev_snow,   # snow layer thickness
                        "TofSNW_LTOP_M" = nlev_snow,   # temperature of snow layers
                        "ICEinSNW_VC_M" = nlev_snow,   # ice content of snow layers
                        "H2OinSNW_VC_M" = nlev_snow,   # water content of snow layers
                        "T_SO"          = nlev_grnd)   # temperature of ground layers
    for (n in names(expected_names)) {
        tmp <- sum(names(x) == n)
        if (!tmp == expected_names[n]) {
            stop(sprintf("Found %d columns for \"%s\", expected %d columns!", tmp, n, expected_names[n]))
        }
    }

    # ---------------------------------------------
    # Next we start manipulating the data set.
    # First, we start renaming the variables which belong to a specific
    # level by adding "_<level>" at the end. This makes them unique.
    for (n in names(expected_names)) {
        if (expected_names[n] == 1) next # No renaming happens here.
        # Else search for the index; rename these columns
        idx <- which(names(x) == n)
        names(x)[idx] <- paste(names(x)[idx], seq_along(idx), sep = "_")
    }

    # One last step is to cumulate the layer heights as we need to 'top'
    # of each layer measured from the ground for the plotting later on.
    # Done via cumsum:
    idx <- grep("^DZ_SNOW_M_[0-9]+$", names(x))
    x[, idx] <- t(apply(x[, idx], 1, cumsum))

    # Double-check if that is all fine. Calculate the difference
    # of the height of the last layer (from cumsum) and the
    # variable 'H_SNOW' which contains the overall snow height.
    h_diff <- x[, max(idx)] - x[, "H_SNOW"]
    if (max(abs(h_diff), na.rm = TRUE) > htol) {
        stop("Found unexpectedly large differences between calculated and overall snow height!")
    }

    return(x)

}
