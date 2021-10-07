
#' Convert Snowdata to an sf Object
#'
#' This is the main function of the package. Taking some pre-prepared
#' \code{zoo} object returned by \code[snowprofi]{snowdata_to_zoo}
#' to create a bunch of polygons for each of the segments needed for
#' plotting.
#'
#' @param x object returned by \code[snowprofi]{snowdata_to_zoo} or
#'        \code[snowprofi]{snowdata_clean}. 
#'
#' @return An sf object with polygon geometries and
#' \code{level} and \code{value} as variables for plotting.
#'
#' @export
#' @importFrom zoo is.zoo
#' @author Reto
snowdata_to_sf <- function(x) {
    stopifnot(zoo::is.zoo(x), isTRUE(row.rm) | isFALSE(row.rm))

    # Check that the names match what we expect and extract level names
    stopifnot(all(grepl("^(height|value)_[0-9]+$", names(x))))
    levs  <- sort(unique(as.integer(regmatches(names(x), regexpr("[0-9]+$", names(x))))))

    # Do we have all the variables we need?
    for (var in c("height", "value")) {
        needed <- sprintf("%s_%d", var, levs)
        check  <- which(needed %in% names(x))
        if (!all(check)) { stop(sprintf("Missing %s", paste(needed[!check], collapse = ", "))) }
    }

    # Split height and data into two different zoo objects (same dimension, same order)
    height <- x[, sprintf("height_%d", levs)]
    value  <- x[, sprintf("value_%d",  levs)]
    rm(x) # No longer needed

    require("sf")

    # Now we need to create a function which
    # Loops from i = 2:nrow(height)
    # In each iteration we will build up polygons 
    #
    #  i-1   i
    # 
    #        +
    #      / |
    #    +   |
    #    |   |    vertical: height axis
    #    +   |
    #      \ |
    #        +
    #
    #   time axis
    #
    # The 'value' and 'level' will be kept alongside with the polygon.
    # Important is that we must also find out which polygons make sense.
    # If a polygon has size 0 we can drop it. If (for one step) multiple
    # polygons are laying on top of each other we keep the last one with
    # the highest level name (last in 'levs'); most to the right in the
    # data set.

    height <<- height
    value <<- value

    return(x)
}



#ylo <- 0
#res <- list()
#for (i in 10:50) {
#    # Get values and skip (no need for polygons) if all are NA or 0
#    v1 <- as.vector(value[i - 1, ]); v2 <- as.vector(value[i, ])
#    if (all(is.na(c(v1, v2)))) next
#
#    # Extracting height
#    h1 <- as.vector(height[i - 1, ]); h2 <- as.vector(height[i, ])
#    # Check if all values in both vectors are either NA or ylo (reference y)
#    all_ylo <- function(x, ylo) {
#        x <- na.omit(x)
#        return(isTRUE(all.equal(x, rep(ylo, length(x)))))
#    }
#    if (all_ylo(h1, ylo) & all_ylo(h2, ylo)) next
#
#    # We found some useful data. Now let us try to find out which of the
#    # levels is worth a polygon (not area 0). This is the case if the
#    # height DIFFERENCE changes for the specific level. Looping over l
#    for (l in seq_along(levs)) {
#        lo_h1 <- ifelse(l == 1, ylo, h1[l - 1]);        hi_h1 = h1[l]
#        lo_h2 <- ifelse(l == 1, ylo, h2[l - 1]);        hi_h2 = h2[l]
#        # No change? This layer has a volume of 0, skip creating a polygon!
#        if ((hi_h1 - lo_h1 + hi_h2 - lo_h2) == 0) next
#
#        # Else let us create the poly
#        #cat(sprintf(" Heights for i-1: %10.3f %10.3f     Heights for i: %10.3f %10.3f\n", lo_h1, hi_h1, lo_h2, hi_h2))
#        lo_time <- as.numeric(index(height)[i - 1]); hi_time <- as.numeric(index(height)[i])
#        p <- st_polygon(list(matrix(c(lo_time, lo_time, hi_time, hi_time, lo_time,
#                                      lo_h1,   hi_h1,   hi_h2,   lo_h2,   lo_h1),   ncol = 2)))
#
#        # Appending result to list. geomtry is the polygon, and add current level and value
#        # which is the value of the CURRENT time (i) and the current level.
#        res[[length(res) + 1]] <- list(geometry = st_sfc(p), level = levs[l], value = v2[l])
#    }
#    # We have to create a poly if height > 0. However, we also want to make
#}
#
#
#x <- dplyr::bind_rows(res)
#head(x)
#k <- st_sf(x)
#plot(st_geometry(k), asp = 0)
#plot(k[, "value"], asp = 0)
#
#
#
#
#
#
#
#
#
#
#
#
