
#' Convert Snowdata to Polygonformat
#'
#' Takes the object returned by \code[snowprofi]{snowdata_to_zoo}
#' and prepares a data.frame which can be used with \code[ggplot2]{geom_poly}.
#' The way it works is that you need a grouping identifier, and multiple (four)
#' points. The rest is then taken over by ggplot2.
#'
#' @param x object returned by \code[snowprofi]{snowdata_to_zoo} or
#'        \code[snowprofi]{snowdata_clean}. 
#'
#' @return An data.frame prepared to be used with \code[ggplot2]{geom_poly}.
#'
#' @export
#' @importFrom zoo is.zoo index
#' @author Reto
snowdata_to_poly <- function(x) {

    t_start <- Sys.time() # for measuring time elapsed ...

    # Sanity check
    stopifnot(zoo::is.zoo(x))

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

    ylo <- 0
    res <- list()

    # Progress bar ...
    pb <- txtProgressBar(0, nrow(height), style = 3)

    for (i in 2:nrow(height)) {

        setTxtProgressBar(pb, i)

        # Get values and skip (no need for polygons) if all are NA or 0
        v1 <- as.vector(value[i - 1, ]); v2 <- as.vector(value[i, ])
        if (all(is.na(c(v1, v2)))) next

        # Extracting height
        h1 <- as.vector(height[i - 1, ]); h2 <- as.vector(height[i, ])
        # Check if all values in both vectors are either NA or ylo (reference y)
        all_ylo <- function(x, ylo) {
            x <- na.omit(x)
            return(isTRUE(all.equal(x, rep(ylo, length(x)))))
        }
        if (all_ylo(h1, ylo) & all_ylo(h2, ylo)) next

        # We found some useful data. Now let us try to find out which of the
        # levels is worth a polygon (not area 0). This is the case if the
        # height DIFFERENCE changes for the specific level. Looping over l
        for (l in seq_along(levs)) {

            # Data point missing? We still proceed as we would like to have
            # the missing values v2[l] in there (will be drawn with missing color).

            # Extracting lo (lower level) and hi (higher level)
            # for both, the previous step (i - 1) and the current step (i).
            # Previous step is h1, current step h2.
            lo_h1 <- ifelse(l == 1, ylo, h1[l - 1]);        hi_h1 = h1[l]
            lo_h2 <- ifelse(l == 1, ylo, h2[l - 1]);        hi_h2 = h2[l]

            if (any(is.na(c(lo_h1, hi_h1, lo_h2, hi_h2)))) next # Missing value, skip

            # No change? This layer has a volume of 0, skip creating a polygon!
            if ((hi_h1 - lo_h1 + hi_h2 - lo_h2) == 0) next

            # Else let us create the poly
            lo_time <- zoo::index(height)[i - 1]
            hi_time <- zoo::index(height)[i]

            # Make the polygon now
            tmp <- data.frame(grp       = length(res),
                              timestamp = c(lo_time, lo_time, hi_time, hi_time),
                              height    = c(lo_h1, hi_h1, hi_h2, lo_h2),
                              #timestamp = c(lo_time, lo_time, hi_time, hi_time, lo_time),
                              #height    = c(lo_h1, hi_h1, hi_h2, lo_h2, lo_h2),
                              level     = levs[l],
                              value     = v2[l])
            # Appending result to list. geomtry is the polygon, and add current level and value
            # which is the value of the CURRENT time (i) and the current level.
            res[[length(res) + 1]] <- tmp
        }
    }
    close(pb)

    if (length(res) == 0) {
        stop("Have not found ANY valid cell to create a polygon!")
    }

    # Combine the list to a proper data.frame, which we then immediately
    # convert to an sf object. Autoamtically detects and uses the 'geometry'
    # variable as sf geometry.
    x <- do.call(rbind, res)

    t_diff <- as.numeric(Sys.time() - t_start)
    cat(sprintf("The entire preparation of the polygons took %.1f %s\n",
                ifelse(t_diff < 60, t_diff, t_diff / 60),
                ifelse(t_diff < 60, "seconds", "minutes")))

    return(x)
}


