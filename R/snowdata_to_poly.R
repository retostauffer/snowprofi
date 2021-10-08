
#' Convert Snowdata to Polygonformat
#'
#' Input is the object as returned by \code[snowprofi]{snowpack_read_wfj}.
#' We will, internally, ensure that we got what we expected, a zoo object
#' with some specific columns needed to generate the polygons.
#'
#' @param x object returned by \code[snowprofi]{snowdata_read_wfj}
#' @param baseheight numeric, single value. Typically 0 (ground height),
#'        used to construct the polygon for the first layer.
#'
#' @return An data.frame prepared to be used with \code[ggplot2]{geom_poly}.
#'
#' @export
#' @importFrom zoo is.zoo index
#' @author Reto
snowdata_to_poly <- function(x, baseheight = 0) {

    stopifnot(zoo::is.zoo(x))
    stopifnot(is.numeric(baseheight), length(baseheight) == 1)

    # Measure execution time ...
    t_start <- Sys.time()

    # We _MUST_ have DZ_SNOW_M_* as this contains the height (top) of the
    # snow layers. This also gives us the number of layers we have at hand.
    # We can then make use of this to identify all other varaibles which
    # are available on the layers such that we can attach them to the final
    # object later on.
    idx <- grep("^DZ_SNOW_M_[0-9]+$", names(x)); stopifnot(length(idx) > 0)
    nlev_snow <- length(idx)

    # Now find 'groups of variables'. We therefore remove the layer info
    # at the end (_[0-9]+$) and count how often they occur. All groups of
    # variables which have nlev_snow levels are considered as data connected
    # to the snow layers and will be added to the data later.
    tbl       <- table(gsub("_[0-9]+$", "", names(x)))
    snow_vars <- names(tbl)[tbl == nlev_snow]
    cat("Identified the following snow-related variables:", paste(snow_vars, collapse = ", "), "\n")

    # Store column names for the required variables.
    # They are used later in the loop to extract the data
    # when creating the polygons. The function get_colnames
    # ensures that these names are in the correct order according
    # to the level information in the names of the variables.
    # We do this once for:
    #  - snow layer height (DZ_SNOW_M)
    #  - all other variables in snow_vars (will be attached later)
    get_colnames <- function(pattern, cols) {
        idx  <- grep(pattern, cols)
        cols <- cols[idx]
        levs <- as.integer(regmatches(cols, regexpr("[0-9]+$", cols)))
        return(cols[order(levs)])
    }

    cols_height    <- get_colnames("DZ_SNOW_M_[0-9]+$", names(x))
    cols_snow_vars <- list()
    for (n in snow_vars[!snow_vars == "DZ_SNOW_M"]) {
        cols_snow_vars[[n]] <- get_colnames(sprintf("^%s_[0-9]+$", n), names(x))
    }

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

    res <- list()

    # Helper function used below; checks if all values are either NA or baseheight.
    all_base <- function(x, baseheight) {
        x <- na.omit(x)
        if (length(x) == 0) return(TRUE)
        return(isTRUE(all.equal(x, rep(baseheight, length(x)))))
    }

    # Progress bar ...
    pb <- txtProgressBar(0, nrow(x), style = 3)

    for (i in 2:nrow(x)) {

        setTxtProgressBar(pb, i)

        # Extracting heights
        h1 <- as.vector(x[i - 1, cols_height])
        h2 <- as.vector(x[i,     cols_height])

        # Check if all values in both vectors are either NA or baseheight.
        # If so, we don't have to process them and skip this iteration.
        if (all_base(h1, baseheight) & all_base(h2, baseheight)) next

        # We found some useful data. Now let us try to find out which of the
        # levels is worth a polygon (not area 0). This is the case if the
        # height DIFFERENCE changes for the specific level. Looping over l
        for (l in seq_len(nlev_snow)) {

            # Extracting lo (lower level) and hi (higher level)
            # for both, the previous step (i - 1) and the current step (i).
            # Previous step is h1, current step h2.
            lo_h1 <- ifelse(l == 1, baseheight, h1[l - 1]);        hi_h1 = h1[l]
            lo_h2 <- ifelse(l == 1, baseheight, h2[l - 1]);        hi_h2 = h2[l]

            ###DEV###cat("Heights lo: %.3f %.3f, Heights hi %.3f %.3f\n", lo_h1, lo_h2, hi_h1, hi_h2)

            # Missing values in one of the heights? Skip as we cannot make a proper polygon.
            if (any(is.na(c(lo_h1, hi_h1, lo_h2, hi_h2)))) next

            # No change? This layer has a volume of 0, skip creating a polygon!
            if ((hi_h1 - lo_h1 + hi_h2 - lo_h2) == 0) next

            # Else let us create the poly
            lo_time <- zoo::index(x)[i - 1]
            hi_time <- zoo::index(x)[i]

            # Make the polygon now. 'grp' is an auto increment, each integer
            # defines one polygon (used later for ggplot2). 'timestamp' and 'height'
            # later make up the coordinates for the polygons. In addition we some
            # more information: the level (integer; level count) as well as 
            # the values of the variables we identified to be connected
            # to snow layers. This information is stored in cols_snow_vars.
            tmp <- data.frame(grp       = length(res),
                              timestamp = c(lo_time, lo_time, hi_time, hi_time),
                              height    = c(lo_h1, hi_h1, hi_h2, lo_h2),
                              level     = l)

            for (n in names(cols_snow_vars)) tmp[[n]] <- x[i, cols_snow_vars[[n]][l]]

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

    t_diff <- Sys.time() - t_start
    cat(sprintf("The entire preparation of the polygons took %.1f %s\n",
                as.numeric(t_diff, units = units(t_diff)), units(t_diff)))

    return(x)
}


