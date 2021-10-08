
##### unused ##### #' Reading CSV File Sascha-Version
##### unused ##### #'
##### unused ##### #' Reading the demo file you sent me and prepare some
##### unused ##### #' things. I also assume a 'date' and use your time
##### unused ##### #' information on X as 'hours after initialization'.
##### unused ##### #'
##### unused ##### #' @param file name of the file to read (format you sent me). Must
##### unused ##### #'        be character of length 1 (and exist).
##### unused ##### #' @param start object of class POSIXt, length 1. Used as starting
##### unused ##### #'        point to generate the datetime variable.
##### unused ##### #' @param value.na NULL or vector of the same class as the value column (your Z)
##### unused ##### #'        If not NULL, these values are replaced with NA.
##### unused ##### #'
##### unused ##### #' @return Returns a data.frame with with three variables named
##### unused ##### #' \code{datetime} (POSIXct), \code{
##### unused ##### #'
##### unused ##### #' @examples
##### unused ##### #' x <- read_snowdata("data/datasascha.csv", as.POSIXct("2021-10-03"), value.na = 0)
##### unused ##### #'
##### unused ##### #' @export
##### unused ##### #' @author Reto
##### unused ##### read_snowdata <- function(file, start, value.na = NULL) {
##### unused #####     stopifnot(is.character(file), length(file) == 1L, file.exists(file))
##### unused #####     stopifnot(inherits(start, "POSIXt"), length(start) == 1L)
##### unused ##### 
##### unused #####     # Trying to read the file
##### unused #####     x <- tryCatch(read.csv(file),
##### unused #####                   warning = function(w) { warning(w); stop("Warning not good, stop.") },
##### unused #####                   error   = function(e) { stop(e) } )
##### unused ##### 
##### unused #####     # Make sure it contains what we expect it to contain for now
##### unused #####     stopifnot(identical(names(x), c("X", "Y", "Z")))
##### unused #####     # Rename and adjust 
##### unused #####     names(x) <- c("datetime", "height", "value")
##### unused #####     x <- transform(x, datetime = start + (datetime - 1) * 3600)
##### unused ##### 
##### unused #####     # Now we have to make sure we have the same number of levels
##### unused #####     # for each date. If not, stop.
##### unused #####     nlevs <- unique(table(x$datetime))
##### unused #####     if (!length(nlevs) == 1L) {
##### unused #####         stop("Not all times have the same number of levels! Stop!")
##### unused #####     }
##### unused ##### 
##### unused #####     # Knowing that we have nlev levels for each of the time step
##### unused #####     # we can now add the levels by repeating 1:nlevs as often
##### unused #####     # as we need.
##### unused #####     x <- transform(x, level = rep(1:nlevs, length.out = nrow(x)))
##### unused ##### 
##### unused #####     # Replace "0" in x$value if existing
##### unused #####     if (!is.null(value.na)) {
##### unused #####         stopifnot(class(value.na) == class(x$value))
##### unused #####         warning(sprintf("Replacing %s in value (Z) with missing values.", paste(value.na, collapse = ", ")))
##### unused #####         x <- transform(x, value = ifelse(value %in% value.na, NA, value))
##### unused #####     }
##### unused #####     return(x)
##### unused ##### }
