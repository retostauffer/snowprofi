
#### UNUSED #### #' Convert Snowdata to Zoo
#### UNUSED #### #'
#### UNUSED #### #' Takes the object returned by \code[snowprofi]{read_snowdata}
#### UNUSED #### #' and converts this object (data.frame) into a multivariate
#### UNUSED #### #' zoo object. In addition, it performs a series of checks
#### UNUSED #### #'
#### UNUSED #### #' @param x data.frame returned by \code[snowprofi]{read_snowdata}.
#### UNUSED #### #' @param ... currently ignored.
#### UNUSED #### #'
#### UNUSED #### #' @return Returns a multivariate zoo object with a distinct
#### UNUSED #### #' way of naming the variables. This is required to be able
#### UNUSED #### #' to rip it appart again later on when we convert the data
#### UNUSED #### #' into a series of polygons for plotting.
#### UNUSED #### #'
#### UNUSED #### #' @export
#### UNUSED #### #' @importFrom zoo zoo is.regular
#### UNUSED #### #' @importFrom tidyr pivot_wider
#### UNUSED #### #' @author Reto Stauffer
#### UNUSED #### snowdata_to_zoo <- function(x, ...) {
#### UNUSED #### 
#### UNUSED ####     # Sanity checks
#### UNUSED ####     stopifnot(all(c("datetime", "height", "value", "level") %in% names(x)))
#### UNUSED #### 
#### UNUSED ####     # Convert from long to wide format
#### UNUSED ####     x <- tidyr::pivot_wider(x, names_from = "level", values_from = c("height", "value"))
#### UNUSED #### 
#### UNUSED ####     # Remove tibble df classes
#### UNUSED ####     class(x) <- "data.frame"
#### UNUSED #### 
#### UNUSED ####     # Convert to zoo
#### UNUSED ####     x <- zoo::zoo(x[, -1], x[, 1])
#### UNUSED ####     stopifnot(is.numeric(x)) # Check that the entire thing is numeric zoo
#### UNUSED #### 
#### UNUSED ####     # Check for strict regularity
#### UNUSED ####     if (!zoo::is.regular(x, strict = TRUE)) {
#### UNUSED ####         warning("Just that you know: the time series is not strictly regular and has some gaps!")
#### UNUSED ####     }
#### UNUSED #### 
#### UNUSED ####     return(x)
#### UNUSED #### }
