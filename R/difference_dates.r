#' Date difference
#'
#' Generate the difference between two dates.
#'
#' Version 1.4, 12/31/2024
#'
#' @param first_date Integer array of length 2, a date where the first element is the year and 
#'        the second element is the month or quarter. This is a required entry.
#' @param second_date Integer array of length 2, a date to comapare to \code{this_date}.
#'        This is a required entry.
#' @param this_frequency Integer scalar, frequency of target time series.
#'        Entries limited to 1 (yearly), 2 (biannual), 3 (triannual), 4 (quarterly), 
#'        6 (Bimonthly), and 12. Default is 12 (monthly). 
#' @return Integer scalar; difference between first date and second date.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' diff_start <- difference_dates(start(shoes2007), c(1990,1))
#' @export
difference_dates <- 
	function(first_date = NULL, 
			 second_date = NULL, 
			 this_frequency = 12) {
    # Author: Brian C. Monsell (OEUS), Version 1.4, 12/31/2024
	
    # check if a value is specified for \code{first_date}
    if (is.null(first_date)) {
        stop("no date specified for first_date")
    }
    
    # check if a value is specified for \code{second_date}
    if (is.null(second_date)) {
        stop("no date specified for second_date")
    }
    
    # check if length of the arguments are the same.
    if (length(first_date) != length(second_date)) {
        stop(paste("Lengths of arguments not compatable - ", length(first_date), 
                   length(second_date), sep = " "))
    }
	
	freq_set <- c(1,2,3,4,6,12)
	this_match <- this_frequency %in% freq_set
	if (!this_match) {
		cat(paste0("Improper entry for this_frequency: ", this_frequency))
		stop(paste("Valid entries are ", freq_set))
	}
	
	if (this_frequency == 12) {
		first_date_string  <- stringr::str_pad(first_date[2], 2, pad = "0")
		second_date_string <- stringr::str_pad(second_date[2], 2, pad = "0")
	} else {
		first_period       <- ((12 / this_frequency) * (first_date[2] - 1)) + 1
		first_date_string  <- stringr::str_pad(first_period, 2, pad = "0")
		second_period      <- ((12 / this_frequency) * (second_date[2] - 1)) + 1
		second_date_string <- stringr::str_pad(second_period, 2, pad = "0")
	}
	
	
	first_date_object  <- as.Date(paste0(first_date[1],   "-", first_date_string,  "-01"))
	second_date_object <- as.Date(paste0(second_date[1],  "-", second_date_string, "-01"))
	
	return(first_date_object - second_date_object)
}