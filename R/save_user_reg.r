#' save user regression matrix
#'
#' Save a user-defined regression array or matrix with time series attributes to an external ASCII file 
#' in X-13ARIMA-SEATS' datevalue format
#'
#' Version 3.2, 12/27/2024
#'
#' @param this_reg double precision time series array or matrix to be saved.
#'        This is a required argument.
#' @param this_reg_file character string; name of file time series array or matrix to be saved to.
#'        This is a required argument.
#' @param add_dates logical scalar; save the file with dates on each record (datevalue format).
#'        Default is TRUE.
#' @param start_date Integer vector of length 2, Start date for series to be stored, where the 
#'        first element is the year and the second element is the month or quarter.. 
#'        Default is start of series.
#' @return file with user-defined regressors will be produced.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' gf_years <- c(2001, 2006, 2017, 2022)
#' this_gf <-
#'      gen_rw_regressors(gf_years, 4, 2000, 2027, 'gf', remove_cal_means = FALSE)
#' \dontrun{save_user_reg(this_gf, 'gf_2000_2027.txt')}
#' @import stats
#' @import utils
#' @export
save_user_reg <- function(this_reg = NULL, this_reg_file = NULL, add_dates = TRUE, start_date = NULL) {
    # Author: Brian C. Monsell (OEUS), Version 3.2, 12/27/2024
    
    if (is.null(this_reg)) {
        cat("must specify the time series regressor object")
        return(NULL)
    } else {
        # check if \code{this_reg} is a ts object
		if (!is.ts(this_reg)) {
			cat("must specify a ts object")
			return(NULL)
		}
    }    

    if (is.null(this_reg_file)) {
        cat("must specify the output file")
        return(NULL)
    } else {
		if (!is.character(this_reg_file)) {
			cat("must specify a character object for the regression file")
				return(NULL)
		}
    }

	if (!is.null(start_date)) {
		this_freq  <- frequency(this_reg)
		this_start <- start(this_reg)
		this_diff  <-
			difference_dates(start_date, this_start, this_freq)
		if (this_diff < 0) {
			cat(paste0("Date specified in start_date (", start_date, 
			           ") is before starting date of regressors (", this_start))
			return(NULL)		   
		}
		this_reg <- window(this_reg, start = start_date)
	}
	
    if (add_dates) {
	# generate time series data matrix with year, month, value
		temp <- cbind(time(this_reg)%/%1, cycle(this_reg), this_reg)
    # save data matrix into \code{this_reg_file}
		write.table(temp, this_reg_file, sep = " ", row.names = FALSE, col.names = FALSE)
    } else {
		write.table(this_reg, this_reg_file, sep = " ", row.names = FALSE, col.names = FALSE)
	}
	
}
