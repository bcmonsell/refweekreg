#' Calendar mean adjust regressor
#'
#' Remove the calendar month mean of a given regressor expressed as a time series object.
#'
#' Version 2.0, 5/6/2024
#'
#' @param this_reg double precision time series array; a regressor from which the calendar month mean will be removed.
#'                 This is a required entry.
#' @return double precision time series array of the regressor with the calendar month mean removed
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' gf_years <- c(2001, 2006, 2017, 2022)
#' this_gf <-
#'      gen_rw_regressors(gf_years, 4, 2000, 2027, 'gf', remove_cal_means = FALSE)
#' this_gf_mean_adj <- calendar_mean_adj(this_gf)
#' @import stats
#' @export
calendar_mean_adj <- function(this_reg = NULL) {
    # Author: Brian C. Monsell (OEUS), Version 2.0, 5/6/2024
    
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
    
    # set variables for the periods and frequency of the regressor time series object
    this_cycle <- cycle(this_reg)
    this_freq <- frequency(this_reg)
    
    # initialize calendar mean and number of periods
    this_mean <- array(0, dim = this_freq)
    this_num <- array(0, dim = this_freq)
    
    # update the sums used to compute the calendar means
    for (i in 1:length(this_reg)) {
        this_mean[this_cycle[i]] <- this_mean[this_cycle[i]] + this_reg[i]
        this_num[this_cycle[i]] <- this_num[this_cycle[i]] + 1
    }
    
    # geneare the means from the sums
    for (i in 1:this_freq) {
        this_mean[i] <- this_mean[i]/this_num[i]
    }
    
    # remove the calendar means from the original regressor
    for (i in 1:length(this_reg)) {
        this_reg[i] <- this_reg[i] - this_mean[this_cycle[i]]
    }
    
    # return the calendar mean adjusted regressor
    return(this_reg)
    
}
