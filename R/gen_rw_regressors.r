#' Generate reference week regressor
#'
#' Generate specific reference week related holiday regressors, defined as a time series object. 
#' The object returned is either a time series vector or a matrix of time series indicator variables.
#'
#' Version 3.0, 5/6/2024
#'
#' @param holiday_years Integer array; Years where the holiday appears in the reference week (or for Columbus Day 
#'        or Veteran's day, years where the holiday does not occur in the reference week)
#'        This is a required argument.
#' @param holiday_month Integer scalar; The month in which this holiday occurs. For Good Friday and Easter, 
#'        this would be April (4); for Labor Day, this would be September (9), etc.
#'        This is a required argument.
#' @param start_year Integer scalar; First year of the generated holiday regressor. 
#'        The regressor will begin on the first observation of this year.
#'        This is a required argument.
#' @param end_year Integer scalar; Final year of the generated holiday regressor. 
#'        The regressor will end on the last observation of this year.
#'        This is a required argument.
#' @param col_label Character scalar; A label used to generate column names for the individual AO regressors 
#'        if \code{join_regressors = FALSE}.
#' @param join_regressors Logical scalar; 
#'        if TRUE, individual indicator regressors are combined into one grouped regressor. 
#'        If FALSE, a matrix of the individual AO regressors will be returned. 
#'        Default setting is TRUE.
#' @param remove_cal_means Logical scalar; if TRUE, calendar month means are removed from the 
#'        final regression matrix. 
#'        Default setting is TRUE.
#' @return if \code{join_regressors = TRUE}, a time series object with the holiday regressor is returned; 
#'         otherwise, a matrix of AO regressors for the individual holidays is returned.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' gf_years <- c(2001, 2006, 2017, 2022)
#' # returns a time series object with one grouped regressor
#' this_gf_grouped <-
#'      gen_rw_regressors(gf_years, 4, 2000, 2027)
#' # returns a time series matrix with four columns of indicator regressors
#' this_gf_individual <-
#'      gen_rw_regressors(gf_years, 4, 2000, 2027, 'gf', join_regressors = FALSE)
#' @import stats
#' @export
gen_rw_regressors <- 
    function(holiday_years = NULL, holiday_month = NULL, start_year = NULL, end_year = NULL, col_label = "Reg", 
             join_regressors = TRUE, remove_cal_means = TRUE) {
    # Author: Brian C. Monsell (OEUS), Version 3.0, 5/6/2024
    
    if (is.null(start_year)) {
        cat("must specify the starting year")
        return(NULL)
    } else {
        if (is.null(end_year)) {
            cat("must specify the ending year")
            return(NULL)
        }
    }
    
    if (is.null(holiday_years)) {
        cat("must specify the holiday years")
        return(NULL)
    } else {
        if (is.null(holiday_month)) {
            cat("must specify the holiday month")
            return(NULL)
        }
    }

    # initialize holiday regression matrix with zeros, adding time series attributes
    holiday_length <- (end_year - start_year + 1) * 12
    holiday_number <- length(holiday_years)
    holiday_matrix <- ts(matrix(0, ncol = holiday_number, nrow = holiday_length), start = c(start_year, 
        1), end = c(end_year, 12), frequency = 12)
    
    # generate regressors for month and years of individual observations
    this_month <- cycle(holiday_matrix)
    this_year <- time(holiday_matrix)%/%1
    
    # generate indicator variables putting a 1 in the month and year of the holidays
    for (i in 1:holiday_number) {
        this_date <- holiday_years[i]
        this_filter <- this_month == holiday_month & this_year == holiday_years[i]
        holiday_matrix[this_filter, i] <- 1
    }
    
    # if regressors are joined generate row sum of the holiday matrix, adding time series attributes
    if (join_regressors) {
        holiday_reg <- ts(rowSums(holiday_matrix), start = c(start_year, 1), end = c(end_year, 12), 
            frequency = 12)
        
        # remove calendar period means if requested
        if (remove_cal_means) {
            holiday_reg <- calendar_mean_adj(holiday_reg)
        }
        
        # return holiday regressor
        return(holiday_reg)
    } else {
        
        # add column names to regression matrix
        colnames(holiday_matrix) <- paste0(col_label, holiday_years)
        
        # return holiday matrix
        return(holiday_matrix)
    }
    
}
