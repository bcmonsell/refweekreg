#' Reference week regression matrix
#'
#' Generate full regression matrix for reference week related holiday regressors, defined as a time series object.
#'
#' Version 2.0, 5/6/2024
#'
#' @param this_reference_week Array of date objects of the start of the reference week for a given month
#'        This is a required argument.
#' @param add_gf Logical scalar; if TRUE, a Good Friday holiday regressor will be included in the regression matrix. 
#'        Default setting is TRUE.
#' @param add_easter Logical scalar; if TRUE, an Easter holiday regressor will be included in the regression matrix. 
#'        Default setting is TRUE.
#' @param add_labor Logical scalar; if TRUE, a Labor Day holiday regressor will be included in the regression matrix. 
#'        Default setting is TRUE.
#' @param add_columbus Logical scalar; if TRUE, a Columbus Day holiday regressor will be included in the regression matrix. 
#'        Default setting is TRUE.
#' @param add_vet Logical scalar; if TRUE, a Veteran's Day holiday regressor will be included in the regression matrix. 
#'        Default setting is TRUE.
#' @param remove_cal_means Logical scalar; if TRUE, calendar month means are removed from the final regression matrix. 
#'        Default setting is TRUE.
#' @return Array of reference week dates starting in January of \code{start_year} and ending in December of \code{end_year}
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' replacement_dates <- c(as.Date('2013-11-10'), as.Date('2019-11-10'))
#' rw2005 <- gen_reference_day(2005, 2024, back_dates = replacement_dates)
#' holiday_matrix_2005 <- gen_rw_holiday_matrix(rw2005, add_gf = FALSE, add_easter = FALSE,
#'                        remove_cal_means = FALSE)
#' @import stats
#' @export
gen_rw_holiday_matrix <- 
    function(this_reference_week = NULL, add_gf = TRUE, add_easter = TRUE, add_labor = TRUE, 
             add_columbus = TRUE, add_vet = TRUE, remove_cal_means = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 2.0, 5/6/2024
    
    if (is.null(this_reference_week)) {
       cat("must specify reference week dates")
       return(NULL)
    }   
    
    # Determine how many holidays to include, and check if no holidays are specified
    number_of_holidays <- add_gf + add_easter + add_labor + add_columbus + add_vet
    if (number_of_holidays < 1) {
        stop("No holidays specified")
    }
    
    # initialize holiday regression matrix with zeros
    number_of_rows <- length(this_reference_week)
    this_holiday_matrix <- matrix(0, nrow = number_of_rows, ncol = number_of_holidays)
    
    # initialize objects and arrays needed to generate holiday regressors, including current column,
    # sequence of years, first and last year of regressors, month of the observations, and column
    # names
    this_col <- 0
    this_year_sequence <- unique(lubridate::year(this_reference_week))
    first_year <- min(this_year_sequence)
    last_year <- max(this_year_sequence)
    rw_month <- lubridate::month(this_reference_week)
    col_names <- array(" ", dim = number_of_holidays)
    
    # The next section will generate the different sets of regressors.  The general procedure is to
    # update the current column of the regression matrix, generate the dates for each year of the
    # holiday in question, generate the difference between the start of the reference week and the
    # date of the holiday in the month that the holiday regressor occurs in, get the years for which
    # the holiday falls in the reference week (or for Columbus Day and Veteran's Day, years the
    # holiday does not fall in), use the function \code{gen_rw_regressor} to generate the regressor,
    # update the column name.
    
    # Generate Good Friday regressors
    if (add_gf) {
        this_col <- this_col + 1
        gf_dates <- as.Date(timeDate::GoodFriday(this_year_sequence))
        this_diff <- this_reference_week[rw_month == 4] - gf_dates
        gf_years <- this_year_sequence[this_diff == -5]
        this_holiday_matrix[, this_col] <- gen_rw_regressors(gf_years, 4, first_year, last_year, remove_cal_means = remove_cal_means)
        col_names[this_col] <- "gf"
    }
    
    # Generate Easter regressors
    if (add_easter) {
        this_col <- this_col + 1
        easter_dates <- as.Date(timeDate::Easter(this_year_sequence))
        this_diff <- this_reference_week[rw_month == 4] - easter_dates
        easter_years <- this_year_sequence[this_diff == 0]
        this_holiday_matrix[, this_col] <- gen_rw_regressors(easter_years, 4, first_year, last_year, 
            remove_cal_means = remove_cal_means)
        col_names[this_col] <- "easter"
    }
    
    # Generate Labor Day regressors
    if (add_labor) {
        this_col <- this_col + 1
        labor_dates <- as.Date(timeDate::USLaborDay(this_year_sequence))
        this_diff <- this_reference_week[rw_month == 9] - labor_dates
        labor_years <- this_year_sequence[this_diff == -1]
        this_holiday_matrix[, this_col] <- gen_rw_regressors(labor_years, 9, first_year, last_year, 
            remove_cal_means = remove_cal_means)
        col_names[this_col] <- "labor"
    }
    
    # Generate Columbus Day regressors
    if (add_columbus) {
        this_col <- this_col + 1
        columbus_dates <- as.Date(timeDate::USColumbusDay(this_year_sequence))
        this_diff <- this_reference_week[rw_month == 10] - columbus_dates
        columbus_years <- this_year_sequence[this_diff == -8]  # indicating when it is not in the reference week
        this_holiday_matrix[, this_col] <- gen_rw_regressors(columbus_years, 10, first_year, last_year, 
            remove_cal_means = remove_cal_means)
        col_names[this_col] <- "columbus"
    }
    
    # Generate Veterans Day regressors
    if (add_vet) {
        this_col <- this_col + 1
        vet_dates <- as.Date(timeDate::USVeteransDay(this_year_sequence))
        this_diff <- this_reference_week[rw_month == 11] - vet_dates
        vet_years <- this_year_sequence[this_diff < -6]  # indicating when it is not in the reference week
        this_holiday_matrix[, this_col] <- gen_rw_regressors(vet_years, 11, first_year, last_year, 
            remove_cal_means = remove_cal_means)
        col_names[this_col] <- "vet"
    }
    
    # Add time series attributes to holiday matrix
    this_holiday_matrix <- ts(this_holiday_matrix, start = c(first_year, 1), end = c(last_year, 12), 
        frequency = 12)
    
    # Add column names to the regression matrix
    colnames(this_holiday_matrix) <- col_names
    
    return(this_holiday_matrix)
}
