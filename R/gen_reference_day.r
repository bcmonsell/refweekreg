#' Reference Day Generation
#'
#' Generate the start of the reference week for a span of years, with an option to incorporate exceptions
#'
#' Version 3/0 5/6/2024
#'
#' @param start_year First year of the sequence
#'                   This is a required argument.
#' @param end_year Ending year of the sequence
#'                 This is a required argument.
#' @param census_adj Logical scalar, apply adjustment done by US Census Bureau for November and December.
#'                   Default is TRUE.
#' @param back_dates Array of date objects where the reference week needs to be set back a week by interviewers
#' @return Array of reference week dates starting in January of \code{start_year} and ending in December of \code{end_year}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' replacement_dates <- c(as.Date('2013-11-10'), as.Date('2019-11-10'))
#' rw2005 <- gen_reference_day(2005, 2024, back_dates = replacement_dates)
#' @import stats
#' @export
gen_reference_day <- function(start_year = NULL, end_year = NULL, census_adj = TRUE, back_dates = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 3/0 5/6/2024
    
    if (is.null(start_year)) {
        cat("must specify the starting year")
        return(NULL)
    } else {
        if (is.null(end_year)) {
            cat("must specify the ending year")
            return(NULL)
        }
    }
    
    # Generate text string for starting and ending dates of the reference week date sequence
    start_date <- paste0(start_year, "-01-12")
    end_date <- paste0(end_year, "-12-12")
    
    # Generate array with a sequnce of date objects for the start of every month
    this_date_sequence <- seq(as.Date(start_date), as.Date(end_date), by = "months")
    
    # Generate array with starting day of the reference week
    this_reference_date <- 13 - lubridate::wday(this_date_sequence)
    
    # Construct date object for reference day by pasting together the year, month and day
    reference_day_sequence <- as.Date(paste0(lubridate::year(this_date_sequence), "-", lubridate::month(this_date_sequence), 
        "-", this_reference_date))
    
    if (census_adj) {
        # set up year, month, and weekday arrays related to the reference date sequence
        reference_day_year <- lubridate::year(this_date_sequence)
        reference_day_month <- lubridate::month(this_date_sequence)
        reference_day_wday <- lubridate::wday(this_date_sequence)
    
        # Adjust reference dates for December when the week containing the 5th is completely 
        # within December
    
        this_filter <- reference_day_month == 12 & reference_day_wday < 6
        reference_day_sequence[this_filter] <- reference_day_sequence[this_filter] - 7
    
        # Adjust reference dates for November after 2006 when the week containing the 19th 
        # has Thanksgiving
    
        this_filter <- 
            reference_day_year > 2005 & reference_day_month == 11 & reference_day_wday < 3
        reference_day_sequence[this_filter] <- reference_day_sequence[this_filter] - 7
    
        # Add changes made due to interviewer changes for November and December
    
        if (!is.null(back_dates)) {
            for (i in 1:length(back_dates)) {
                y <- lubridate::year(back_dates[i])
                m <- lubridate::month(back_dates[i])
                this_filter <- reference_day_month == m & reference_day_year == y
                reference_day_sequence[this_filter] <- reference_day_sequence[this_filter] - 7
            }
        }
    }
    
    # Return reference day sequence
    return(reference_day_sequence)
    
}
