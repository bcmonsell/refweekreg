#' generate 4/5 week regressors
#'
#' Generate 4 to 5 week effect regressors from paper by Cano, Scott, Kropf, Scott and Stamas (1996)
#'
#' Version 3.0 5/6/2024
#'
#' @param start_year First year of the sequence
#'                   This is a required argument.
#' @param end_year Ending year of the sequence
#'                 This is a required argument.
#' @param omit_march Logical scalar, exclude March from the set of regressors returned
#'        Default is TRUE
#' @param remove_cal_means Logical scalar; 
#'        if TRUE, calendar month means are removed from the final regression matrix. 
#'        Default setting is FALSE.
#' @return Matrix of time series arrays of 4/5 week regressors starting in January of 
#'         \code{start_year} and ending in December of \code{end_year}, with each column
#'         regresenting a different month.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples 
#' four2five_reg     <- gen_4to5_week_regressors(2005, 2024)
#' four2five_reg_cal <- gen_4to5_week_regressors(2005, 2024, remove_cal_means = TRUE)
#' @import stats
#' @export
gen_4to5_week_regressors <- 
    function(start_year = NULL, end_year = NULL, omit_march = TRUE, remove_cal_means = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 3.0 5/6/2024
    
    if (is.null(start_year)) {
        cat("must specify the starting year")
        return(NULL)
    } else {
        if (is.null(end_year)) {
            cat("must specify the ending year")
            return(NULL)
        }
    }
    
    this_rw <- ts(gen_reference_day(start_year-1, end_year, census_adj = FALSE),
                  start = c(start_year-1,1), frequency = 12)
    diff_rw <- window(diff(this_rw), start = c(start_year, 1), end = c(end_year, 12))

    reg_4to5 <- array(1, dim=length(diff_rw))
    reg_4to5[diff_rw < 29] <- -0.6
    
    numRows <- length(reg_4to5)
    
    matrix_4to5 <- matrix(0, ncol = 12, nrow = numRows)
    for (i in 1:numRows) {
         j <- i %% 12
         if (j == 0) {
             j <- 12
         }
         matrix_4to5[i,j] <- reg_4to5[i]
    }
    
    if (remove_cal_means) {
        for (j in 1:12){
             this_month <- matrix(matrix_4to5[,j], ncol = 12, byrow = TRUE)
             cal_means <- apply(this_month, 2, mean)

             for (i in 1:numRows) {
                  k <- i %% 12
                  if (k == 0) {
                      k <- 12
                  }
                  matrix_4to5[i,j] <- matrix_4to5[i,j] - cal_means[k]
             }
        }
    }
        
    this_month_abb <- unique(month.abb[cycle(diff_rw)])
    if (omit_march) {
        matrix_4to5 <- cbind(matrix_4to5[,1:2], matrix_4to5[,4:12])
        colnames(matrix_4to5) <- c(this_month_abb[1:2], this_month_abb[4:12])
    } else {
        colnames(matrix_4to5) <- this_month_abb
    }
        
    return(ts(matrix_4to5, start = c(start_year, 1), frequency = 12))
}
