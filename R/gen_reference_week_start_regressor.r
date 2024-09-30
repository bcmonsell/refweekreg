#' generate reference week start holiday regressors
#'
#' Generate reference week start holiday regressors, defined as a time series matrix. 
#'
#' Version 4.0 5/6/2024
#'
#' @param this_reference_day Array of date objects of the start of the reference week for a given month
#'        This is a required argument.
#' @param collapse_col integer scalar; collapses the first \code{collapse_col} columns into a single column; 
#'        Default setting is NULL, all columns are returned.
#' @param reg_means numeric vector; vector of means to be removed from the regressors; 
#'        Default is NULL, no mean removal done.
#' @param contrast_reg Logical scalar; if TRUE, contrast regressors are generated. 
#'        Default setting is TRUE.
#' @return Time series regression matrix object with reference week start regressors.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' replacement_dates <- c(as.Date('2013-11-10'), as.Date('2019-11-10'))
#' rw2005 <- gen_reference_day(2005, 2024, back_dates = replacement_dates)
#' ref_week_start_reg <- gen_reference_week_start_regressor(rw2005)
#' @import stats
#' @export
gen_reference_week_start_regressor <- 
  function(this_reference_day = NULL, collapse_col = NULL, reg_means = NULL, contrast_reg = TRUE) {
  # Author: Brian C. Monsell (OEUS) Version 4.0 5/6/2024
  
  if (is.null(this_reference_day)) {
       cat("must specify reference days")
       return(NULL)
  }   
  
  this_date_day <- lubridate::day(this_reference_day)
  this_date_year <- lubridate::year(this_reference_day)

  # inititialize matrix with 0s
  this_start <- c(this_date_year[1], lubridate::month(this_reference_day[1]))
  this_reg_matrix <- ts(matrix(0, ncol=12, nrow=length(this_reference_day)),
                        start = this_start, frequency = 12)
                        
  # set indicator for weekday of the first day of the month
  for (i in 1:length(this_reference_day)) {
     this_reg_matrix[i,this_date_day[i]] <- 1
  }
  
  if (is.null(collapse_col)) {
    final_ncol <- 11
  } else {
    temp_col <- array(0, dim = length(this_reference_day))
    for (i in 1:length(this_reference_day)) {
      for (j in 1:collapse_col) {
        temp_col[i] <- temp_col[i] + this_reg_matrix[i,j]
      }
    }
    this_reg_matrix <- cbind(temp_col, this_reg_matrix[,(collapse_col+1):12])
    final_ncol <- ncol(this_reg_matrix)
  }
  
  if (!contrast_reg) {
    if (is.null(collapse_col)) {
      colnames(this_reg_matrix) <- paste0("rws", 1:final_ncol)
    } else {
      colnames(this_reg_matrix) <- 
        c(paste0("rws1-", collapse_col), paste0("rws", collapse_col + 1:(final_ncol - 1)))
    }
    return(this_reg_matrix)
  }
  
  final_ncol <- final_ncol - 1
  final_reg_matrix <- ts(matrix(0, ncol=final_ncol, nrow=length(this_reference_day)),
                         start = this_start, frequency = 12)
  
  for (i in 1:final_ncol) {
    final_reg_matrix[,i] <- this_reg_matrix[,i] - this_reg_matrix[,(final_ncol+1)]
  }

  # mean adjust regressors
  if (!is.null(reg_means)) {
    for (i in 1:final_ncol) {
      final_reg_matrix[,i] <- final_reg_matrix[,i] - reg_means[i]
    }
  }
  
  if (is.null(collapse_col)) {
    colnames(final_reg_matrix) <- paste0("rws", 1:final_ncol)
  } else {
    colnames(final_reg_matrix) <- 
      c(paste0("rws1-", collapse_col), paste0("rws", collapse_col + 1:(final_ncol - 1)))
  }
  
  return(final_reg_matrix)
}