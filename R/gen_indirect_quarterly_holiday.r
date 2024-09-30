#' Generate indirect quarterly holiday adjustments
#'
#' Generate indirect quarterly holiday factors and an indirect holiday adjusted series from monthly time series and 
#' monthly holiday factors.
#'
#' Version 2.0, 5/6/2024
#'
#' @param this_a1 Real array; ts object of the original series
#'                This is a required argument.
#' @param this_hol Real array; ts object of the holiday factors
#'                 This is a required argument.
#' @param ratio Logical scalar; if TRUE, holiday factors are assumed to be ratios; otherwise, the factors are assumed 
#'        to be on the same scale as the original series. Default setting is TRUE.
#' @return List object of two ts objects: \code{holadj}, which contains the indirect holiday adjusted quarterly series and 
#'         \code{holfac}, the indirect holiday factors.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' n2033157_hol_q_list <- gen_indirect_quarterly_holiday(n2033157_a1, n2033157_hol)
#' @import stats
#' @export
gen_indirect_quarterly_holiday <- function(this_a1 = NULL, this_hol = NULL, ratio = TRUE) {
    # Author: Brian C. Monsell (OEUS), Version 2.0, 5/6/2024
    
   if (is.null(this_a1)) {
       cat("must specify the original series")
       return(NULL)
   } else {
       # check if \code{this_a1} is a ts object
       if (!is.ts(this_a1)) {
           cat("must specify a ts object for the original series")
           return(NULL)
	}
   }    

   if (is.null(this_hol)) {
       cat("must specify the holiday factors")
       return(NULL)
   } else {
       # check if \code{this_hol} is a ts object
       if (!is.ts(this_hol)) {
           cat("must specify a ts object for the holiday factors")
           return(NULL)
	}
   }    

   if (ratio) {
      this_holadj   <- this_a1 / this_hol
   } else {
      this_holadj   <- this_a1 - this_hol
   }

   this_q        <- aggregate.ts(this_a1, nfrequency = 4)
   this_holadj_q <- aggregate.ts(this_holadj, nfrequency = 4)

   if (ratio) {
      this_holfac_q <- this_q / this_holadj_q
   } else {
      this_holadj_q <- this_q - this_holadj_q
   }

   return(list(holadj = this_holadj_q, holfac = this_holfac_q))
}
