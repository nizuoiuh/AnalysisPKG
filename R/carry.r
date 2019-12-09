
#' Function to calculate carry
#'
#' @param price - price of bond
#' @param yield - yield of bond
#' @param numberOfDays - number of days to calculate carry
#' @param financingRate - financing rate
#' @param maturity -maturity of bond
#' @param name - name of bond
#'
#' @return returns list of couponIncome, financingCharge, carry, name, maturity
#'
#' @examples
#'
#'
#'
carry <- function(price,yield,numberOfDays,financingRate,maturity,name){
  couponIncome <-yield*numberOfDays/365

  financingCharge <- price*financingRate * numberOfDays/365
  ret <- list()
  ret$couponIncome <- couponIncome
  ret$financingCharge <- financingCharge

  ret$carry <- couponIncome - financingCharge
  ret$name <- name
  ret$maturity <- maturity
  return(ret)
}



