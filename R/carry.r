
carry <- function(price,yield,numberOfDays,financingRate){
  couponIncome <-yield*numberOfDays/365

  financingCharge <- price*financingRate * numberOfDays/365
  ret <- list()
  ret$couponIncome <- couponIncome
  ret$financingCharge <- financingCharge

  ret$carry <- couponIncome - financingCharge

  return(ret)
}



