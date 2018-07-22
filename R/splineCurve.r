
SplineCurveAndDerivative <- function(YValues, XValues,xlabel="x",ylabel="y",mainlabel="main"){
  if(length(discountMargin)!=length(maturity)){
    stop("Number of elements in maturity and discount margin must be the same")
  }

  #spline

  spl <- smooth.spline(discountMargin~maturity,all.knots = TRUE)
  par(mfrow=c(2,1))
  plot(spl,ylab = ylabel ,xaxt='n',xlab = xlabel,main = mainlabel)
  lines(spl)
  if(class(maturity) == "Date"){
    axis.Date(side = 1,maturity,format = '%m/%Y',labels = T,at=maturity,las=2)
  }
  sx <- seq(from=head(maturity,1),to = tail(maturity,1),by = (tail(maturity,1)-head(maturity,1))/100)
  pred.prime<-predict(spl,as.numeric(sx),deriv=1)
  d1 <- data.frame(pred.prime)
  plot(d1,type = "l",xaxt='n',xlab=xlabel,ylab="derivative")
  if(class(maturity) == "Date"){
    axis.Date(side=1,sx,format = "%m/%Y",labels = T,at = maturity,las=2)
  }
  recordedPlot =recordPlot()
  return(recordedPlot)
}
