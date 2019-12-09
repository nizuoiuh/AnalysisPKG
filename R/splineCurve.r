
#' Create spline curve and calculate 1st derivative
#'
#' @param YValues
#' @param XValues
#' @param xlabel
#' @param ylabel
#' @param mainlabel
#' @param ylim
#'
#' @return plot
#'
#' @examples
SplineCurveAndDerivative <- function(YValues, XValues,xlabel="x",ylabel="y",mainlabel="main",ylim=c(-0.5,0.5)){
  if(length(YValues)!=length(XValues)){
    stop("Number of elements in maturity and discount margin must be the same")
  }

  #spline

  spl <- smooth.spline(YValues~ XValues,all.knots = TRUE)
  par(mfrow=c(2,1))

  plot(spl,ylab = ylabel ,xaxt='n',xlab = xlabel,main = mainlabel)
  graphics::abline(h = 0, lty = 2)
  splData <- seq(min(XValues),max(XValues),(max(XValues)-min(XValues))/500)
  sx <- seq(from=head(XValues,1),to = tail(XValues,1),by = (tail(XValues,1)-head(XValues,1))/1000)
  lines(predict(spl,as.numeric(sx)))
  if(class(XValues) == "Date" || class(XValues)=="POSIXt"){
    axis.Date(side = 1,XValues,format = '%m/%Y',labels = T,at=XValues,las=2)
  }
  pred.prime<-predict(spl,as.numeric(sx),deriv=1)
  d1 <- data.frame(pred.prime)
  plot(d1,type = "l",xaxt='n',xlab=xlabel,ylab="derivative",ylim = ylim)
  graphics::abline(h = 0, lty = 2)
  if(class(XValues) == "Date"){
    axis.Date(side=1,sx,format = "%m/%Y",labels = T,at =XValues,las=2)
  }
  par(mfrow=c(1,1))
  recordedPlot =recordPlot()
  return(recordedPlot)

}
