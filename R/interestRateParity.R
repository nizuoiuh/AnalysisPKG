



coveredIP <- function(irB=NULL,irQ=NULL,startDate,endDate,points=NULL){
  numberOfDays <- endDate - startDate
  if(is.null(irB) && !is.null(irQ) && !is.null(points)){
  }
  else if(is.null(irQ) && !is.null(irB) && !is.null(point)){

  }
  else if(is.null(points) &&is.null(irQ) && is.null(irB)){

  }
  else{
    warning("two out of three parameters should be specified : irB, irQ, points")
  }
}
