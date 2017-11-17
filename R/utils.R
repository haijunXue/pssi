#' mean value of the vector elements
#'
#' \code{mean} returns the mean of all the values present in its arguments
#'
#' This is a generic function: methods can be defined for it directly or via the
#' \code{\link{mean}} group generic.
#' @param  x the array input

#' @examples
#' mean(c(1,2,3))

#' @aliases meanfunction
#' @return  the mean value
#' @import checkmate

mean <- function(x){
  assertNumeric(x ,lower = 1.30,upper = 2.40,finite = FALSE,any.missing = FALSE)
  checkVector(x,any.missing = FALSE)
 # checkDate(x,any.missing = FALSE)
  #checkMatrix(x,any.missing = FALSE)
  checkFALSE(x, na.ok = FALSE)

 assert( checkVector(x,any.missing = FALSE),assertNumeric(x ,lower = 1.30,upper = 2.40,finite = FALSE,any.missing = FALSE))
  return (round(sum(x)/length(x),digits = 3))
}

#mean(students$height)
#mean(c(0,0,0))
#mean <- function(x){
#  sum = 0
 # for (i in 1:length(x)){
  #  sum = x[i]+sum
  #}

  #return (round(sum/length(x),digits = 3))
#}
#x = c(1,2,3)
#x = students$height
#c <- mean1(x)
#assertList(list(x),types = "double")
#assertClass(x,classes = students[,'height'])
#checkScalar(x,na.ok = FALSE,null.ok = FALSE)
# assertFunction(x=mean )
# assertNumber(x , lower = 0, upper = 10000)

#checkArray(x,any.missing = TRUE,max.d = 1)
#anyNA(x)
# checkCount(x,na.ok = FALSE,positive = FALSE,tol = sqrt(.Machine$double.eps), null.ok = FALSE)





