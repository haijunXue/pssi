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
mean <- function(x){
  sum = 0
  for (i in 1:length(x)){
    sum = x[i]+sum
  }

  return (round(sum/length(x),digits = 3))
}
#x = c(1,2,3)
#x = students$height
#c <- mean1(x)
#c



