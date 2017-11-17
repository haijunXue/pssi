source("Exercise1.R")
library(dplyr)
library(checkmate)


test_that("the output of the students ",{
  out <- checkHeight(students.input = students,sex.specific=FALSE,print.statement = FALSE)

 # expect_that(nrow(out),equals(8))
  #compare(nrow(out),nrow(students.input))
  expect_equal(nrow(out),nrow(students.input))

  students$height = 2.0
  out1 <- checkHeight(students.input = students,sex.specific=TRUE,print.statement = FALSE)
  out2 <- checkHeight(students.input = students,sex.specific=FALSE,print.statement = FALSE)
  #expect_match("out1",out2)
  compare(out1,out2)




})

Sys.setenv(LANG = "en")
expect_failure(
  expect_error(stop("xxx"), regexp =checkHeight(students.input = students,sex.specific=TRUE), class = "checkHeight",ignore.case = TRUE),
  "threw an condition with unexpected class"
)

#-----------3.4
assertDataFrame(x=students.input, min.rows = 4,ncols = 5, types = c("numeric", "numeric", "numeric", "factor", "character"),any.missing = FALSE)
assertNumeric(x = students.input[,"height"],lower = 1.30,upper = 2.40,finite = FALSE,any.missing = FALSE)
assertFactor(x= students.input[,"sex"],levels = c('F','M'),max.levels = 2)
assertNumeric(x = as.integer(students.input[,'height']),lower = 1.30,upper = 2.40,finite = FALSE,any.missing = FALSE)

test_that("variable checks", {
 expect_error(checkHeight(students,sex.specific = "XX"),
             "Assertion on 'sex.specific' failed: must be type 'logical'." )
  expect_error(checkHeight(students,sex.specific= TRUE,print.statement  = TRUE),
               "Assertion on 'print.statement' failed: must be type 'logical'." )
  expect_error(students$height,
               "failed: Must be of type 'numeric', not 'character'.")
  expect_error(students$sex,
               "failed: Must be of type 'factor', not 'character'.")
  f<- function(x){
    if (x < 1.30) stop("error x")
    if(x>2.40) stop("error")
  }
  expect_error(f(as.integer(students$height)),
               "failed: Must gross than 1.30 und smaller than 2.40.")


})

#----5
test_that("print checking", {
  expect_output(checkHeight(students,sex.specific = TRUE, print.statement = FALSE), "Yippie, I calculated the mean differences!")
})

#--6
testthat::test_dir("tests/testthat/")
