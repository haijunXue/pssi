source("Exercise1.R")
library(dplyr)
library(checkmate)


test_that("the output of the students ",{
  #out <- checkHeight(students.input = students,sex.specific=FALSE,print.statement = FALSE)

 # expect_that(nrow(out),equals(8))
  #compare(nrow(out),nrow(students.input))
 # expect_equal(nrow(out),nrow(students.input))

  students$height = 2.0
  out1 <- checkHeight(students.input = students,sex.specific=TRUE,print.statement = FALSE)
  out2 <- checkHeight(students.input = students,sex.specific=FALSE,print.statement = FALSE)
  #expect_match("out1",out2)
  compare(out1,out2)
})

#expect_that(1/"a",throws_error("nicht-numerisches Argument für binären Operator"))


Sys.setenv(LANG = "en")
expect_failure(
  expect_error( regexp =checkHeight(students.input = students,sex.specific=TRUE), class = "checkHeight",ignore.case = TRUE),
  "threw an condition with unexpected class"
)
#stop("variable <paramter> not found"),
#-----------3.4

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

  students_test <- students
  levels(students_test[,4] <- c("F","M","C"))
  expect_error(checkHeight(students_test,TRUE,FALSE))
})

colnames(test.data)[which(names(test.data) == "age")] <- "test"
expect_that(checkHeight(test.data), throws_error("Variable <age> not found."))
#----5
test_that("print checking", {
  expect_output(checkHeight(students,sex.specific = TRUE, print.statement = TRUE), "Yippie, I calculated the mean differences!")
})

#--6
#testthat::test_dir("tests/testthat/")
