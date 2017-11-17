


checkHeight1 = function(students.input,sex.specific,print.statement){
  # prepare result data.frame
  result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
  colnames(result.frame) = c("name", "difference")
  result.frame$name <- students.input$name

  #-----------------------
  assertLogical(x = sex.specific,len=1L)
  assertLogical(x=print.statement,len=1L)
  assertDataFrame(x=students.input, min.rows = 4,ncols = 5, types = c("numeric", "numeric", "numeric", "factor", "character"),any.missing = FALSE)
  assertNumeric(x = students.input[,"height"],lower = 1.30,upper = 2.40,finite = FALSE,any.missing = FALSE)
  assertFactor(x= students.input[,"sex"],levels = c('F','M'),max.levels = 2)
  assertNumeric(x = as.integer(students.input[,'height']),lower = 1.30,upper = 2.40,finite = FALSE,any.missing = FALSE)


  #---------------------------------
  # calculate sex means for height
  male.mean = students.input %>%
    filter(sex == "M") %>%
    summarise(mean = mean(height))
  female.mean = students.input %>%
    filter(sex == "F") %>%
    summarise(mean = mean(height))
  overall.mean = mean(students.input$height)

  # calculate the difference and store it the result data.frame
  result.frame$difference <- apply(students.input, 1, function(stud){
    if (sex.specific){
      ifelse(stud["sex"] == "M",
             male.mean - as.numeric(stud["height"]),
             female.mean - as.numeric(stud["height"])
      )
    } else {
      overall.mean - as.numeric(stud["height"])
    }
  })


  print.data.frame(result.frame)
  if(print.statement){
    print("Yippie, I calculated the mean differences!")
  }
  # return(result.frame)


}
#checkHeight1(students.input = students,sex.specific=FALSE,print.statement = TRUE)
checkHeight1(students.input = students, sex.specific = TRUE, print.statement = "a")
