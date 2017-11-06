#install.packages("formatR")
#formatR::tidy_dir("R")

#install.packages("lintr")
#lintr::lint_package()
#------------------1
# find a suitable and permitted <pkg_name> for your package.
#check on CRAN if the name is already taken.
#name : pssi     pefeckt solved statistic issue

#----------2
#create the structure for the package via devtools:: create('../folder/pkg_name')
#devtools::create("~/hello-world/U2/psy")
#-----------3
#put your function checkHeight() from the previous sessions in the created /R folder.
library(dplyr)


age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")

students = data.frame(cbind(age, weight, height, sex))
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))

students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")

students


#help(package = pssi)
# ?pssi_data
# ?checkHeight
# save(students,file = "data/passi_data.rda")

checkHeight3 = function(students.input = students){
  # prepare result data.frame
  result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
  colnames(result.frame) = c("name", "difference")
  result.frame$name <- students.input$name

  # calculate sex means for height
  male.mean = students.input %>%
    filter(sex == "M") %>%
    summarise(mean = mean(height))
  female.mean = students.input %>%
    filter(sex == "F") %>%
    summarise(mean = mean(height))

  # calculate the difference and store it the result data.frame
  result.frame$difference <- apply(students.input, 1, function(stud) {
    ifelse(stud["sex"] == "M",
           male.mean - as.numeric(stud["height"]),
           female.mean - as.numeric(stud["height"])
    )})

  return(result.frame)
}
checkHeight3(students.input = students)

#mean <- pssi::mean()
#-----------------4
# implement the option to calcaulte the difference from the sex-specific or
# the overall mean height
# the funciton should have a signature like this:
# checkHeight = function(students.input,sex.specific=TRUE)


#' @title  calcaulte the difference from the sex-specific or the overall mean height
#'
#' @description calculate
#' @param students.input the student dataframe
#' @param sex.secific the student sex
#' @export
#' @include utils.R
#' @seealso \code{\link[utils]{head}}
#' @return result.frame
#' @examples
#'checkHeight(students.input = students,sex.specific=FALSE)
#'checkHeight(students.input = students,sex.specific=TRUE)



checkHeight = function(students.input,sex.specific){
  # prepare result data.frame
  result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
  colnames(result.frame) = c("name", "difference")
  result.frame$name <- students.input$name

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

  return(result.frame)
}
checkHeight(students.input = students,sex.specific=FALSE)


#------------5
#add a welcome text like Welcome to my first R-package and thanks for using it.muchacho!
#after your package is loaded
# Hint : use .onAttach()
# and packageStartupMessage()

.onAttach <- function(libname,pkgname){
  packageStartupMessage("Welcome to my first R-package and thanks for using it, muchacho!")
}

#-----------6
#Add meaningful Descriptions to the corresponding file that devtools already created for you.
#Rememner,that we use the package dplyr to calculate the sex-specific means  within the function.
#Thus,include dplyr as required Imports:.

#-----------7
#Document your function using the roxygen2 package.
#1) Manually add roxygen2 style comments on your function in the folder R
#2) use the command load_all() from devtools to source the functions from your package
#devtools::load_all()
#3) use devtools:document() to automatically update the man/ and the NAMESPACE file.
#here it is important that your working direcotry is set to your package'path
#devtools::document()
#4) preview the documentation with ? checkHeight().
#your should receive the well-known R-style documentation for your funtion
#?checkHeight()
#5)repeat that process until you like what you see
#6) imagine your would have to write this -Rd files by yourself. Roxygen2 is lit af!

#--------------8
#for any reason ,we want our own mean() function to be used with checkHeight().
#This functin should round the returned value on three decimals and should just be used
#internally and thus not exported.
#create a new script utils.R in which you store this little,self-written helper function.
#Document it for yourself in roxygen2-style.

#-------------9
#check,if your self-written mean() function is used by checkHeight().
#Hint: you check the enviroment of a function via environment(function)
#envoriment(mean)
#-------10
#make sure,that only checkHeight() is getting exported
#-------------11
#updata the NAMESPACE file again via devtools::document()

#--------------12
#add the students.csv to your package as example data.
#therefore export the dataframe from the lst exercise as a.rda objec
# and store it in a sub-folder data.
#also ,include a description as a script in R/ with roxygen2 comments
#updata evetything via devtools:document()

write.csv(students,file = "extdata/students.csv",row.names = FALSE)
stu <- read.csv(file = "extdata/students.csv")
stu
#system.file("students", "students.csv", package = "pssi")
devtools::use_data(stu,overwrite = TRUE)

#-------------13
# run a check on your whole package via devtool's check() function
#hint: make sure,you are in the package's working directory
#try to understand the output and correct your code accordingly
#hint : do not get confused by notes concerning the variable names
#like no visible binding for global variable 'sex'
#this is due to the use of dplyr and cannot be mitigated

#devtools::check()


#14. put your package on Github and add an informative README.md

#15.Install your package via devtools:: install_github("<Username>/<Package name>")
# and test its functionality on the provided students data set
# also check the help description

#devtools::release()

devtools::install_github("haijunXue/Exercise3")
