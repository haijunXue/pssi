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
#' @param print.statement
#' @import checkmate
#' @export
#' @include utils.R
#' @seealso \code{\link[utils]{head}}
#' @examples
#'checkHeight(students.input = students,sex.specific=FALSE)
#'checkHeight(students.input = students,sex.specific=TRUE)



checkHeight = function(students.input,sex.specific,print.statement){
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


  print.data.frame(result.frame)
  if(print.statement){
    print("Yippie, I calculated the mean differences!")
  }
 # return(result.frame)


}
checkHeight(students.input = students,sex.specific=FALSE,print.statement = TRUE)

library(checkmate)
#---4-1-3
#assert : Combine multiple checks into one assertion
#x = 1
#assert(checkChoice(x, c("a", "b")), checkDataFrame(x))
#AssertCollection: Collect multiple assertions
#makeAssertCollection()
#reportAssertions(collection)
#----4-1-4(1)
# check if the variable that controls the sex specificity of
#the mean calculation is boolean(Hint:assertLogical)
sex.specific = TRUE
assertLogical(x = sex.specific,len=1L)
#2)
print.statement = FALSE
assertLogical(x=print.statement,len=1L)
#3)
students.input = students
assertDataFrame(x=students.input, min.rows = 4,ncols = 5, types = c("numeric", "numeric", "numeric", "factor", "character"),any.missing = FALSE)
#4)
apply(students.input[3],MARGIN = 2,FUN = function(h){
  assertNumeric(x = h,lower = 1.30,upper = 2.40,finite = FALSE,any.missing = FALSE)
  })
#5)
apply(students.input[4],MARGIN = 2,FUN = function(sex){
  assertFactor(x = factor(sex),max.levels = 2,levels = c("F","M"))
})

#1-5



#assertNumeric(lower = 1.30,upper = 2.40,finite = FALSE,len = 3,any.missing = FALSE)
#------------5
#add a welcome text like Welcome to my first R-package and thanks for using it.muchacho!
#after your package is loaded
# Hint : use .onAttach()
# and packageStartupMessage()

.onAttach <- function(libname,pkgname){
  packageStartupMessage("Welcome to to  my first R-package and thanks for using it, muchacho!")
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

devtools::install_github("haijunXue/pssi",force=TRUE)


# Exercise 2
#1.what is the difference between a package and a library
#Packages are collections of R functions, data, and compiled code in a well-defined format. The directory where packages are stored is called the library.
#2.2. What is the difference between source() of scripts and the use of packages?
#source causes R to accept its input from the named file or URL or connection or expressions directly
# Input is read and parsed from that file until the end of the file is reached, then the parsed expressions are evaluated sequentially in the chosen environment.
#library and require load and attach add-on packages.
#3. In which 5 statuses can packages be?

#4
#Depends: list here all packages that are required by your package including version if needed (checked if the packages are installed when calling R CMD INSTALL)
#Suggests: list here packages that are used in the vignettes (when calling R CMD check)
#SystemRequirements: list package required but not automatically installed when running R CMD INSTALL

#5. Which open sources licenses could be used for your package? How do they mainly differ?
#MIT (v. similar: to BSD 2 and 3 clause licenses). This is a simple and permissive license. It lets people use and freely distribute your code subject to only one restriction: the license must always be distributed with the code
#GPL-2 or GPL-3. These are “copy-left” licenses. This means that anyone who distributes your code in a bundle must license the whole bundle in a GPL-compatible way. Additionally, anyone who distributes modified versions of your code (derivative works) must also make the source code available. GPL-3 is a little stricter than GPL-2, closing some older loopholes
#CC0. It relinquishes all your rights on the code and data so that it can be freely used by anyone for any purpose. This is sometimes called putting it in the public domain, a term which is neither well-defined nor meaningful in all countries.
#6. What is the difference between loading and attaching?
#Packages whose namespace only is needed to load the package using library(pkgname) must be listed in the ‘Imports’ field and not in the ‘Depends’ field.
#Packages that need to be attached to successfully load the package using library(pkgname) must be listed in the ‘Depends’ field, only.

#7.Why should you always use library() in data analysis scripts and not require()?
#library() which automatically load the package if needed

#8Why should you never use library() in a package

#9. What does lazy loading of data via LazyData: mean? Why is it useful?
#LazyData makes it easier to access data in your package. Because it’s so important, it’s included in the minimal description created by devtools. It’s described in more detail in external data.

#10. How can you find the perfect compression type for the .rda objects in your package? Why should you
#want to know this?
#

#Run tools::checkRdaFiles() to determine the best compression for each file.

#Re-run devtools::use_data() with compress set to that optimal value. If you’ve lost the code for recreating the files, you can use tools::resaveRdaFiles() to re-save in place.
