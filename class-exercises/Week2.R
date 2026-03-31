#metrix 

mat <- matrix(
  c(1, 2, 3, 4, 5,6),
  nrow = 2,
  byrow = TRUE
  
  
  
)
mat
mat[1,2] # first raw second value
mat [,2] # get only second column
mat [1,] # get first raw
mat [2] #second row 1 value

#list we can use = inside of list 

student <- list(
  name = "Joe",
  age = 24,
  marks = c(55, 92, 78)
  
)

student
student$age

#data frame

students <- data.frame(
  name = c("Joe", "Zainab", "Ragini"),
  age = c(24, 27, 27),
  marks = c(50, 90, 85)
  
)
students$marks

students[1,2]

students[, "marks"]

str(students)
students[1,]

nrow(students)#how many rows
ncol(students)#how many col

mean(students$marks)

students$marks >= 60 #it give true false results

#CONDITIONAL INDEXING
passedStudents <- students[students$marks >= 60, ]#give all the records of students who passed over 60
passedStudents

passedMarks <- students$marks[students$marks >= 60]
passedMarks


for (i in students$marks){
  print(i)
}

#call the function we declare in another file
source("Test2.R")
addTwoNumbers(2,3)

results <- c(1,2,3,4,5,6,7,8,9,0,9,8,7,6,5,4,3,2,1)
results 

write.csv(results, "results.csv", row.names = FALSE)

readResults <- read.csv("results.csv")
head(readResults)
