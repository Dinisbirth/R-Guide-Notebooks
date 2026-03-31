addTwoNumbers <- function(a,b){
  return(a+b)
}

addTwoNumbers(2,3)


#1. Create a numeric vector containing at least five marks

marks <- c(35, 56, 78, 85, 90)
marks

#2. Create a list that stores:
#  - a student name
#- an age
#- the vector of marks you created above.

studen <- list (
  name = "Aurther",
  age = 25,
  marks = c(35, 56, 78, 85, 90)
)

#3. Create a data frame containing at least three students and their marks.

#Once created:
#- extract one value from the vector
#- extract one element from the list
#- extract one row and one column from the data frame
#- use at least two inspection functions (e.g. str, head, nrow) on the data frame.

#Comment your code to explain what each step is doing.

#create data frame
studentRecords <- data.frame(
  
  names = c("Aurther", "Merlin", "Modered"),
  marks = c(90, 75, 40)
)

#extract one value from the vector
studentRecords[1,2]

#- extract one element from the list (entire column)
studentRecords[1] # but this one is better
studentRecords$marks # we can do like this way as well 

#- extract one row and one column from the data frame
studentRecords[1, ]
studentRecords$marks

#use at least two inspection functions (e.g. str, head, nrow) on the data frame.
str(studentRecords)
head(studentRecords)
nrow(studentRecords)

#Task 2 – Filtering and Saving Results
#This task focuses on working with columns and saving results.

#1. Using your data frame from Task 1, identify students who have scored 60 or above.

passedStudents <- studentRecords[studentRecords$marks >= 60, ]
passedStudents

#2. Store the filtered results in a new object.


#3. Save this object to a CSV file in your project folder.

write.csv(passedStudents, "passedStudents.csv", row.names = FALSE)

#4. Read the CSV file back into R and print it.

readResults <- read.csv("passedStudents.csv")
head(readResults)



#Task 3 – Debugging: Data Structure Errors
#The script below contains several errors. Do not rewrite it from scratch.

#Your task is to:
#  - run the code
#- read the error messages
#- fix each issue step by step
#- add comments explaining what was wrong and how you fixed it.

#Broken script:
  
  students <- data.frame(
    name = c("Alice", "Bob", "Charlie"),   # FIX: name should be a character vector, so wrap values with c()
    mark = c(55, 68, 72)
  )
  
  passed <- students[students$mark >= 60, ]
  print(passed)
  
