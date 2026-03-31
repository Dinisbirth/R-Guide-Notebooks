# Task 1 – Advanced Calculator (Programming Task)
# Objective:
#   Write a calculator program 
# 
# Requirements:
#   - Store two numeric values
# - Store an operation (+, -, *, /)
# - Use a custom function
# - Validate the operation
# - Prevent division by zero
# - Print a clear result or error message

number1 <- readline("Please enter your first number: ")
number1 <- as.integer(number1)
print(number1)

number2 <- readline ("Please enter your second number: ")
number2 <- as.integer(number2)
print (number2)

operator <- readline ("Please enter your method +, -, x, /: ")

calculate <- function(number1, number2, operator) {
  if (operator == "+") {
    return(number1 + number2)
  } else if (operator == "-") {
    return(number1 - number2)
  } else if (operator %in% c("*", "x")) {
    return(number1 * number2)
  } else if (operator == "/") {
    if (number2 == 0) {
      stop("Division by zero is not allowed")
    }
    return(number1 / number2)
  }

  stop("Wrong input Please check your numbers or method")
}

result <- tryCatch(
  calculate(number1, number2, operator),
  error = function(e) e$message
)

print(result)


#Task 2 – Grade Classification Program (Programming Task)
#Objective:
  #Write a program that classifies marks into grade categories.

#Requirements:
#- Store a vector of marks
#- Use a custom function to classify grades
#- Use a loop to process each mark
#- Print the mark and its classification

marks <- c(95, 82, 67, 54, 41, 38)

grade <- function(mark){
  if (mark >= 75)
    {
    return("Distinction")
  } 
  else if (mark >= 60) 
    {
    return("Merit")
  } 
  else if (mark >= 40)
    {
    return("Pass")
  } 
  else 
    {
    return("Fail")
  }
}

# Use a loop to process each mark
for (m in marks)
  {
  g <- grade(m)
  
  # Print the mark and its classification
  cat("Mark:", m, "-> Grade:", g, "\n")
}

#Task 3 – Debugging Task: Number Analysis Program
#Objective:
  #Fix the errors in the script below so it correctly identifies even and odd numbers.
#Broken script:

  numbers <- seq(1, 10)

is_even <- function(x) {
  if (x %% 2 == 0) {
    return(TRUE)
  } 
  else {
    return(FALSE)
  }
}

for (i in numbers) { #inside the loop, test i not numbers "if (is_even(numbers))"
  if (is_even(i))
    {#combine number + text
    print(paste(i, "is even"))
  } 
  else {
    print(paste(i, "is odd"))
  }
}


#Task 4 – Debugging Task: Temperature Converter
#Objective:
  #Fix the program so it correctly converts Celsius to Fahrenheit.

#Broken script:

  temps <- c(0, 10, 20, 30)

convert_temp <- function(celsius) {
  fahrenheit <- celsius * 9 / 5 + 32
  #need return value
  return(fahrenheit)
}

for (t in temps) {
  result <- convert_temp(t) #need t not temps
  print(paste("Celsius:", t, "Fahrenheit:", result))
}


