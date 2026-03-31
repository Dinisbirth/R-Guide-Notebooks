#this is a comment

print("Hello World")

print (18)
print("CT7202")


#this is <-  the = in r
testing <- "Hello World"
print(testing)

testing <- 10.5
print(testing)

#boolian all need to capital
testing <- FALSE
print(testing)

#this give the type of the value
class(testing)


# without use print we can automatically run it but best practice
x <- 10
y <- 20

sum <- x + y
print (sum)

#otherwise 

x - y
x * y
x / y

x %% 2
x ^ 2
x %/% 2


x < y
x > y
x >= y


round(3.1456534, 2)

sqrt(16)

#absoulit positive 
abs(-16)

#vector
score <- c(65, 75, 45, 90)
score
#print(score)

length(score)
max(score)
min(score)


#vectorise operations
#adding 5 for each value in the vector
score + 5

students <- c("Joe", "Ragini", "Zainab")
students


score <- c(65, 75, 45, 90)
print(score)

passed <- score >= 60
print(passed)

#this is not good, keep int in one and keep string in one
mixed <- c(65, 75, "tewenty five")
print(mixed)

myFunction <- function(){
  print("Hellow world")
}

#argument
cleaning <- function(x){
  return(x * 2)
  
}

cleaning(10)

#if statment
cleaning <- function(mark){
  if (mark >= 60) {
    return("Merit")
  } else if (mark >= 50) {
    return("Pass")
  } else{
    return("Fail")
  }
}
cleaning(55)

#for
for (i in 1 : 5){
  print(i)
}

scores <- c(45, 55, 65, 75)

for (i in scores){
  print(i)
}

#while
counter <- 1
while (counter <= 10){
  print (counter)
  counter <- counter + 1
}



