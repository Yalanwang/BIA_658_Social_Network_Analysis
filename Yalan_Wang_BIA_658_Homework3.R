# BIA 658 Social Network Analysis Homework3
# Yalan Wang
# Write a program in R that prints the numbers from 1 to 100. 
# multiples of three print "Fizz" instead of the number and 
# multiples of five print "Buzz". 
# For numbers which are multiples of both three and five print "FizzBuzz"

Multiple_3_5 <- function(x){
  if (x %% 3 ==0 & x %% 5 == 0){
    print("FizzBuzz")
  }
  else if (x %% 3 == 0) {
    print("Fizz")
  }
  else if (x %% 5 == 0){
    print("Buzz")
  }
  else{
    print(x)
  }
}

x <- 100
for (i in 1:x){
  Multiple_3_5(i)
}
