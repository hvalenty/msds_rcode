# Task 10.1
fib <- numeric(10)
fib[1] <- fib[2] <- 1
for (i in 3:10) {
  fib[i] <- fib[i - 2] + fib[i - 1]
}
fib

# Task 10.2
fib_func <- function(lenny) {
  fib <- numeric(10)
  fib[1] <- fib[2] <- 1
  for (i in seq_along(lenny)) {
    fib[i] <- fib[i - 2] + fib[i - 1]
  }
  return(fib)
}
fib_func(10)

#Task 10.3
rangefinder <- function(x) {
  if (0 <= x & x <= 1){
    return(1)
  } 
  else {
    return(0)
  }
    
}
rangefinder(0.26)

#Task 10.4
rangefinder(0.26)
rangefinder(-4)
rangefinder(5)
rangefinder(1)
rangefinder(0)

#Task 10.5
letters[1:10]
LETTERS[17:26]
#OR
print(tail(LETTERS, 10))
LETTERS[22:24]

#Task 10.6
counter <- (1:100)
print(counter)

for (i in counter){
  if (i%%3 == 0 & i%%5 != 0) {
    print("Fizz")
  }
  else if (i%%3 != 0 & i%%5 == 0) {
    print("Buzz")
  }
  else if (i%%3 == 0 & i%%5 == 0) {
    print("FizzBuzz")
  }
  else print(paste(i))
}

#Task 10.7
messy <- "Happy Birthday Joseph Happy Birthday Hannah"
mes2 <- unlist(strsplit(tolower(messy), ' '))
print(mes2)
print(unique(mes2))

numb <- c(1,2,2,2,3,4,5,6)
print(unique(numb))

#Task 10.8
vec <- c(2,5,6,4,7,8,9,5,55,0)
max(vec)
min(vec)

#Task 10.9
seq(20,50)
mean(seq(20,60))
sum(seq(51,91))

#Task 10.10
help("data.frame")
ch <- c('m','n','o','p','q')
flo <- c(8.5,6.6,3.2,4.1,0.8)
intg <- c(5,6,8,4,7)
boo <- c(T,T,F,T,T)

df <- data.frame(ch, flo, intg, boo)
df

#Task 10.11
airquality
airquality[c(3,5),c(1,3)]

#Task 10.12
mtcars
plot(mtcars[c('mpg','cyl','hp')])
plot(mtcars[ , c('mpg','cyl','hp')])

#Some Notes

#assign a global locally with <<-
#don't reassign variables, don't mess with it?
#factor function to cast into levels

#HW10 Hints
# data-centric programming
#wants multi index for final project