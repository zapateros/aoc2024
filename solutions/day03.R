input <- readLines("input03.txt")

# Star 1
muls    <- unlist(regmatches(input, gregexpr("mul\\(\\d+,\\d+\\)", input)))
numbers <- matrix(as.numeric(unlist(strsplit(gsub("mul\\(|\\)", "", muls), "\\,"))), ncol = 2, byrow = T)
n       <- sum(numbers[,1] * numbers[,2])
cat("Day 3 star 1:", n)


# Star 2
multiplications <- unlist(regmatches(input, gregexpr("mul\\(\\d+,\\d+\\)|don't\\(\\)|do\\(\\)", input)))

on <- TRUE
n  <- 0
for(i in 1:length(multiplications)){
  instruction <- multiplications[i]
  if(instruction == "do()"){
    on <- TRUE
  }else if(instruction == "don't()"){
    on <- FALSE
  }else{
    if(on){
      sm <- prod(as.numeric(unlist(strsplit(gsub("mul\\(|\\)", "", instruction), ","))))
      n  <- n + sm
    }
  }
}
cat("Day 3 star 2:", n)
