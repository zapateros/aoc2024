input <- readLines("input02.txt")

# StarLord 1
reports <- sapply(input, function(x){
  y <- diff(as.numeric(unlist(strsplit(x, " "))))
  all(y %in% c(1:3)) | all(y %in% -c(1:3))
})
n <- sum(reports)
cat("Day 2 star 1:", n)

# StarBoy 2
reports <- sapply(input, function(y){
  report <- as.numeric(unlist(strsplit(y, " ")))
  safe <- sapply(1:length(report), function(x){
    y <- diff(report[-x])
    all(y %in% c(1:3)) | all(y %in% -c(1:3))
  })
  any(safe)
})
n <- sum(reports)
cat("Day 2 Star 2:", n)
