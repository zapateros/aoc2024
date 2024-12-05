input <- readLines("input01.txt")
lists <- matrix(as.numeric(unlist(strsplit(input, "   "))), ncol = 2, byrow = T)

# Star 1
n <- sum(abs(lists[,1][order(lists[,1])] - lists[,2][order(lists[,2])]))
cat("day 1 star 1:", n)

# Star 2
n <- sum(sapply(lists[,1], function(x){sum(lists[,2] == x) * x}))
cat("day 1 star 2:", n)
