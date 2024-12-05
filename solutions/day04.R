# Today I wanted to do it different from the straightforward hardcoded stuff (at least star 1).
# For star 1 the horizontal and vertical ones are simple regex. I make a matrix of all the separated characters and paste the rows and cols, and then search XMAS or SAMX.
# For the diagonals I paste the top left diagonal lines (starting from at least 4 characters long) to the center (the longest diagonal from bottom left to top right).
# To find the indexes of the diagonal lines I use a sequence function.
# Then I turn the matrix by 90 degrees, 3 more times and do the same. The only thing you have to take into account is to not count the middle diagonals double
# to prevent that I use lines <- c(n, n, n - 1, n - 1) but this could probably be a bit more elegant.

input <- readLines("input04.txt")
n     <- length(input)
mt    <- matrix(unlist(strsplit(input, "")), nrow = n, byrow = T)

# Star 1
pattern <- "(?=XMAS|SAMX)"
count   <- 0

# Horizontals and Verticals
count <- count + sum(sapply(1:2, function(y){
  apply(mt, y, function(x){sum(gregexpr(pattern, paste0(x, collapse = ""), perl = T)[[1]] > 0)})
  }))

# Diagonals
rotate <- function(x){t(apply(x, 2, rev))}

lines <- c(n, n, n - 1, n - 1)
for(i in 1:4){
  for(j in 4:lines[i]){
    chars <- paste0(mt[seq(j, (1 + (j - 1) * n), n -1)], collapse = "")
    l     <- sum(gregexpr(pattern, chars, perl = TRUE)[[1]]> 0)
    count <- count + l
  }
  mt <- rotate(mt)
}

cat("Day 4 Star 1:", count)

# Star 2
mt      <- cbind(".", rbind(".", mt, "."), ".")
A       <- which(mt == "A", arr.ind = TRUE)
pattern <- "MAS|SAM"

xmas <- apply(A, 1, function(z){
  verticals <- sapply(c(-1, 1), function(d) 
    paste0(mt[z[1] + d, z[2] - 1], mt[z[1], z[2]], mt[z[1] - d, z[2] + 1], collapse = "")
  )
  all(grepl(pattern, verticals))
})

count <- sum(unlist(xmas))
cat("Day 4 Star 2:", count)
