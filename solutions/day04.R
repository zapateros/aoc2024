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
