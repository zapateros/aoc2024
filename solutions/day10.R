# I solved today with a couple of different approaches. This one is the fastest (~0.05s). First i Calculate a adjacency list for every
# point on a map where the neighbouring elevation is incremental. I used 1D indices instead of my usual 2D ones.
# From every zero as starting point I do recursion until I get to a 9 and output the index of it everytime a trail reaches it
# For star 1 I just count the amount of unique reached 9's per zero and for star 2 you just count every trail.

input <- readLines("input10.txt")
time <- Sys.time()
n   <- length(input)
map <- matrix(as.numeric(unlist(strsplit(input,""))), byrow = T, nrow = n)

# Function that get (1d) indices of neighbours based on index number and amount of rows
get_neighbours <- function(index, nrows){
  neighbour_indices <- NULL
  
  # Add left, right, bottom, top respectively
  if(index > nrows) neighbour_indices <- c(neighbour_indices, index - nrows)
  if(index <= (nrows * (nrows - 1))) neighbour_indices <- c(neighbour_indices, index + nrows)
  if(index %% nrows != 0) neighbour_indices <- c(neighbour_indices, index + 1)
  if(index %% nrows != 1) neighbour_indices <- c(neighbour_indices, index - 1)
  
  neighbour_indices
}

# Create adjacency list of the map where neighbour is 1 higher
adjacency_list <- lapply(1:(n * n), function(x){
  height <- map[x]
  if(height < 9){
    neighbours <- get_neighbours(x, n)
    neighbours[map[neighbours] == (height + 1)]
  }
})

# Function to check every trail from a certain point recursively. It outputs the point of elevation 9 everytime a trail leads to it
hike <- function(point, m = NULL){
  if(map[point] == 9){
    m <- c(m, point)
  }else{
    next_points <- adjacency_list[[point]]
    if(length(next_points) > 0){
      
      for(x in next_points){
        m <- hike(x, m)
      }
    }
  }
  return(m)
}

# All starting points
zeros <- which(map == 0)

# Run the hiking trails for every starting point
all_trails <- lapply(zeros, function(x){
  hike(x)
})

# Star 1:
scores <- sapply(all_trails, function(x) length(unique(x)))
cat("Star 1:", sum(scores))

# Star 2:
ratings <- sapply(all_trails, function(x) length(x))
cat("Star 2:", sum(ratings))
Sys.time() - time
