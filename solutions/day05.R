input <- readLines("input05.txt")

rules     <- setNames(data.frame(matrix(as.numeric(unlist(strsplit(input[grepl("\\|", input)], "\\|"))), byrow = T, ncol = 2)), c("page1", "page2"))
pagesList <- lapply(input[grepl(",", input)], function(x){as.numeric(unlist(strsplit(x, ",")))})
incorrect <- NULL

# Star 1
count <- 0
for(i in 1:length(pagesList)){
  pages  <- pagesList[[i]]
  middle <- pages[ceiling(length(pages)/2)]
  
  correct <- TRUE
  while(length(pages) > 1){
    page  <- pages[1]
    pages <- pages[-1] 
    pagesBefore <- rules$page1[rules$page2 == page]
    if(length(pagesBefore) > 0){
      if(any(pagesBefore %in% pages)){
        correct <- FALSE
        break
      }
    }
  }
  
  if(correct){
    count <- count + middle
  }else{
    incorrect <- c(incorrect, i)
  }
  
}
cat("Day 5 star 1:", count)

# Star 2
# This method is not fundamental. I noticed how the rules are written and used this. For example, if the pages are 1, 2, 3, 4, 5 in this order
# the the rules would be 1|2, 1|3, 1|4, 1|5, 2|3, 2|4, 2|5, 3|4, 3|5, 4|5. If you count the amount of occurences of the left numbers, the number with the most occurences will be the first number (1, 4 times)
# and the number that doesn't occur on the left is the last number (5, 0 times). This way you can order by amount of occurences and take the middle number.
# Again: this method only works because of the way the rules are written. If they are not complete, this method won't work.
middleNumbers <- sapply(incorrect, function(x){
  pages <- pagesList[[x]]                                             # pages, 81 47 79 63 54 42 87 17 37 59 66 ..
  rule  <- rules[rules$page1 %in% pages & rules$page2 %in% pages,]    # the rows of the rules that are relevant
  tb    <- table(rule$page1)                                          # count of the amount occurrences of the pages in the left column of rules
  
  as.numeric(names(tb[order(tb)][length(tb)/2]))                      # Take the pagenumber that has the middle amount of occurences
})
count <- sum(middleNumbers)
cat("Day 5 star 2:", count)
