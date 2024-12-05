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
middleNumbers <- sapply(incorrect, function(x){
  pages <- pagesList[[x]]                                             # pages, 81 47 79 63 54 42 87 17 37 59 66 ..
  rule  <- rules[rules$page1 %in% pages & rules$page2 %in% pages,]    # the rows of the rules that are relevant
  tb    <- table(rule$page1)                                          # count of the amount occurrences of the pages in the left column of rules
  
  as.numeric(names(tb[order(tb)][length(tb)/2]))                      # Take the pagenumber that has the middle amount of occurences
})
count <- sum(middleNumbers)
cat("Day 5 star 2:", count)
