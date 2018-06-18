rm(list = ls())

library(arules)
library(Hmisc)

tags <- read.transactions('cleaned/category_tags.txt', sep = ",", rm.duplicates = T)
tags_vector <- read.csv('output/tag_frequency.csv')
tags_vector <- c(as.character(tags_vector$category_tag))

tags_frequent <- apriori(tags, parameter = list(minlen = 2, maxlen = 4, target = "frequent itemsets", support = 0.0001))
#inspect(tags_frequent)
tags_frequent_frame <- as(tags_frequent, "data.frame")
tags_frequent_frame$items <- sort(tags_frequent_frame$items)


joins <- concat.split.multiple(tags_frequent_frame, split.cols = "items", ",")
joins$support <- NULL
joins$count <- NULL
joins$items_1 <- gsub("[{]", "", joins$items_1)
joins$items_1 <- gsub("[}]", "", joins$items_1)
joins$items_2 <- gsub("[{]", "", joins$items_2)
joins$items_2 <- gsub("[}]", "", joins$items_2)
joins$items_3 <- gsub("[{]", "", joins$items_3)
joins$items_3 <- gsub("[}]", "", joins$items_3)
joins$items_4 <- gsub("[{]", "", joins$items_4)
joins$items_4 <- gsub("[}]", "", joins$items_4)

joins <- joins[order(joins$items_1, joins$count, decreasing = T), ]

x <- aggregate(.~items_1, joins, toString)

for(i in 1:nrow(x)){
  str <- x$items_2[i]
  d <- gsub(" ", "", unlist(strsplit(str, split=",")))
  x$items_2[i] <- paste(unique(d), collapse = ', ')  
}

for(i in 1:nrow(x)){
  str <- x$items_3[i]
  d <- gsub(" ", "", unlist(strsplit(str, split=",")))
  x$items_3[i] <- paste(unique(d), collapse = ', ')  
}

for(i in 1:nrow(x)){
  str <- x$items_4[i]
  d <- gsub(" ", "", unlist(strsplit(str, split=",")))
  x$items_4[i] <- paste(unique(d), collapse = ', ')  
}
