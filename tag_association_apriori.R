rm(list = ls())

library(arules)
library(arulesViz)
library(Hmisc)
library(splitstackshape)
library(data.table)

tags <- read.transactions('cleaned/category_tags.txt', sep = ",", rm.duplicates = T)
tags_vector <- read.csv('output/tag_frequency.csv')
tags_vector <- c(as.character(tags_vector$category_tag))

tag_rules <- apriori(tags, parameter = list(support = .000005, confidence = 0.5, minlen = 2, maxlen = 5))
tag_rules <- sort(tag_rules, by = "count", decreasing = T)

plot(tag_rules, method = "graph")

tag_rules_frame <- as(tag_rules, "data.frame")


tag_rules_frame$fm <- paste0(tag_rules_frame$support, "+", tag_rules_frame$lift)
tag_rules_frame$fm2 <- Lag(tag_rules_frame$fm, shift = 1)
tag_rules_frame <- tag_rules_frame[tag_rules_frame$fm != tag_rules_frame$fm2, ]
tag_rules_frame$fm <- NULL
tag_rules_frame$fm2 <- NULL
write.csv(tag_rules_frame, file = "output/tag_rules_frame_one_to_one.csv")

joins <- data.table(tag_rules_frame$rules, tag_rules_frame$count)
joins <- concat.split.multiple(joins, split.cols = "V1", "=>")
joins$V1_2 <- NULL
colnames(joins) <- c("count", "LHS", "RHS")

aggregated_joins <- aggregate(LHS~RHS, joins, toString)
write.csv(aggregated_joins, file = "output/tag_associations2.csv")
