rm(list = ls())

library(arules)
library(arulesViz)
library(Hmisc)

tags <- read.transactions('cleaned/category_tags.txt', sep = ",", rm.duplicates = T)
tags_vector <- read.csv('output/tag_frequency.csv')
tags_vector <- c(as.character(tags_vector$category_tag))

tags_frequent <- apriori(tags, parameter = list(minlen = 2, maxlen = 5, target = "frequent itemsets", support = 0.003))
inspect(tags_frequent)
tags_frequent_frame <- as(tags_frequent, "data.frame")
tags_frequent_frame <- tags_frequent_frame[order(tags_frequent_frame$support, decreasing = T), ]
write.csv(tags_frequent_frame, file = 'output/tag_frequency_apriori.csv')
plot(tags_frequent[1:50], method = "graph")

tag_rules <- apriori(tags, parameter = list(minlen = 2, target = "rules", support = 0.0002, confidence = 0.001))
inspect(tag_rules)
#inspect(subset(tag_rules, subset = rhs %in% tags_vector))
plot(tag_rules[1:200], method = "graph", control = list(verbose = T, max = 200))
tag_rules_frame <- as(tag_rules, "data.frame")
tag_rules_frame <- tag_rules_frame[order(tag_rules_frame$lift, decreasing = T), ]
write.csv(tag_rules_frame, file = 'output/tag_rules_apriori.csv')


single_tag <- as(subset(tag_rules, subset = lhs %in% tags_vector), "data.frame")
single_tag <- single_tag[order(single_tag$count, decreasing = T), ]
single_tag <- single_tag[!duplicated(single_tag$lift), ]



single_tag$x_rules <- NULL
single_tag <- single_tag[-1, ]


x <- as(tag_rules[!is.redundant(tag_rules, measure = 'confidence')], 'data.frame')




tag_rules <- apriori(tags, parameter = list(minlen = 2, target = "rules", support = 0.0002, confidence = 0.001),
                     appearance = list(lhs = tags_vector))
inspect(tag_rules)
tag_rules_frame <- tag_rules_frame[order(tag_rules_frame$lift, decreasing = T), ]