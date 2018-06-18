rm(list=ls())

library(splitstackshape)
library(data.table)

recommendation_frequency_table <- read.csv('output/tag_comparisions.csv')

recommendation_frequency_table <- data.table(recommendation_frequency_table)

recommendation_frequency_table$X <- NULL
recommendation_frequency_table$initial_tags <- NULL

recommendation_frequency_table <- concat.split.multiple(recommendation_frequency_table, "recommended_tags", seps = ',')

colnames(recommendation_frequency_table) <- c('company_name', 'level1', 'level2', 'level3')

write.csv(recommendation_frequency_table, file = 'output/recommendation_frequency_table.csv')

