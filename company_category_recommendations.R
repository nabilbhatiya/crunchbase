##----##----##----##
### Code to get category table for companies
##----##----##----##

#-------------------
# Clear workspace and load required libraries and files
#-------------------

rm(list = ls())

library(data.table)
library(tidyr)

category_list <- read.csv("rawdata/category_list.csv")
recommendations <- read.csv("output/recommendation_matrix.csv")
tag_frequency <- read.csv('output/tag_frequency.csv')

# Convert to data table
category_list <- data.table(category_list)

# Remove duplicated entries
category_list <- category_list[!duplicated(category_list$company_name), ]
category_list2 <- data.table(category_list)

#-------------------
# Use separate_rows to convert category list to rows
#-------------------
category_list <- separate_rows(category_list, company_category_list, sep = ",")

#-------------------
# Little clean-up
#-------------------
category_list$company_category_list <- gsub("\"","", category_list$company_category_list)
category_list$company_category_list <- gsub("[{]","", category_list$company_category_list)
category_list$company_category_list <- gsub("[}]","", category_list$company_category_list)

category_list$company_category_list[category_list$company_category_list == ""] <- NA

company_category_recommendations <- merge(category_list, recommendations, by.x = 'company_category_list', by.y = 'X', all.x = T)
rm(category_list, recommendations)
company_category_recommendations <- company_category_recommendations[,c(2,1,3,4,5)]
colnames(company_category_recommendations) <- c('company_name', 'current_category_tag', 'recommendation_1', 'recommendation_2', 'recommendation_3')
company_category_recommendations <- company_category_recommendations[order(company_category_recommendations$company_name, decreasing = F), ]
write.csv(company_category_recommendations, file = 'output/company_category_recommendations.csv')

company_category_recommendations <- company_category_recommendations[, c(1,3:5)]
company_category_recommendations$company_name <- as.character(company_category_recommendations$company_name)
company_category_recommendations$recommendation_1 <- as.character(company_category_recommendations$recommendation_1)
company_category_recommendations$recommendation_2 <- as.character(company_category_recommendations$recommendation_2)
company_category_recommendations$recommendation_3 <- as.character(company_category_recommendations$recommendation_3)

company_category_recommendations <- data.table(company_category_recommendations)
recommendation_by_frequency <- company_category_recommendations[,c(recommendation_1, recommendation_2, recommendation_3), by = "company_name"]
rm(company_category_recommendations)
recommendation_by_frequency <- recommendation_by_frequency[!duplicated(recommendation_by_frequency), ]

tag_frequency <- tag_frequency[,c(2:3)]

recommendation_by_frequency <- merge(recommendation_by_frequency, tag_frequency, by.x = 'V1', by.y = 'category_tag', all.x = T)
rm(tag_frequency)

recommendation_by_frequency <- recommendation_by_frequency[order(recommendation_by_frequency$company_name, recommendation_by_frequency$freq, decreasing = T), ]
recommendation_by_frequency <- recommendation_by_frequency[, c(2,1,3)]
colnames(recommendation_by_frequency) <- c('company_name','category','freq')

#Generating groupwise seq
recommendation_by_frequency[, id := seq_len(.N), by = "company_name"]
recommendation_by_frequency[, id := rowid(company_name)]

recommendation_by_frequency <- recommendation_by_frequency[recommendation_by_frequency$id <= 3, ]
recommendation_by_frequency$freq <- NULL
recommendation_by_frequency$id <- NULL

recommendation_by_frequency$category <- as.character(recommendation_by_frequency$category)
recommendation_by_frequency <- aggregate(category~company_name, recommendation_by_frequency, toString)

write.csv(recommendation_by_frequency, file = 'output/recommendation_by_frequency.csv')

category_list2$company_category_list <- gsub("\"","", category_list2$company_category_list)
category_list2$company_category_list <- gsub("[{]","", category_list2$company_category_list)
category_list2$company_category_list <- gsub("[}]","", category_list2$company_category_list)

category_list2$company_category_list[category_list2$company_category_list == ""] <- NA

recommendation_by_frequency <- merge(category_list2, recommendation_by_frequency, by = 'company_name', all.x = T)
rm(category_list2)
colnames(recommendation_by_frequency) <- c('company_name', 'initial_tags', 'recommended_tags')

write.csv(recommendation_by_frequency, file = 'output/tag_comparisions.csv')
