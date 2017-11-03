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

# Convert to data table
category_list <- data.table(category_list)

# Remove duplicated entries
category_list <- category_list[!duplicated(category_list$company_name), ]

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


#-------------------
# check tagwise frequency
#-------------------
tag_frequency <- as.data.frame(table(category_list$company_category_list))
tag_frequency <- tag_frequency[order(tag_frequency$Freq, decreasing = T), ]
# tag frequency percentage
tag_frequency$perc_occurance <- tag_frequency$Freq / sum(tag_frequency$Freq) * 100
colnames(tag_frequency) <- c("category_tag", "freq", "perc_occurance")


# write csv for tag frequency
write.csv(tag_frequency, file = "output/tag_frequency.csv")

#-------------------
# merge tag frequency with category list and re-order to get company wise categories in order
#-------------------
category_list <- merge(category_list, tag_frequency, by.x = "company_category_list", by.y = "category_tag", all.x = T)
category_list <- category_list[,c(2, 1, 3, 4)]
category_list <- category_list[order(category_list$company_name, category_list$perc_occurance, decreasing = T), ]
# re-ordering
category_list[, id := rowid(company_name)]

#-------------------
# reshape to get company wise category table
#-------------------
category_table <- category_list[,c(1, 2, 5)]
rm(category_list)
category_table <- reshape(data = category_table, timevar = "id", idvar = "company_name", direction = "wide")
colnames(category_table) <- c("company name", seq(1:26))
category_table <- category_table[order(category_table$`company name`, decreasing = F), ]

# write csv for category_table
write.csv(category_table, file = "output/category_table.csv")
