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
x <- as.data.frame(table(category_list$company_name, category_list$company_category_list))
colnames(x) <- c("company", "tag", "applicable")











# If the tag was used, 1 if not, 0

x$is_applicable <- as.numeric(x$applicable > 0)
x$applicable <- NULL


# Puts tags as row names and makes it a company by tag truth table

bool_matrix <- reshape(x, idvar = "company", timevar = "tag", direction = "wide")
write.csv(bool_matrix, file = "output/bool_matrix.csv")
save(bool_matrix, file = "cleaned/bool_matrix.bin")
#rm(x)

# Remove columns that aren't required

x$company <- NULL


# ------------------------------------------------------
# Create a matrix to store inter-tag compatibility
# ------------------------------------------------------
load("cleaned/bool_matrix.bin")
tag_matrix <- matrix(0, nrow = ncol(bool_matrix), ncol = ncol(bool_matrix))

# add row and col names for the tag-tag matrix
names <- colnames(bool_matrix)
colnames(tag_matrix) <- names
rownames(tag_matrix) <- names

# for loop to add to the empty matrix the sum of each column for each tag
bool_matrix <- matrix(bool_matrix)
for(i in 1:nrow(tag_matrix)){
  for(j in 1:i){
    tag_matrix[i,j] = sum(bool_matrix[[i]] * bool_matrix[[j]])
  }
}





tag_matrix <- as.data.frame.matrix(tag_matrix)
tag_matrix$company <- NULL
tag_matrix <- tag_matrix[-1, ]
rownames(tag_matrix) <- gsub("is_applicable.", "", x = rownames(tag_matrix))
colnames(tag_matrix) <- rownames(tag_matrix)

rm(bool_matrix)
# -------------------------
# Write output to csv file
# -------------------------

write.csv(tag_matrix, file = "output/tag_matrix.csv")

tag_matrix_with_transpose <- tag_matrix + t(tag_matrix)
diag(tag_matrix_with_transpose) <- NA
write.csv(tag_matrix_with_transpose, file = "output/tag_matrix_with_transpose.csv")
