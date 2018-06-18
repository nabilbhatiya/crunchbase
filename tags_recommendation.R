# ------------------------------------------------
# Load required libraries and files
# ------------------------------------------------

rm(list = ls())
library(OpenMx)
## Read compatibility matrix
tag_matrix <- read.csv("output/tag_matrix.csv")

## Clean up, make the matrix symmetric

rownames(tag_matrix) <- tag_matrix$X
tag_matrix$X <- NULL
colnames(tag_matrix) <- rownames(tag_matrix)
tag_matrix <- tag_matrix + t(tag_matrix)

# ------------------------------------------
# Add some calculated rows to the matrix
# ------------------------------------------

## Change diagonal to 0 to calculate the max of each product except for itself, store the diagonal 
## in a separate vector as product frequencies, append it to original matrix

tag_freq <- diag2vec(tag_matrix)[,1] / 2
diag(tag_matrix) <- 0

overall_compatibility <- colSums(tag_matrix)

# rbind the additional calculated rows
tag_matrix <- rbind(tag_matrix, "overall_compatibility" = overall_compatibility)
tag_matrix <- rbind(tag_matrix, "product_freq" = tag_freq)
rm(overall_compatibility, tag_freq)


# ------------------------------------------
# Calculate output of reccommendation engine
# ------------------------------------------

## Create a blank matrix to store output
recommendation_matrix <- matrix("NA", ncol(tag_matrix), 3)
recommendation_matrix <- data.frame(recommendation_matrix, stringsAsFactors = F)
colnames(recommendation_matrix) <- c("Reccomendation_1", "Reccomendation_2", "Reccomendation_3")
rownames(recommendation_matrix) <- rownames(tag_matrix)[1:(nrow(tag_matrix)-2)]

# Keep record of top 20 global maxima in terms of overall buying frequency

top10global <- sort(tag_matrix[nrow(tag_matrix), ], decreasing = T)[1:20]

## Fill in the output matrix

for (i in 1:nrow(recommendation_matrix)){
  top3local <- sort(tag_matrix[i, ], decreasing = T)[1:3]
  if (top3local[3] > 0){
    recommendation_matrix[i, ] <- names(top3local)
  } else if (top3local[3] == 0 & top3local[2] > 0){
    recommendation_matrix[i, 1:2] <- names(top3local)[1:2]
    tempvec <- setdiff(names(top10global), top3local[1:2])
    tempvec <- setdiff(tempvec, rownames(tag_matrix)[i])
    recommendation_matrix[i, 3] <- tempvec[1]
  } else if (top3local[2] == 0 & top3local[1] > 0) {
    recommendation_matrix[i, 1] <- names(top3local)[1]
    tempvec <- setdiff(names(top10global), top3local[1])
    tempvec <- setdiff(tempvec, rownames(tag_matrix)[i])
    recommendation_matrix[i, 2:3] <- tempvec[1:2]
  } else if (top3local[1] == 0){
    recommendation_matrix[i, ] <- setdiff(names(top10global), rownames(tag_matrix)[i])[1:3]
  }
  recommendation_matrix[i, ] <- gsub(pattern = "has_bought.", replacement = "", x = recommendation_matrix[i, ])
}

rownames(recommendation_matrix) <- gsub("has_bought.", "", x = rownames(recommendation_matrix))


write.csv(recommendation_matrix, file = "output/recommendation_matrix.csv")

