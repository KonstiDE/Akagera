setwd("C:/Users/Konstantin/PycharmProjects/Akagera/data/reverse_classification/")

library(terra)
library(ggplot2)
library(tidyterra)
library(stats)
library(caret)
library(doParallel)
library(tictoc)
library(ranger)

raw20 <- rast("image_export_raw2020.tif")
raw19 <- rast("image_export_raw2019.tif")
class20 <- rast("image_export_classified2020.tif")

raw21 <- rast("image_export_raw2021.tif")


class_mean_vectors <- c()
for(c in 1:9){
  bands_mean <- c()
  for(b in 6:1){
    bands_mean <- append(bands_mean, mean(values(raw20[[b]])[values(class20) == c], na.rm = T))
  }
  class_mean_vectors <- rbind(class_mean_vectors, bands_mean)
}
colnames(class_mean_vectors) <- c("SR_B2_mean", "SR_B3_mean", "SR_B4_mean", "SR_B5_mean", "SR_B6_mean", "SR_B7_mean")

euclidian_func <- function (v){
  shortest_distance <- 99999999
  vec <- rev(v[-7])
  for(t in 1:9){
    euclidean_dist <- dist(rbind(vec, class_mean_vectors[t, ]), method = "manhattan")

    if(!is.na(euclidean_dist)){
        if(euclidean_dist < shortest_distance){
          shortest_distance <- euclidean_dist
          best_class <- t
        }
      }else{
        return(NaN)
      }
  }
  return(best_class)
}

results <- terra::app(raw19, euclidian_func)

ggplot() +
  stat_spatraster(data = results) +
  scale_fill_stepsn(colours = c("black", "orange"))

plot(class20)
plot(results)



# Machine Learning Approach

vectors <- rev(terra::extract(raw20, 1:length(values(raw20)))[-7])
colnames(vectors) <- c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")

classes <- terra::extract(class20, 1:length(values(class20)))
colnames(classes) <- c("class")

df <- cbind(vectors, classes)
df <- df[df$class > 0 & !is.na(df[,1]),]


sampled_df <- data.frame()
for (class_value in 1:9) {
  class_rows <- subset(df, class == class_value)
  
  sampled_rows <- class_rows[sample(nrow(class_rows), 10000), ]
  sampled_df <- rbind(sampled_df, sampled_rows)
}

row.names(sampled_df) <- NULL
nrow(sampled_df)

# Use below here sampled df over df for balanced dataset
shuffled_df = df[sample(1:nrow(df)), ]
nrow(shuffled_df)


# Caret

set.seed(25111999)
trainIndex <- 0.8 * nrow(df)
trainData <- shuffled_df[1:trainIndex, ]
testData <- shuffled_df[(trainIndex + 1):nrow(shuffled_df), ]

tic()
model <- ranger(
  class ~ SR_B2 + SR_B3 + SR_B4 + SR_B5 + SR_B6 + SR_B7, 
  data = trainData,
  verbose = TRUE  
)
toc()


predicts <- predict(model, testData)

predicted_values <- round(predicts$predictions)
actual_values <- testData$class

cbind(predicted_values, actual_values)

# Accuracy on test set
sum((predicted_values == actual_values)) / nrow(testData) * 100


# Predict actual landcover
values <- values(raw21)
values <- values[!is.na(values[,1]),]
colnames(values) <- c("SR_B7", "SR_B6", "SR_B5", "SR_B4", "SR_B3", "SR_B2", "QA_PIXEL")

predicts <- predict(model, values)
predicted_values <- round(predicts$predictions)

class21 <- class20
setValues(class21, predicted_values)
plot(class21)
