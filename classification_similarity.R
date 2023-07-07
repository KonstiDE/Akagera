setwd("C:/Users/Konstantin/PycharmProjects/Akagera/data/reverse_classification_summed_normalized")

library(terra)
library(ggplot2)
library(tidyterra)
library(stats)
library(caret)
library(doParallel)
library(tictoc)
library(ranger)
library(e1071)

raw14 <- rast("image_export_raw2014.tif")
raw15 <- rast("image_export_raw2015.tif")
raw16 <- rast("image_export_raw2016.tif")
raw17 <- rast("image_export_raw2017.tif")
raw18 <- rast("image_export_raw2018.tif")
raw19 <- rast("image_export_raw2019.tif")
raw20 <- rast("image_export_raw2020.tif")
raw21 <- rast("image_export_raw2021.tif")

air14 <- rast("image_export_airfield2014.tif")
air15 <- rast("image_export_airfield2015.tif")
air16 <- rast("image_export_airfield2016.tif")
air17 <- rast("image_export_airfield2017.tif")
air18 <- rast("image_export_airfield2018.tif")
air19 <- rast("image_export_airfield2019.tif")
air20 <- rast("image_export_airfield2020.tif")
air21 <- rast("image_export_airfield2021.tif")

class20 <- rast("image_export_classified2020_new.tif")


class_mean_vectors <- c()
for(c in 1:7){
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
  for(t in 1:7){
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


# Machine Learning Approaches
raw21 <- rast("image_export_raw2021.tif")
raw20 <- rast("image_export_raw2020.tif")
diff20_21_airfield <- colMeans(values(air20) - values(air21))
values(raw20) <- values(raw20) - diff20_21_airfield

vectors <- terra::extract(raw20, seq_along(values(raw20)))
colnames(vectors) <- c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")

classes <- terra::extract(class20, seq_along(values(class20)))
colnames(classes) <- c("class")

df <- cbind(vectors, classes)
df <- df[df$class > 0 & !is.na(df[,1]),]


sampled_df <- data.frame()
for (class_value in 1:7) {
  class_rows <- subset(df, class == class_value)
  
  sampled_rows <- class_rows[sample(nrow(class_rows), 12000), ]
  sampled_df <- rbind(sampled_df, sampled_rows)
}
row.names(sampled_df) <- NULL
nrow(sampled_df)
sampled_df$class <- as.factor(sampled_df$class)


# Use below here sampled df over df for balanced dataset
shuffled_df <- sampled_df[sample(seq_len(nrow(sampled_df))), ]
nrow(shuffled_df)


set.seed(25111999)
trainIndex <- 0.8 * nrow(shuffled_df)
trainData <- shuffled_df[1:trainIndex, ]
testData <- shuffled_df[(trainIndex + 1):nrow(shuffled_df), ]




# Ranger
tic()
model <- ranger(
  class ~ .,
  data = trainData,
  verbose = TRUE,
  num.trees = 500
)
toc()

predicts <- predict(model, testData)

predicted_values <- predicts$predictions
actual_values <- testData$class

cbind(predicted_values, actual_values)

# Accuracy on test set
sum((predicted_values == actual_values)) / nrow(testData) * 100


# Predict actual landcover
values <- terra::extract(raw21, seq_along(values(raw21)))
values <- values[!is.na(values[,1]),]
colnames(values) <- c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")

predicts <- predict(model, values)
predicted_values <- predicts$predictions

class21 <- class20
class21[!is.na(class21) & (class21 != 0)] <- predicted_values

par(mfrow=c(1, 1))
plot(class21)
plot(class20)



# Radial SVM
tic()
radial <- svm(class ~ ., data=shuffled_df, kernel="radial", cost=5, scale=F, verbose=T)
toc()

radial

tic()
predicts <- predict(radial, newdata = testData)
toc()

predicts <- as.numeric(predicts)
actual <- as.numeric(testData$class)

cbind(predicts, actual)

sum((predicts == actual)) / length(actual) * 100

