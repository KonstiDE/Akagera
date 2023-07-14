setwd("data")

library(terra)
library(ggplot2)
library(tidyterra)
library(stats)
library(caret)
library(doParallel)
library(tictoc)
library(ranger)
library(raster)
library(e1071)

shape_park_old <- vect("park/Akagera_boundary_old_GADM_based.shp")
shape_park_new <- vect("park/anp_park_boundary.shp")

raw20 <- rast("classification/image_export_raw2020.tif")
raw20_r <- raster("classification/image_export_raw2020.tif")

class20 <- rast("classification/image_export_classified2020.tif")

raw86 <- rast("ones/one_1986.tif")
raw93 <- rast("ones/one_1993.tif")
raw97 <- rast("ones/one_1997.tif")
raw00 <- rast("ones/one_2000.tif")
raw10 <- rast("ones/one_2010.tif")
raw14 <- rast("ones/one_2014.tif")
raw18 <- rast("ones/one_2018.tif")
raw22 <- rast("ones/one_2022.tif")

elevation <- raster("elevation/output_COP30.tif")
elevation_np <- raster::crop(elevation, extent(raw20_r))
elevation_np <- alignExtent(extent(elevation_np), raw20_r, snap = "near")

extent(raw20_r) == extent(elevation_np)

# Detemninistic approach
class_mean_vectors <- c()
for(c in 1:7){
  bands_mean <- c()
  for(b in 1:5){
    bands_mean <- append(bands_mean, mean(values(raw20[[b]])[values(class20) == c], na.rm = T))
  }
  class_mean_vectors <- rbind(class_mean_vectors, bands_mean)
}
colnames(class_mean_vectors) <- c("blue", "green", "red", "nir", "swir1", "swir2")

euclidian_func <- function (v){
  shortest_distance <- 99999999
  for(t in 1:7){
    euclidean_dist <- dist(rbind(v, class_mean_vectors[t, ]), method = "manhattan")

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

results <- terra::app(raw21, euclidian_func)

ggplot() +
  stat_spatraster(data = results) +
  scale_fill_stepsn(colours = viridis(7))


# Machine Learning (RF) Approach
vectors <- terra::extract(raw20, seq_along(values(raw20)))
colnames(vectors) <- c("blue", "green", "red", "nir", "swir1", "swir2", "elevation")

elevation_np_vals <- terra::extract(elevation_np, seq_along(values(elevation_np)))

classes <- terra::extract(class20, seq_along(values(class20)))
colnames(classes) <- c("class")

df <- cbind(vectors, classes)
df <- df[df$class > 0 & !is.na(df),]


sampled_df <- data.frame()
for (class_value in 1:7) {
  class_rows <- subset(df, class == class_value)
  
  sampled_rows <- class_rows[sample(nrow(class_rows), 9000), ]
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
values <- values[complete.cases(values),]
colnames(values) <- c("blue", "green", "red", "nir", "swir1", "swir2")

predicts <- predict(model, values)
predicted_values <- predicts$predictions
unique(predicted_values)

class21 <- raw21[[1]]
class21[!is.na(class21)] <- predicted_values
plot(class21)

writeRaster(class21, "2021.tif")


# Predict before genozide
values <- terra::extract(raw86_one, seq_along(values(raw86_one)))
values <- values[complete.cases(values),]
colnames(values) <- c("blue", "green", "red", "nir", "swir1", "swir2")

predicts <- predict(model, values)
predicted_values <- predicts$predictions
unique(predicted_values)

class86 <- raw86_one[[1]]
class86[!is.na(class86)] <- predicted_values
plot(class86)

writeRaster(class86, "1986_one.tif")

head(raw86)

plotRGB(raw86, r=3, g=2, b=1, stretch = "hist")

