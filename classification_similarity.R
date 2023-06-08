setwd("data/reverse_classification/")

library(terra)

raw20 <- rast("image_export_raw2020.tif")
class20 <- rast("image_export_classified2020.tif")

raw21 <- rast("image_export_raw2021.tif")


class_mean_vectors <- NULL
for(c in 1:9){
  bands_mean <- c()
  for(b in 1:6){
    bands_mean <- append(bands_mean, base::mean(values(raw20[[b]])[values(class20) == c], na.rm = T))
  }
  class_mean_vectors <- rbind(class_mean_vectors, bands_mean)
}

euclidian_func <- function (v){
  shortest_distance <- 99999999
  for(t in 1:9){
    euclidean_dist <- dist(rbind(rev(v[-7]), class_mean_vectors[t, ]), method = "euclidean")

    if(!is.na(euclidean_dist)){
        if(euclidean_dist < shortest_distance){
          shortest_distance <- euclidean_dist
          best_class <- t
        }
      }else{
        best_class <- NaN
      }
  }
  return(best_class)
}

results <- terra::app(raw20, euclidian_func)

plot(class20)
plot(results)

