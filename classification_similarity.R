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

euclidean_dist <- distance(raw21, class_mean_vectors[1, ])

classification_vector <- c()
for(t in seq_along(length(values(raw21)))){
  shortest_distance <- 999999999

  for(i in 1:9){
    b_vec <- class_mean_vectors[i,]
    euclidean_distance <- dist(rbind(b_vec, extract(raw21, t)[-1]))

    if(!is.na(euclidean_distance)){
      if(euclidean_distance < shortest_distance){
        shortest_distance <- euclidean_distance
        best_class <- i
      }
    }else{
      best_class <- NA
    }

  }
  print(best_class)
  classification_vector <- append(classification_vector, best_class)
}

