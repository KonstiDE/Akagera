setwd("data/reverse_classification/")

library(terra)

raw20 <- rast("image_export_raw2020.tif")
class20 <- rast("image_export_classified2020.tif")

raw21 <- rast("image_export_raw2021.tif")

class_mean_vectors <- c()
for(c in 1:9){

  bands_mean <- c()
  for(b in 1:6){
    band_vector <- c()

    for(v in seq_along(values(raw20[[b]]))){
      print(v)
      if((!is.nan(values(raw20[[b]])[v]) && values(class20)[v] == c)){
        append(band_vector, v)
      }
    }

    append(bands_mean, mean(band_vector))

  }
  append(class_mean_vectors, bands_mean)

}

