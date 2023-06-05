setwd("data/reverse_classification/")

library(terra)

raw20 <- rast("image_export_raw2020.tif")
class20 <- rast("image_export_classified2020.tif")

raw21 <- rast("image_export_raw2021.tif")
mean(raw21)


class_mean_vectors <- NULL
for(c in 1:9){
  bands_mean <- c()
  for(b in 1:6){
    bands_mean <- append(bands_mean, base::mean(values(raw20[[b]])[values(class20) == c], na.rm = T))
  }
  class_mean_vectors <- rbind(class_mean_vectors, bands_mean)
}
class_mean_vectors <- as.data.frame(class_mean_vectors)
colnames(class_mean_vectors) <- c("B1", "B2", "B3", "B4", "B5", "B6")

vec21 <- c(
  values(raw21[[1]])[6672],
  values(raw21[[2]])[6672],
  values(raw21[[3]])[6672],
  values(raw21[[4]])[6672],
  values(raw21[[5]])[6672],
  values(raw21[[6]])[6672]
)

# Calculate the eucldean distance from vec21 to all vectors in the dataframe to receive vec21s class

