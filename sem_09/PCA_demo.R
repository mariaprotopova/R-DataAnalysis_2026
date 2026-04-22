library(tidyverse)
library(Metrics)

data <- tibble(
  x = seq(1, 20),
  y= 2*x + rnorm(20, mean=1, sd=3)
  )

ggplot(data, aes(x, y)) +
  geom_point() +
  geom_smooth(method='lm')

cov(data$x, data$y)

data$x_centered <- scale(data$x, scale=F)
data$y_centered <- scale(data$y, scale=F)
A <- cov(scale(data$x_centered), scale(data$y_centered))
e <- eigen(A)

data_reconstructed <- t(as.matrix(e$values)) %*% t(data$x_centered)
data$x_reconstructed <- t(data_reconstructed) + mean(data$x)
data[, c(1, 5)]
View(data)
mean((data$x_reconstructed - data$x)**2)


# ==============================================================================
library(imgpalr)
library(jpeg)
library(factoextra)
library(gridExtra)
library(ggplot2)
library(magick)

photo<-readJPEG("button.jpg")
plot(1, type="n") # plotting the rasterImage - colour photo
rasterImage(photo, 0.6, 0.6, 1.4, 1.4)

photo.sum<-photo[,,1]+photo[,,2]+photo[,,3] # summing up RGB shades
photo.bw<-photo.sum/max(photo.sum)          # dividing by max
plot(1, type="n")                             # plotting the rasterImage - black & white photo
rasterImage(photo.bw, 0.6, 0.6, 1.4, 1.4)
writeJPEG(photo.bw, "photo_bw.jpg")

r.pca<-prcomp(r, center=FALSE, scale.=FALSE)
# PCA for R color component
g.pca<-prcomp(g, center=FALSE, scale.=FALSE)
# PCA for G color component
b.pca<-prcomp(b, center=FALSE, scale.=Tr)
# PCA for B color component

bw.pca <- prcomp(photo.bw, center=FALSE, scale.=FALSE)

rgb.pca<-list(r.pca, g.pca, b.pca)
# merging all PCA into one object

library(gridExtra)
f1<-fviz_eig(r.pca, main="Red", barfill="red", ncp=5, addlabels=TRUE)
f2<-fviz_eig(g.pca, main="Green", barfill="green", ncp=5, addlabels=TRUE)
f3<-fviz_eig(b.pca, main="Blue", barfill="blue", ncp=5, addlabels=TRUE)
f1 <- fviz_eig(bw.pca, main="Black", barfill="blue", ncp=5, addlabels=TRUE)
grid.arrange(f1, ncol=1)

vec<-c(1, 3, 5, 10, 15, 50, 100, 500, 950)
for(i in vec){
  photo.pca<-sapply(list(bw.pca), function(j) {
    new.RGB<-j$x[,1:i] %*% t(j$rotation[,1:i])}, simplify="array")
  assign(paste("photo_", round(i,0), sep=""), photo.pca) # saving as object
  writeJPEG(photo.pca, paste("photo_", round(i,0), "_princ_comp.jpg", sep=""))
}

par(mfrow=c(3,3)) 
par(mar=c(1,1,1,1))
plot(image_read(get(paste("photo_", round(vec[1],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[2],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[3],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[4],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[5],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[6],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[7],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[8],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[9],0), sep=""))))

sizes<-matrix(0, nrow=9, ncol=4)
colnames(sizes)<-c("Number of PC", "Photo size", "Compression ratio", "MSE-Mean Squared Error")
sizes[,1]<-round(vec,0)
for(i in 1:9) {
  path<-paste("photo_", round(vec[i],0), "_princ_comp.jpg", sep="")
  sizes[i,2]<-file.info(path)$size 
  photo_mse<-readJPEG(path)
  sizes[i,4]<-mse(photo.bw, photo_mse) # from Metrics::
}
sizes[,3]<-round(as.numeric(sizes[,2])/as.numeric(sizes[9,2]),3)
sizes
