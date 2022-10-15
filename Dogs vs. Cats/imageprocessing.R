library(EBImage)
library(pbapply)
library(seriation)

image_process <- function(file.dir){
  #file.dir <- "raw/train"
  images <- list.files(file.dir)
  image_to_file <-  pblapply(images,function(img.name){
    #img.name <- "cat.1.jpg"
    img <- readImage(file.path(file.dir,img.name))
    red.weight <- 0.2989
    green.weight <- 0.587
    blue.weight <- 0.114
    img.gray <- red.weight * imageData(img)[,,1] +
      green.weight * imageData(img)[,,2] +
      blue.weight * imageData(img)[,,3]
    #gray_raw
    jpeg(paste("raw_gray/test/",img.name))
    display(img.gray,method = "raster")
    dev.off()
    #permute2:行列各切兩半
    new_rows.split <- c((nrow(img.gray)/2+1):nrow(img.gray),1:(nrow(img.gray)/2))
    new_cols.split <- c((ncol(img.gray)/2+1):ncol(img.gray),1:(ncol(img.gray)/2))
    img.split <- img.gray[new_rows.split,new_cols.split]
    jpeg(paste("permuted/split/test/",img.name))
    display(img.split,method = "raster")
    dev.off()
    #permute3:seriation #有變化再去看哪個好
    x.seriation  <- seriate(img.gray,'Random')
    new_rows <- x.seriation[[1]][1:nrow(img.gray)]
    new_cols <- x.seriation[[2]][1:ncol(img.gray)]
    img.seriation <- img.gray[new_rows,new_cols]
    jpeg(paste("permuted/random/test/",img.name))
    display(img.seriation,method = "raster")
    dev.off()
  }) 
}
image_process("raw/train")
image_process("raw/test")

