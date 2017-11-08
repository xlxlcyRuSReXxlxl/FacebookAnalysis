#loading packages

require(EBImage)
require(magick)
require(pixmap)
require(imager)
require(imagedata)

########################################################

#' @name orientation_index
#' @details get infos about comments of the photos of a profile.
#' @export 
#' @author Ciro Lista
#' @param img_coord 
#' @return gives back the orientation of a given photo

img_orientation<-function(img_coor){
  orientation<-"Square"
  if(img_coor[1]>img_coor[2]){orientation<-"Landscape"}
  if(img_coor[1]<img_coor[2]){orientation<-"Portrait"}
  return(orientation)
}

#########################################################

#' @name dv_tot_grayScale
#' @details get infos about comments of the photos of a profile.
#' @export 
#' @author Livio Finos, Ciro Lista
#' @param user_path path of the user 
#' @return \code{data.frame} with n rows, one for each photo with the following values: 
#'  #'   "userID"            "album"             "photo_id"        
#'    "n_commenti_totali" "n_commenti_user"   "comm_dates"
#'    the column "comm_dates" contains the pasted texts of the got comments from a given photo





# it provides variability index using standard deviation, togheter with the image dimension and orientation
dv_tot_grayScale<-function(path){
  library(magick)
  library(pixmap)
  f <- tempfile(fileext = ".pgm")
  img<-image_read(path)
  img_coor<-as.numeric(image_info(img)[2:3])
  dim_img<-img_coor[1]*img_coor[2]
  orient_img<-img_orientation(img_coor)
  if(((image_info(img)[2]>448)&((image_info(img)[3])>448))){
    img<-image_scale(img,"256x")}
  x<-image_convert(img,format = "pgm",depth=8)
  image_write(x, path = f, format = "pgm")
  image <- read.pnm(file = f, cellres = 1)
  x <- image@grey * 255
  st_dv<-sd(x)
  out<-list(st_dv=st_dv,dim_img=dim_img,orient_img=orient_img)
}


# it provides variability index using Moore distance 
moore_grayScale<-function(path){
  library(magick)
  library(pixmap)
  f <- tempfile(fileext = ".pgm")
  img<-image_read(path)
  if(((image_info(img)[2]>448)&((image_info(img)[3])>448))){
    img<-image_scale(img,"256x")}
  x<-image_convert(img,format = "pgm",depth=8)
  image_write(x, path = f, format = "pgm")
  image <- read.pnm(file = f, cellres = 1)
  x <- image@grey * 255
  dist.moore<-matrix(data=NA,nrow=NROW(x),ncol=NCOL(x))
  nrighe=dim(dist.moore)[1]
  ncolonne=dim(dist.moore)[2]   
  for(i in 2:(nrighe-1)){         
    for(j in 2:(ncolonne-1)){
      dist.moore[i,j]<-abs(x[i,j]-x[i,j-1])+abs(x[i,j]-x[i,j+1])+abs(x[i,j]-x[i-1,j])+abs(x[i,j]-x[i+1,j])+abs(x[i,j]-x[i+1,j+1])+abs(x[i,j]-x[i-1,j-1])+abs(x[i,j]-x[i-1,j+1])+abs(x[i,j]-x[i+1,j-1])
    }
  }
  
  i=1   
  j=1
  dist.moore[i,j]=abs(x[i,j]*3-sum(x[i+1,j+1],x[i+1,j],x[i,j+1]))
  i=1
  j=NCOL(x)
  dist.moore[i,j]=abs(x[i,j]*3-sum(x[i,j-1],x[i+1,j],x[i-1,j-1]))
  i=NROW(x)
  j=NCOL(x)
  dist.moore[i,j]=abs(x[i,j]*3-sum(x[i,j-1],x[i-1,j-1],x[i-1,j]))
  i=NROW(x)
  j=1
  dist.moore[i,j]=abs(x[i,j]*3-sum(x[i,j+1],x[i-1,j],x[i-1,j-1]))
  
  i=1 
  for(j in 2:(NCOL(x)-1)){ 
    dist.moore[i,j]=abs(x[i,j]*5-sum(x[i+1,j],x[i+1,j+1],x[i+1,j-1],x[i,j+1],x[i,j-1]))
  }
  i=NROW(x)
  for(j in 2:(NCOL(x)-1)){
    dist.moore[i,j]=abs(x[i,j]*5-sum(x[i-1,j-1],x[i-1,j],x[i-1,j+1],x[i,j+1],x[i,j-1]))
  }
  
  j=1
  for(i in 2:(NROW(x)-1)){
    dist.moore[i,j]=abs(x[i,j]*5-sum(x[i,j+1],x[i+1,j+1],x[i-1,j+1],x[i-1,j],x[i+1,j]))
  }
  
  j=NCOL(x)
  for(i in 2:(NROW(x)-1)){
    dist.moore[i,j]=abs(x[i,j]*5-sum(x[i,j-1],x[i-1,j-1],x[i+1,j-1],x[i+1,j],x[i-1,j]))
  }
  
  st_dev<-sd(c(dist.moore))
  return(st_dev)
}

# this function provides the proportion of the main colors in the image
baige.rgb<-c(255, 250, 200)
black.rgb<-c(0, 0, 0)	
blue.rgb<-c(0, 130, 200)
brown.rgb<-c(170, 110, 40)
coral.rgb<-c(255, 215, 180)	
cyan.rgb<-c(70, 240, 240)
green.rgb<-c(60, 180, 75)
grey.rgb<-c(128, 128, 128)	
lavander.rgb<-c(230, 190,255)
lime.rgb<-c(210, 245, 60)
magenta.rgb<-c(240, 50, 230)
maroon.rgb<-c(128, 0, 0)
mint.rgb<-c(170, 255, 195)
navy.rgb<-c(0, 0, 128)
olive.rgb<-c(128, 128, 0)
orange.rgb<-c(245, 130, 48)
purple.rgb<-c(145, 30, 180)	
pink.rgb<-c(250, 190, 190)
red.rgb<-c(230, 25, 75)
silver.rgb<-c(192, 192, 192)
teal.rgb<-c(0, 128, 128)	
white.rgb<-c(255, 255, 255)
yellow.rgb<-c(255, 225, 25)

rgb.dataframe<-data.frame(rbind(baige.rgb,black.rgb,blue.rgb,brown.rgb,cyan.rgb,coral.rgb,green.rgb,grey.rgb,lavander.rgb,
                                lime.rgb,magenta.rgb,maroon.rgb,mint.rgb,navy.rgb,olive.rgb,orange.rgb,purple.rgb,pink.rgb,red.rgb,silver.rgb,teal.rgb,white.rgb,yellow.rgb))
colnames(rgb.dataframe)[1]<-"R"
colnames(rgb.dataframe)[2]<-"G"
colnames(rgb.dataframe)[3]<-"B"


color.detection<-function(pixel){
  col.class<-row.names(rgb.dataframe)[which.min((apply(rgb.dataframe,1,function(x) sum(abs(x-pixel)))))]
  return(col.class)
}

color.detection.img<-function(path){
  library(jpeg)
  img<-readImage(path)
  img<-EBImage::resize(img, w = 128, h = 128)
  if((is.na((dim(img)[3]))))
  {
  results<-as.vector(apply(img,c(1,2),function(x) color.detection(x)))
  }
  else{
  img<-round(255*imageData(img),digits=0)
  results<-as.vector(apply(img,c(1,2),function(x) color.detection(x)))
  }   
  c<-data.frame(prop.table(table(factor(results,levels = row.names(rgb.dataframe)))))
  return(c)
}


# Lightness,Saturation and Contrast indexes. These ones are computed both at pixel level and at image level
library(grDevices)
library(imager)
library(jpeg)

lightness.pix.level<-function(x){
  round((0.5*max(x[1]/255,x[2]/255,x[3]/255)+0.5*min(x[1]/255,x[2]/255,x[3]/255)),3)
}

saturation.pix.level<-function(x){
  Cmax<-max(x[1]/255,x[2]/255,x[3]/255)
  Cmin<-min(x[1]/255,x[2]/255,x[3]/255)
  delta<-Cmax-Cmin
  if(delta!=0) {
    delta/(1-abs(2*lightness.pix.level(x)-1))
  }
  else{0}
}

intensity.pix.level<-function(x){
  round((0.3*(x[1]/255)+0.3*(x[2]/255)+0.3*(x[3]/255)),3)
}


lightness.img.level<-function(path){
  img<-readImage(path)
  img<-EBImage::resize(img, w = 256, h = 256)
  img<-round(255*imageData(img),digits=0)
  results<-mean(c(apply(img,c(1,2),function(x) lightness.pix.level(x)))) 
  return(results)
}



saturation.img.level<-function(path){
  img<-readImage(path)
  img<-EBImage::resize(img, w = 256, h = 256)
  img<-round(255*imageData(img),digits=0)
  results<-mean(as.vector(apply(img,c(1,2),function(x) saturation.pix.level(x))))
  return(results)
}

contrast.img.level<-function(path){
  img<-readImage(path)
  img<-EBImage::resize(img, w = 256, h = 256)
  img<-round(255*imageData(img),digits=0)
  results<-sd(as.vector(apply(img,c(1,2),function(x) intensity.pix.level(x))))
  return(results)
}

glob.img.level<-function(path){
  results<-rep(NA,3)
  img<-readImage(path)
  if((is.na((dim(img)[3]))))
  {
  results[1]<-"NA"
  results[2]<-"NA"
  results[3]<-"NA"
  }
  else{
  img<-EBImage::resize(img, w = 256, h = 256)
  img<-round(255*imageData(img),digits=0)
  results[1]<-mean(c(apply(img,c(1,2),function(x) lightness.pix.level(x)))) 
  results[2]<-mean(as.vector(apply(img,c(1,2),function(x) saturation.pix.level(x))))
  results[3]<-sd(as.vector(apply(img,c(1,2),function(x) intensity.pix.level(x))))
  }
  return(results)}



