#' @name get.Season
#' @details 
#' @export 
#' @author Ciro Lista
#' @param  data  
#' @return the season on which a certain photos of a profile has been taken 



get.Season <- function(data) {
WINTER.START <- as.Date("2017-12-22", format = "%Y-%m-%d") 
SPRING.START <- as.Date("2017-3-21",  format = "%Y-%m-%d") 
SUMMER.START <- as.Date("2017-6-22",  format = "%Y-%m-%d") 
AUTUMN.START <- as.Date("2017-9-23",  format = "%Y-%m-%d") 
if(is.na(data)){
season<-"NA"
}
else{
date <- as.Date(strftime(data, format="2017-%m-%d"))
season<-'Autumn'
if ((date >= WINTER.START | date < SPRING.START)){
season<- "Winter"}
if ((date >= SPRING.START & date < SUMMER.START)){
season<-"Spring"}
if ((date >= SUMMER.START & date < AUTUMN.START)){
season<-"Summer"}
}
return(season)
}


