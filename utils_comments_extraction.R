#' @name get_photo_comments
#' @details get infos about comments of the photos of a profile.
#' @export 
#' @author Livio Finos, Ciro Lista
#' @param user_path path of the user 
#' @return \code{data.frame} with n rows, one for each photo with the following values: 
#'  #'   "userID"            "album"             "photo_id"        
#'    "n_commenti_totali" "n_commenti_user"   "comm_dates"
#'    the column "comm_dates" contains the pasted texts of the got comments from a given photo

 

get_photo_comments<-function(user_path){ 
  
  user_path=myFBr:::.fixPercorso(user_path)
  user_path_photos=paste(user_path,sep="/","photos")
  
  # list of albums to be scraped
  list_album<-list.dirs(user_path_photos,recursive = FALSE)

  # get commments from each album
  res=llply(list_album,.get_comments_album)
  res=bind_rows(res)
  res
}


# get comments from a given album 
.get_comments_album <- function(album){
  html_path<-paste(album,'index.htm',sep="/")
  #if missing index.htm return NULL
  if(length(dir(album,pattern = "index.htm"))==0) {
    return(NULL)
  }
  
  pg = htmlTreeParse(html_path,useInternalNodes=TRUE)
  
  user_name=user_name=toString.XMLNode(getNodeSet(pg,"//h1/text()")[[1]])
  blocks=getNodeSet(pg,"//div[@class='block']")
  
  users= plyr::llply(blocks, function(el) {getNodeSet(el, "div/div[@class='comment']/span[@class='user']/text()")})
  time= plyr::llply(blocks, function(el) {getNodeSet(el, "div/div[@class='comment']/div[@class='meta']/text()")})
  comments= plyr::llply(blocks, function(el) {getNodeSet(el, "div/div[@class='comment']/text()")})
  
  
  n_commenti_totali=llply(users,length)
  n_commenti_totali=unlist(n_commenti_totali)
  n_commenti_user=llply(users,function(usr){
    sum(unlist(myFBr:::.estraielementi(usr))==user_name) })
  n_commenti_user=unlist(n_commenti_user)
  
  
  .make_time_comments=function(i) {
    users_=unlist(llply(users[i],myFBr:::.estraielementi))
    users_=paste(c("","|=|")[(users_==user_name)+1],
              users_,sep=" ")
    
    comments_=unlist(llply(comments[i],myFBr:::.estraielementi))
    
    time_=unlist(llply(time[i],myFBr:::.estraielementi))
    time_=myFBr:::inDataIT(time_)
    temp=data.frame(users=users_, comment=comments_,time=time_)
    temp$users=as.character(temp$users)
    temp$comment=as.character(temp$comment)
    temp$time=as.character(temp$time)
    out=paste(temp$users,temp$comment,temp$time,sep="&----&",collapse = "&++++&")
  }
  
  commnts=llply(which(n_commenti_totali>0),.make_time_comments)
  commnts=unlist(commnts)
  
  photo_id=llply(1:length(blocks),function(i)as.vector(xmlToList(xmlChildren(blocks[[i]])$img)))
  photo_id=unlist(photo_id)
  
  
  commenti=rep("",length(n_commenti_totali))
  commenti[n_commenti_totali>0] = commnts
    
  data.frame(stringsAsFactors = FALSE,
             album=album,photo_id=photo_id,
             n_commenti_totali=n_commenti_totali,
             n_commenti_user=n_commenti_user,
             comm_dates=commenti)
}

