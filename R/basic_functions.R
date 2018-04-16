#' creating a workspace
#'
#' This function builds a simply project scope for image processing work.
#' @keywords create
#' @export
#' @examples
#' create_proj()

create_proj = function(){
  dir.create(paste0(getwd(),"/","pos"))
  dir.create(paste0(getwd(),"/","negs"))
  dir.create(paste0(getwd(),"/","imagebank"))
  dir.create(paste0(getwd(),"/","explore"))
  img_dirs <<-
    list(
      pos = paste0(getwd(),"/","pos")
      ,negs = paste0(getwd(),"/","negs")
      ,imagebank = paste0(getwd(),"/","imagebank")
      ,explore = paste0(getwd(),"/","explore")
    )
  homez <<- getwd()
}


#' produce image directory 
#'
#' This function calls the image directories.
#' @keywords create
#' @export
#' @examples
#' create_proj()
create_direct=function(){
  img_dirs <<-
    list(
      pos = paste0(getwd(),"/","pos")
      ,negs = paste0(getwd(),"/","negs")
      ,imagebank = paste0(getwd(),"/","imagebank")
      ,explore = paste0(getwd(),"/","explore")
    )
}


#' download video from link
#'
#' This function pulls a video from the web to work with; you must run create_proj first if you omit sbj.
#' @param what the link to the video.
#' @param home the directory it will go to.
#' @param sbj a key work for the video to be named.
#' @keywords download
#' @export
#' @examples
#' download_vid("https://drive.switch.ch/index.php/s/3b3bdbd6f8fb61e05d8b0560667ea992/download?path=%2Fvideos%2Fdrones&files=Video_1.avi",home,"moto)
download_vid =  function(what,home,sbj) {
  
  sbj =
    if(missing(sbj)) {
      "src"
    } else {
      sbj
    }
  
  print(sbj)
  
  vid_typ <<- substr(what,nchar(what)-3,nchar(what))
  
  #the name of the file
  home =
    if(missing(home)) {
      paste0(getwd(),"/",paste0("raw_vid_",sbj,vid_typ))
    } else {
      home
    }
  
  #download the video
  download.file(url = what
                ,destfile = home
                ,mode = "wb"
  )
  setwd(homez)
  
  
}

#' extract frames and produce an image bank from a video file
#'
#' This function extracts frames from video and places them in your workspace with specific parameters.
#' @param vid the link to the video.
#' @param height the directory it will go to.
#' @param width a key work for the video to be named.
#' @param chnl a key work for the video to be named.
#' @param output a key work for the video to be named.
#' @keywords etl
#' @export
#' @examples
#'

etl_frames = function(vid,height,width,chnl,output) {
  
  vid = if(missing(vid)){vid = paste0(
    getwd(),"/","raw_vid_src",vid_typ
  )} else {vid}
  
  print(vid)
  
  vid_obj=Rvision::video(vid)
  
  output = if(missing(output)){".jpg"} else {output}
  
  h =
    if(missing(height)) {100} else {h}
  w =
    if(missing(width)) {100} else {w}
  chnl=
    if(missing(chnl)) {
      "BGR"
    } else {
      chnl
    }
  
  pblapply(1:nframes(vid_obj),function(x){
    
    
    cat = Rvision::readFrame(vid_obj,x)
    
    cat = Rvision::changeColorSpace(cat,chnl)
    
    #resize the image 100 x 100
    cat = Rvision::resize(cat, width = w, height=h)
    setwd(img_dirs$imagebank)
    
    print(paste0(getwd(),"/",x,output))
    Rvision::write.Image(cat
                         ,paste0(getwd(),"/",x,output))
    
    #
    setwd(homez)
    
  })
  
  Rvision::release(vid_obj)
  
}
