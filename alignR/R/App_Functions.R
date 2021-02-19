###Function to add coordinates to data.frame

saveLMs <- function(x,current_lm,keep) {
  if (exists("landmarks")){
    landmarks[as.numeric(current_lm),] <<- keep
  } else{
    landmarks <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
    landmarks[as.numeric(current_lm),] <<- keep
  }
}

loadLMs <- function() {
  if (exists("landmarks")) {
    landmarks
  }
}

clearLMs <- function(x){
  if (exists("landmarks")){
    landmarks <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
  }
}

next.sp <- function(x,n){
  if (x < n){
    next_spec <- x + 1
    return(next_spec)
  }else{
    return(1)
  }
}

prev.sp <- function(x,n){
  if (x > 1){
    next_spec <- x - 1
    return(next_spec)
  }else{
    return(n)
  }
}

write.lms <- function (landmarks,x){
    tmp <- paste("landmarks_", x, ".csv", sep="")
    write.csv(landmarks,file=tmp) #this writes it to a csv, which will be the ultimate condition
    ##should replace with something that it saves it to a data.frame with a specimen ID
}


###make a function to load in and make basic rgl window


###make a function that adds landmark points and updates rgl window



rgl.landmarking <- function(x, temp_scene, specimen) {
  current_lm <- as.numeric(x)

  LM_ids <- temp_scene[["objects"]][[1]]["id"]

  keep <- ans <- NULL
  keep <- selectpoints3d(LM_ids, value = FALSE, button = "right")[2]

  return(keep)
}
