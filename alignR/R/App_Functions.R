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


rgl.landmarking <- function(x, temp_scene, specimen) {
  current_lm <- as.numeric(x)

  LM_ids <- temp_scene[["objects"]][[1]]["id"]

  keep <- ans <- NULL
  keep <- selectpoints3d(LM_ids, value = FALSE, button = "right")[2]

  return(keep)
}
