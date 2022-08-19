### Loading packages ###

library(geomorph)
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(shinyBS)
library(shinyFeedback)
library(shinycssloaders)
library(fontawesome)
library(Rvcg)
library(Morpho)
library(scales)
library(tools)
library(sp)
library(crosstalk)

sp_list <- list()
lm_list <- list()
spec.fl <- list.files("~/Dropbox/alignR/alignR/data/turts/input/", pattern = ".off")
spec.fl <- list.files("/Users/zach/Dropbox/AmniotePalateDiversity/Palatine_Meshes")


spec.lo <- c("/Users/zach/Dropbox/AmniotePalateDiversity/Palatine_Meshes")
spec.lo <- c("~/Dropbox/alignR/alignR/data/turts/input/")

for (i in seq(spec.fl)){
  sp_list[[i]] <- vcgImport(file.path(spec.lo,spec.fl[i]))
}

#create a list to store landmarks eventually
lm_list <- rep(list(NA),length(sp_list))
names(lm_list) <- spec.fl


#This will run the app, once the ui and server objects are stored in memory
#Need to change the server code to identify the surface object (line 41)

shinyApp(alignR_ui, alignR_server)

text_choose <- readLines(file.choose())

test_shapes <-readShapes("Landmarks.txt")

write.table(lm_list,file="Landmarks.txt",eol = "\n")
cat(sapply(lm_list, toString), "Landmarks.txt", sep="\n")
dput(lm_list, "Landmarks.txt")
#
# if(!is.null(json_list$save_as_shapes)){
  list2XML4R(list=lm_list, file="Landmarks.txt")
# }

## currently recieve this error "getObj id is undefined" when selecting a point ##

#This will be the ultimate wrapper for the function, which takes a file directory and then passes the list of objects to the app
#Doesn't work just yet
alignR(file_dir = spec.lo,
       loadAll = FALSE)

vert2points(scallopPLY$ply)
apply(vert2points(scallopPLY$ply),2,mean)

specimen <- scale(as.matrix(t(scallopPLY$ply$vb)[, -4]), scale = FALSE)
spec$vb <- rbind(t(specimen), 1)


test_matrix <- matrix(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6),
       ncol=3, byrow = T)
colnames(test_matrix) <- c("X","Y","Z")
lm_list[[2]] <- test_matrix

