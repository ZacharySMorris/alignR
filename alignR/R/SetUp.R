### Loading packages ###

library(geomorph)
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(shinyBS)
library(shinyFeedback)
library(fontawesome)
library(Rvcg)
library(Morpho)

sp_list <- list()
spec.fl <- list.files("~/Dropbox/alignR/alignR/data/turts/input/", pattern = ".off")

for (i in c(1:5)){
  sp_list[[i]] <- vcgImport(file.path("~/Dropbox/alignR/alignR/data/turts/input/",spec.fl[i]))
}



#This will run the app, once the ui and server objects are stored in memory
#Need to change the server code to identify the surface object (line 41)
shinyApp(alignR_ui, alignR_server)

## currently recieve this error "getObj id is undefined" when selecting a point ##

#This will be the ultimate wrapper for the function, which takes a file directory and then passes the list of objects to the app
#Doesn't work just yet
alignR(file_dir = ,
       loadAll = TRUE)

vert2points(scallopPLY$ply)
apply(vert2points(scallopPLY$ply),2,mean)

specimen <- scale(as.matrix(t(scallopPLY$ply$vb)[, -4]), scale = FALSE)
spec$vb <- rbind(t(specimen), 1)
