### Loading packages ###

library(geomorph)
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(Rvcg)

sp_list <- list()
data("scallopPLY")
scallopPLY
sp_list[[1]] <- scallopPLY$ply
sp_list[[2]] <- vcgImport(dir(file_dir, full.names = TRUE))
sp_list[[2]] <- scallopPLY$ply
sp_list[[3]] <- scallopPLY$ply
sp_list[[4]] <- scallopPLY$ply
sp_list[[5]] <- scallopPLY$ply


#This will run the app, once the ui and server objects are stored in memory
#Need to change the server code to identify the surface object (line 41)
shinyApp(alignR_ui, alignR_server)


#This will be the ultimate wrapper for the function, which takes a file directory and then passes the list of objects to the app
#Doesn't work just yet
alignR(file_dir = ,
       loadAll = TRUE)
