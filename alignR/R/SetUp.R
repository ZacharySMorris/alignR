### Loading packages ###

library(geomorph)
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(StereoMorph)


#This will run the app, once the ui and server objects are stored in memory
#Need to change the server code to identify the surface object (line 41)
shinyApp(alignR_ui, alignR_server)


#This will be the ultimate wrapper for the function, which takes a file directory and then passes the list of objects to the app
#Doesn't work just yet
alignR(file_dir = ,
       loadAll = TRUE)
