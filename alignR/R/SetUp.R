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

# spec.lo <- c("/Users/zach/Dropbox/AmniotePalateDiversity/Palatine_Meshes")
spec.lo <- c("~/Dropbox/alignR/alignR/data/turts/input/")

#This will be the ultimate wrapper for the function, which takes a file directory and then passes the list of objects to the app
#Doesn't work just yet
alignR(file_dir = spec.lo, file_name = "TurtTest.txt")

turt_test <- readLandmarks("TurtTest.txt")

vert2points(scallopPLY$ply)
apply(vert2points(scallopPLY$ply),2,mean)

specimen <- scale(as.matrix(t(scallopPLY$ply$vb)[, -4]), scale = FALSE)
spec$vb <- rbind(t(specimen), 1)


test_matrix <- matrix(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6),
       ncol=3, byrow = T)
colnames(test_matrix) <- c("X","Y","Z")
lm_list[[2]] <- test_matrix

