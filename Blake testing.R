rm(list= ls())


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


# Tried to tidy up the code so that only functions are in ./R for easier loading and testing. Eventually need to tidy it for conversion into a package.


# source functions --------------------------------------------------------

AlignRfunctions <- file.path("./R", list.files("./R"))


for(i in 1:length(AlignRfunctions)){
  source(AlignRfunctions[i])
}


# run ---------------------------------------------------------------------



# changed absolute file paths to relative file paths


spec.lo <- c("./data/turts/input/")

#This will be the ultimate wrapper for the function, which takes a file directory and then passes the list of objects to the app
#Doesn't work just yet
# Not run
try(
  alignR(file_dir = spec.lo, file_name = "TurtTest.txt")

)

# alignR_ui is loaded from align_R.ui, but addResourcePath which doesn't work even when I change it to relative file path. No idea what this command does so i just commented it out. If its important, need to make sure the code is portable


try(
  addResourcePath(prefix = 'www', directoryPath = '~/Dropbox/alignR/alignR/www')

)

try(
 addResourcePath(prefix = 'www', directoryPath = './www')
)

source("./alignR_ui.R")


## Prints an error when I run.
# Warning: Error in grep: invalid 'pattern' argument

alignR(file_dir = spec.lo, file_name = "TurtTest.txt")

## I can navigate the ui. and rotate the model.

## I can click previous and next specimen, with the error
# Warning in if (class(tmp_values$coords) == "numeric") { :
# the condition has length > 1 and only the first element will be used

## I can click set posisiotn, but then crashes when i clck "landmark!" with the error:
# Warning: Error in matrix: 'data' must be of a vector type, was 'NULL'




