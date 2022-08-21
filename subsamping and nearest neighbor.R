rm(list = ls())

install.packages("RANN")

require(RANN)
require(rgl)
require(Rvcg)
require(StereoMorph)


# load meshes -------------------------------------------------------------


mesh.fl <- list.files("./data/turts/input", pattern =".off")

mesh.fp <- file.path("./data/turts/input", mesh.fl)

mesh.names <- gsub(mesh.fl, pattern = ".off", replacement = "")

meshes <- lapply(mesh.fp[1:3], vcgImport)

clear3d()
wire3d(meshes[[1]])


# load landmarks ----------------------------------------------------------

lands <- read.csv("./data/turts/fixed landmarks.csv", row.names = 1)

# only test forst 3 for now




lands.array <- array(NA, dim = c(3,3,ncol(lands)/3), dimnames = list(NULL,NULL ,mesh.names))

d

for(i in 1:(ncol(lands)/3)){




}


