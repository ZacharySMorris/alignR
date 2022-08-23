rm(list = ls())

# install.packages("RANN")

require(RANN)

# install.packages("rgl")
require(rgl)
# install.packages("Rvcg")
require(Rvcg)
# install.packages("StereoMorph")
require(StereoMorph)


# load meshes -------------------------------------------------------------


mesh.fl <- list.files("./data/turts/input", pattern =".off")

mesh.fp <- file.path("./data/turts/input", mesh.fl)

mesh.names <- gsub(mesh.fl, pattern = ".off", replacement = "")

meshes <- lapply(mesh.fp, vcgImport)
names(meshes) <- mesh.names

# clear3d()
# shade3d(meshes[[1]], col = "grey", alpha= 0.5)



# load LMs and coerce in array --------------------------------------------

# lands <- as.matrix(read.csv("./data/turts/fixed landmarks.csv", row.names = 1))

# lands.array <- array(NA, dim = c(3,3,ncol(lands)/3), dimnames = list(NULL,NULL ,mesh.names))
#
# i = 1
#
# nc <- seq(1,ncol(lands), by = 3)
#
# for(i in 1:(ncol(lands)/3)){
#
#   rowind = nc[i] + c(0,1,2)
#
#   lands.array[,,i] <- lands[,rowind]
#
# }
#
# lands.array


# save(lands.array, file = "landsArray.Rdata")


# load LM array -----------------------------------------------------------

load("landsArray.Rdata")

# install.packages("Morpho")
require("Morpho")
mshape <- arrMean3(lands.array)


# clear3d()
# points3d(lands.array[,,1], size = 10, col = "red")
#
# # looking good!
#
# # dimnames(lands.array)[[3]] %in%  mesh.names
#
# # Fixed LM alignment/registration -----------------------------------------
#
# require(Morpho)
#
#
#
# rot <- rotonto(lands.array[,,1], lands.array[,,2],
#                scale = T, reflection = F, )
#
#
# rotmesh <- rotmesh.onto(mesh = meshes[[2]],
#                         tarmat = lands.array[,,1], refmat = lands.array[,,2],
#              scale = T)
#
#
# clear3d()
#
# shade3d(meshes[[1]], col = "grey", alpha= 0.5)
# points3d(lands.array[,,1], size = 10, col = "red")
#
# shade3d(rotmesh$mesh, col = "lightblue", alpha= 0.5)
# points3d(rot$yrot, size = 10, col = "orange")
#
#
# # try with meanshape
#
#



#
# rot <- rotonto(mshape, lands.array[,,2],
#                scale = T, reflection = F, )
#
#
# rotmesh <- rotmesh.onto(mesh = meshes[[2]],
#                         tarmat = mshape, refmat = lands.array[,,2],
#                         scale = T)
#
#
# clear3d()
#
#
#
# points3d(mshape, size = 10, col = "red")
#
# shade3d(rotmesh$mesh, col = "lightblue", alpha= 0.5)
# points3d(rot$yrot, size = 10, col = "orange")
#
#
#
#
#
# # test reverse
#
# revPoints <- rotreverse(mat = rot$yrot, rot = rot)
#
# revMesh <- rotreverse(mat = rotmesh$mesh, rot = rot)
#
# clear3d()
#
# shade3d(meshes[[2]], col = "grey", alpha= 0.5)
# points3d(lands.array[,,2], size = 10, col = "red")
#
# wire3d(revMesh, col = "lightblue", alpha= 0.7)
# points3d(revPoints, size = 20, col = "orange")
#
# # WORKING!!!


# RECURSIVE ALIGMENT ------------------------------------------------------

require(Morpho)

source("./R/alignR_alignFunctions.R")



#
# clear3d()
# points3d(mshape, size =10)

# align all landmarks and meshes to mshape

lands.array <- lands.array
meshes <- meshes
target <- mshape


landMeshes <- list()
for(i in 1: length(meshes)){
  landMeshes[[i]] <- list(lands = lands.array[,,i], mesh = meshes[[i]])
}
names(landMeshes) <- names(meshes)


landMeshes[[1]]

target

# 4*6
# mfrow3d(4,6, sharedMouse = T)
#
# for(i in 1:length(landMeshes)){
#
#   rotLandMesh(landMeshes[[i]],target = target, plot = T)
#   next3d()
# }

alignedLandMesh <- regLandMesh(lands.array, meshes, scale = T, plot = F)



# try a shitty for loop


mesh1 <- alignedLandMesh$LandMeshes$`Amyda_cartilaginea_R-MCZHERP_R-1632`$rotMesh$mesh

mesh1verts <- t(mesh1$vb[1:3,])

land1 <- alignedLandMesh$LandMeshes$`Amyda_cartilaginea_R-MCZHERP_R-1632`$rotLand$yrot


clear3d()
points3d(mesh1)
points3d(land1, size = 20, col = "red")

comb1 <- rbind(land1, mesh1)


# farthest point sampling -------------------------------------------------
#
# install.packages("rdist", dependencies = T)
#
# install.packages('Rcpp')
# library(Rcpp)
#
# require("rdist")
#
# dist_mat <- pdist(mesh1)
# # farthest point sampling
# fps3 <- farthest_point_sampling(dist_mat)
# # fps2 <- farthest_point_sampling(df, metric = "euclidean")
# # all.equal(fps, fps2)
# # have a look at the fps distance matrix
# rdist(df[fps2[1:5], ])
# dist_mat[fps2, fps2][1:5, 1:5]

# VERY SLOW


# ??resampling using fastKmeans  --------------------------------------------

?fastKmeans

KmeansMesh <- fastKmeans(mesh1, k = 1024)


clear3d()
points3d(mesh1)
points3d(land1, size = 20, col = "red")

comb1 <- rbind(land1, mesh1)
points3d(KmeansMesh$centers, size = 5, col = "green")
mesh1verts[KmeansMesh$selected,]

# plot two meshes with landmarks ------------------------------------------

