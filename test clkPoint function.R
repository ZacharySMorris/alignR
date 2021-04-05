rm(list = ls())
require(rgl)
require(Rvcg)

spec.fl <- list.files("./alignR/data/turts/input/", pattern = ".off")

i=1


mesh <- vcgImport(file.path("./alignR/data/turts/input/",spec.fl[i]))

tri <- t(mesh$vb)[mesh$it,1:3]


# select point ------------------------------------------------------------
# 


ptdist <- function(X,Y){
  if(is.vector(Y)){
    Y = t(as.matrix(Y))
  }
  
  XY = as.matrix(Y - X)
  colnames(XY) <- c("x", "y", "z")
  XYdist = apply(Y[,1:2], 1, function(x,y){dist(rbind(x,y))}, y= X[1:2])
  
  return(cbind(XY,XYdist))
}

par3d(ignoreExtent = F)

clear3d()
triangles3d(tri, col = "grey", alpha = 0.4, add= F)

clkPoint()
