
rotLandMesh <- function(landMesh, target, plot = F, scale = T, reflection = F){

  rotLand <- rotonto(target, landMesh$lands,
                     scale = scale, reflection = reflection)


  rotMesh  <- rotmesh.onto(mesh = landMesh$mesh,
                           tarmat = target, refmat = landMesh$lands,
                           scale = scale, reflection = reflection)


  if(plot){

    # clear3d()

    points3d(target, size = 15, col = "red", add=F)

    shade3d(rotMesh$mesh, col = "lightblue", alpha= 0.5)
    points3d(rotLand$yrot, size = 10, col = "orange")

  }

  return(list(rotLand = rotLand, rotMesh = rotMesh))

}


regLandMesh <- function(lands.array, meshes, target = "mshape", plot = F, scale = TRUE, reflection = FALSE){
  if(!is.array(lands.array)){
    stop("lands.array is not an array")
  }

  if(class(meshes[[1]]) != "mesh3d"){
    stop("meshes should be a list of meshes of class = mesh3d ")
  }

  if(dim(lands.array)[3] != length(meshes)){
    stop("length of lands.array does not match number of meshes")
  }
  if(!is.matrix(target)){
    if(target == "mshape"){
      target <- arrMean3(lands.array)
      attr(target, "target method") <- "mshape"
    }
  } else{
    if(!all(dim(lands.array)[1:2] == dim(target))){
      stop(cat("target dimensions do not match landmark array.",
               "Target should be a matrix of", dim(lands.array)[1], "x", dim(lands.array)[1], "landmarks"))
    }
    attr(target, "target method") <- "user"
  }

  target <- as.matrix(target)

  #set list for convenient rotation
  landMeshes <- list()
  for(i in 1: length(meshes)){
    landMeshes[[i]] <- list(lands = lands.array[,,i], mesh = meshes[[i]])
  }
  names(landMeshes) <- names(meshes)


  regList <- lapply(landMeshes, FUN = rotLandMesh,
                    target = target, scale, plot = plot, reflection = reflection)

  output <- list(LandMeshes = regList, target = target)
  class(output) <- "regLandMesh"
  attr(output, "target method") <- attr(target, "target method")
  attr(output, "scale") <- scale
  attr(output, "reflection") <- reflection
  attr(output, "array.dim") <- dim(lands.array)

  return(output)


}
