###Function to add coordinates to data.frame

saveLMs <- function(x,current_lm,keep) {
  if (exists("landmarks")){
    landmarks[as.numeric(current_lm),] <<- keep
  } else{
    landmarks <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
    landmarks[as.numeric(current_lm),] <<- keep
  }
}

loadLMs <- function() {
  if (exists("landmarks")) {
    landmarks
  }
}

clearLMs <- function(x){
  if (exists("landmarks")){
    landmarks <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
  }
}


saveVerts <- function(x,current_lm,keep) {
  if (exists("vertices")){
    vertices[as.numeric(current_lm),] <<- keep
  } else{
    vertices <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
    vertices[as.numeric(current_lm),] <<- keep
  }
}

loadVerts <- function() {
  if (exists("vertices")) {
    vertices
  }
}

clearVerts <- function(x){
  if (exists("vertices")){
    vertices <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
  }
}

next.sp <- function(x,n){
  if (x < n){
    next_spec <- x + 1
    return(next_spec)
  }else{
    return(1)
  }
}

prev.sp <- function(x,n){
  if (x > 1){
    next_spec <- x - 1
    return(next_spec)
  }else{
    return(n)
  }
}

write.lms <- function (landmarks,x){
    tmp <- paste("landmarks_", x, ".csv", sep="")
    write.csv(landmarks,file=tmp) #this writes it to a csv, which will be the ultimate condition
    ##should replace with something that it saves it to a data.frame with a specimen ID
}


###make a function to load in and make basic rgl window


###make a function that adds landmark points and updates rgl window



rgl.landmarking <- function(x, temp_scene, specimen) {
  current_lm <- as.numeric(x)

  LM_ids <- temp_scene[["objects"]][[1]]["id"]

  keep <- ans <- NULL
  keep <- selectpoints3d(LM_ids, value = FALSE, button = "right")[2]

  return(keep)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# rgl.convertLandmark3d <- function (proj, region = proj$region) {
#   llx <- region[1]
#   lly <- region[2]
#   urx <- llx + 0.05
#   ury <- lly + 0.05
#   if (llx > urx) {
#     temp <- llx
#     llx <- urx
#     urx <- temp
#   }
#   if (lly > ury) {
#     temp <- lly
#     lly <- ury
#     ury <- temp
#   }
#   proj$view["x"] <- proj$view["y"] <- 0
#   function(x, y = NULL, z = NULL) {
#     pixel <- rgl.user2window(x, y, z, projection = proj)
#     x <- pixel[, 1]
#     y <- pixel[, 2]
#     z <- pixel[, 3]
#     (llx <= x) & (x <= urx) & (lly <= y) & (y <= ury) & (0 <=
#                                                            z) & (z <= 1)
#   }
# }
#
#
# rgl.convertLandmark3d <- function (proj, region = proj$region) {
#   rgl_x <- region[1]
#   rgl_y <- region[2]
#
#   proj$view["x"] <- proj$view["y"] <- 0
#   function(x, y = NULL, z = NULL) {
#     pixel <- rgl.user2window(x, y, z, projection = proj)
#     x <- pixel[, 1]
#     y <- pixel[, 2]
#     z <- pixel[, 3]
#     (x <= rgl_x <= x) & (y <= rgl_y <= y) & (0 <= z) & (z <= 1)
#   }
# }

# better_modalButton <- function(style, label, icon = NULL){
#     tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", style = style,
#                 validateIcon(icon), label)
# }


shinySelectPoints3d <- function(specimen, verts, par_input, shinyBrush){
  specimen <- specimen
  verts <- verts
  tmp_proj <- shinyEnvironment(par_input)
  click_x <- shinyBrush$region[1]
  click_y <- shinyBrush$region[2]


  tmp_coords <- rgl.user2window(x=verts, projection = tmp_proj) #extracts the window coordinates that correspond to the vertices of the shape given the projection
  tmp_z <- tmp_coords[, 3] #grab the z values
  keep <- (0 <= tmp_z) & (tmp_z <= 1) #keep only those z coords b/w zero and one

  tmp_x <- tmp_coords[, 1] #grab the x values of the window? coordinates
  x_dist <- (tmp_x - click_x)^2

  tmp_y <- tmp_coords[, 2]
  y_dist <- (tmp_y - click_y)^2

  dist <- x_dist + y_dist #adds the x and y distributions together
  sort_dist <- sort.int(dist, decreasing = TRUE, index.return = TRUE)$ix

  # nearest_verts <- verts[match(c(1,2,3),sort_dist),]
  verts <- verts[match(c(1),sort_dist),,drop = F]
  coords <- specimen[match(c(1),sort_dist),,drop = F]

  return(list("verts" = verts,
              "coords" = coords))
}


shinyUserProj <- function(par_input){
  #par_input from shinyGetPar3d to get the model, projection, and view for the current orientation of the shape in the rglwidget
  x <- list()
  # temp_model <- par_input$userMatrix
  # temp_model[,4]<- t(par_input$modelMatrix[4,])
  # x[["model"]] <- temp_model
  x[["model"]] <- par_input$modelMatrix
  x[["proj"]] <- par_input$projMatrix
  #need to create viewport that actually lists the proper pixel dimensions of the rgl window (viewport just gives window dimensions of 1)
  # temp_view <- par_input$viewport
  # temp_view[1:4] <- par_input$windowRect
  # x[["view"]] <- temp_view
  x[["view"]] <- par_input$viewport
  return(x)
}

shinyWindowProj <- function(par_input){
  #par_input from shinyGetPar3d to get the model, projection, and view for the current orientation of the shape in the rglwidget
  x <- list()
  x[["model"]] <- par_input$modelMatrix
  x[["proj"]] <- par_input$projMatrix
  #need to create viewport that actually lists the proper pixel dimensions of the rgl window (viewport just gives window dimensions of 1)
  # temp_view <- par_input$viewport
  # temp_view[1:4] <- par_input$windowRect
  # x[["view"]] <- temp_view
  x[["view"]] <- par_input$viewport
  return(x)
}

shinyClickLine <- function(par_input, shinyBrush){

  tmp_proj <- par_input
  tmp_click <- shinyBrush$region[c(1,2)]
  dbl_click <- c(tmp_click,tmp_click)
  tmp_matrix <- cbind(matrix(dbl_click,ncol = 2, byrow = T), c(0,1))

  win_line <- rgl.window2user(x=tmp_matrix, projection = tmp_proj)

  return(list("clickpoint" = tmp_matrix[1,], "clickline" = win_line, "proj" = tmp_proj))
}

shinyTriangleDist <- function(centers, verts, tris, N, par_input, shinyBrush){
  #centers is an array of the center points for each triangle
  #verts is an array of triangle verticies for the mesh
  #tris is a matrix containig vertex indices of the triangles of the mesh
  #N is the number of triangles
  #par_input is an input from shinyGetPar3d to get the model, projection, and view for the current orientation of the shape in the rglwidget
  #shinyBrush contains the output of the click

  tmp_centers <- centers
  tmp_verts <- verts
  tmp_tris <- tris
  ntri <- N
  tmp_proj <- par_input
  tmp_click <- c(shinyBrush$region[c(1,2)],c(0)) #sets the z coordinate to 0

  win_centers <- rgl.user2window(x=tmp_centers, projection = tmp_proj) #extracts the window coordinates that correspond to the vertices of the shape given the projection
  # win_verts <- rgl.user2window(x=tmp_verts, projection = tmp_proj) #extracts the window coordinates that correspond to the vertices of the shape given the projection

  if(is.vector(win_centers)){
    win_centers = t(as.matrix(win_centers))
    }

  tri_diff = as.matrix(win_centers - tmp_click)
  colnames(tri_diff) <- c("x", "y", "z")
  tri_dist = apply(win_centers[,1:2], 1, function(x,y){dist(rbind(x,y))}, y= tmp_click[1:2])

  tri_data <- cbind(tri_diff,tri_dist)
  tri_order <- order(tri_data[,4], decreasing = F)[1:ntri]

  tri.i <- seq(1, nrow(tmp_tris),by = 3 )[tri_order] # make a dummy triangle index
  tri.v <- as.vector(sapply(tri.i, FUN = function(X,Y) X + c(0:2))) # triangle vertex index

  nearestCenters <- tmp_centers[tri_order,] # these are the scene coords of the nearest triangle centers
  nearestTris <- tmp_verts[tri.v,] # these are the scene coords of the triangle vertices

  return(list("coords" = nearestCenters,
              "tris" = nearestTris
              ))
}

MeshManager <- function(object, size = 1, center = TRUE){
  ###
  # this function is to do some pre-processing of the mesh file, including determining the file type,
  # if the file should be centered, and checking coloring of the mesh
  # object is a mesh object as output by vcgImport or a matrix of values
  # size determines the size of vertex points for plot3d
  # center is a logical value of whether the specimen should be centered or not
  # this function is based off of the code in digit.fixed from Geomorph 3.0.0
  ###

  spec <-  object
  ptsize <- size
  center <- center


  spec.name <- deparse(substitute(spec))
  mesh <- NULL

  # if object is a shape3d or mesh3d file, then proceed as normal
  if (inherits(spec, "shape3d") == TRUE || inherits(spec, "mesh3d") ==
      TRUE) {
    if (center == TRUE) {
      specimen <- scale(as.matrix(t(spec$vb)[, -4]), scale = FALSE)
      spec$vb <- rbind(t(specimen), 1)
    }
    if (center == FALSE) {
      specimen <- as.matrix(t(spec$vb)[, -4])
    }
    # create mesh and check for material colors
    mesh <- spec
    if (is.null(mesh$material))
      mesh$material$color <- "gray"
    if (is.null(mesh$material$color))
      mesh$material$color <- "gray"
  }

  # if the object is not a mesh object or a matrix, then we want to stop and give an error
  else if (inherits(spec, "matrix") == FALSE) {
    stop("File is not a shape3d/mesh3d object or xyz matrix") #need to replace with an actual modal warning
  }
  # if the object is an appropriately formatted matrix, then we can move to centering
  else if (inherits(spec, "matrix") == TRUE && dim(spec)[2] ==
           3) {
    if (center == TRUE) {
      specimen <- scale(spec, scale = FALSE)
    }
    if (center == FALSE) {
      specimen <- spec
    }
  }
  # give warning if it is a matrix, but not properly formatted, then give a error
  else {
    stop("File is not matrix in form: vertices by xyz") #need to replace with an actual modal warning
  }


  return(list("specimen" = specimen, "mesh" = mesh, "ptsize" = ptsize))

}
