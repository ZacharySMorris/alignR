##Functions for working within alignR's shiny environment

# #function for controlling app loading screen
# load_data <- function() {
#   Sys.sleep(2)
#   hide("loading_page")
#   show("main_content")
# }

###Function to add coordinates to data.frame

changeAnalysis <- function(current_tab, next_tab){
  current_tab <- current_tab
  next_tab <- next_tab

  shinyalert(
    title = "",
    text = "Changing analyses will delete all current landmark data. \n Do you still want to proceed?",
    size = "xs",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "success",
    showConfirmButton = TRUE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes!",
    confirmButtonCol = "#AEDEF4",
    cancelButtonText = "No!",
    timer = 0,
    imageUrl = "https://fontawesome.com/icons/question-circle?style=light",
    animation = TRUE,
    callbackR = function(x) {
      print(x)
      if(x){

        updateNumericInput(session,inputId = "n", value = 10)
        shinyjs::showElement(id="n")
        tab_n(next_tab)
        shinyjs::disable(paste("tab", next_tab, sep = ""))
        shinyjs::enable('tab2')
        shinyjs::enable('tab3')

      } else{
        click(paste("tab", current_tab, sep = ""))
      }
    }
  )
  }


saveLMs <- function(lm_data,current_sp,current_lm,lm_labels,keep) {
  if (exists(names(lm_data)[current_sp],where=lm_data,mode="numeric")){
    landmarks <- lm_data[[current_sp]]
    landmarks[current_lm,] <- keep
  } else{
    n <- length(lm_labels)
    landmarks <- array(NA, dim = c(n,3), dimnames = list(lm_labels, c("X","Y","Z")))
    landmarks[current_lm,] <- keep
  }
  return(landmarks)
}

loadLMs <- function(lm_data,current_sp,lm_labels) {
  n <- length(lm_labels)
  if (exists(names(lm_data)[current_sp],where=lm_data,mode="numeric")){
    landmarks <- lm_data[[current_sp]]
  } else {
    landmarks <- array(NA, dim = c(n,3), dimnames = list(c(lm_labels), c("X","Y","Z")))
  }
  return(landmarks)
}

checkLMs <- function(current_lm,check) {
  landmarks <- array(check, dim = c(1,3), dimnames = list(current_lm, c("X","Y","Z")))
  return(landmarks)
}

# printLMs <- function() {
#   if (exists("landmarks")) {
#     return(landmarks)
#   }
# }

clearLMs <- function(lm_labels){
  n <- length(lm_labels)
  landmarks <<- array(NA, dim = c(n,3), dimnames = list(c(lm_labels), c("X","Y","Z")))
}


# saveVerts <- function(x,current_lm,keep) {
#   if (exists("vertices")){
#     vertices[as.numeric(current_lm),] <<- keep
#   } else{
#     vertices <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
#     vertices[as.numeric(current_lm),] <<- keep
#   }
# }
#
# loadVerts <- function() {
#   if (exists("vertices")) {
#     vertices
#   }
# }
#
# clearVerts <- function(x){
#   if (exists("vertices")){
#     vertices <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
#   }
# }

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


##I don't remember why I made this...
rgl.landmarking <- function(x, temp_scene, specimen) {
  current_lm <- as.numeric(x)

  LM_ids <- temp_scene[["objects"]][[1]]["id"]

  keep <- ans <- NULL
  keep <- rgl::selectpoints3d(LM_ids, value = FALSE, button = "right")[2]

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


# shinySelectPoints3d <- function(specimen, verts, par_input, shinyBrush){
#   specimen <- specimen
#   verts <- verts
#   tmp_proj <- shinyEnvironment(par_input)
#   click_x <- shinyBrush$region[1]
#   click_y <- shinyBrush$region[2]
#
#
#   tmp_coords <- rgl.user2window(x=verts, projection = tmp_proj) #extracts the window coordinates that correspond to the vertices of the shape given the projection
#   tmp_z <- tmp_coords[, 3] #grab the z values
#   keep <- (0 <= tmp_z) & (tmp_z <= 1) #keep only those z coords b/w zero and one
#
#   tmp_x <- tmp_coords[, 1] #grab the x values of the window? coordinates
#   x_dist <- (tmp_x - click_x)^2
#
#   tmp_y <- tmp_coords[, 2]
#   y_dist <- (tmp_y - click_y)^2
#
#   dist <- x_dist + y_dist #adds the x and y distributions together
#   sort_dist <- sort.int(dist, decreasing = TRUE, index.return = TRUE)$ix
#
#   # nearest_verts <- verts[match(c(1,2,3),sort_dist),]
#   verts <- verts[match(c(1),sort_dist),,drop = F]
#   coords <- specimen[match(c(1),sort_dist),,drop = F]
#
#   return(list("verts" = verts,
#               "coords" = coords))
# }


# shinyUserProj <- function(par_input){
#   #par_input from shinyGetPar3d to get the model, projection, and view for the current orientation of the shape in the rglwidget
#   x <- list()
#   # temp_model <- par_input$userMatrix
#   # temp_model[,4]<- t(par_input$modelMatrix[4,])
#   # x[["model"]] <- temp_model
#   x[["model"]] <- par_input$modelMatrix
#   x[["proj"]] <- par_input$projMatrix
#   #need to create viewport that actually lists the proper pixel dimensions of the rgl window (viewport just gives window dimensions of 1)
#   # temp_view <- par_input$viewport
#   # temp_view[1:4] <- par_input$windowRect
#   # x[["view"]] <- temp_view
#   x[["view"]] <- par_input$viewport
#   return(x)
# }
#
# shinyWindowProj <- function(par_input){
#   #par_input from shinyGetPar3d to get the model, projection, and view for the current orientation of the shape in the rglwidget
#   x <- list()
#   x[["model"]] <- par_input$modelMatrix
#   x[["proj"]] <- par_input$projMatrix
#   #need to create viewport that actually lists the proper pixel dimensions of the rgl window (viewport just gives window dimensions of 1)
#   # temp_view <- par_input$viewport
#   # temp_view[1:4] <- par_input$windowRect
#   # x[["view"]] <- temp_view
#   x[["view"]] <- par_input$viewport
#   return(x)
# }

shinyClickLine <- function(par_input, shinyBrush){

  tmp_proj <- par_input
  tmp_click <- shinyBrush$region[c(1,2)]
  dbl_click <- c(tmp_click,tmp_click)
  tmp_matrix <- cbind(matrix(dbl_click,ncol = 2, byrow = T), c(0,1))

  win_line <- rgl::rgl.window2user(x=tmp_matrix, projection = tmp_proj)

  return(list("clickpoint" = tmp_matrix[1,], "clickline" = win_line, "proj" = tmp_proj))
}


shinySelectPoints3d <- function(centers, verts, tris, N, par_input, shinyBrush){
  #centers is an array of the center points for each triangle
  #verts is an array of triangle verticies for the mesh
  #tris is a matrix containig vertex indices of the triangles of the mesh
  #N is the number of triangles
  #par_input is an input from shinyGetPar3d to get the model, projection, and view for the current orientation of the shape in the rglwidget
  #shinyBrush contains the output of the click

  ## load in variables
  tmp_centers <- centers
  tmp_verts <- verts
  tmp_tris <- tris
  ntri <- N
  tmp_proj <- par_input
  tmp_click <- shinyBrush$region[1:2]
  ##
  ## make click matrix, single click value, and convert to click to user coordinate values
  dbl_click <- c(tmp_click,tmp_click)
  click_matrix <- cbind(matrix(dbl_click,ncol = 2, byrow = T), c(0,1))
  sgl_click <- click_matrix[1,]

  # print("inital click_matrix:")
  # print(click_matrix)
  # print("inside tmp_proj:")
  # print(tmp_proj)

  user_click <- rgl::rgl.window2user(x=click_matrix, projection = tmp_proj)
  colnames(user_click) <- c("x", "y", "z")
  ##
  ## convert centers and verts of triangles to window coordinate values
  win_centers <- rgl::rgl.user2window(x=tmp_centers, projection = tmp_proj) #extracts the window coordinates that correspond to the centers given the projection
  win_verts <- rgl::rgl.user2window(x=tmp_tris, projection = tmp_proj) #extracts the window coordinates that correspond to the vertices of the shape given the projection
  if(is.vector(win_centers)){
    win_centers = t(as.matrix(win_centers))
  }
  if(is.vector(win_verts)){
    win_verts = t(as.matrix(win_verts))
  }
  ##
  ## calculate distance between triangle centers and the click coordinate
  tri_diff = as.matrix(win_centers - sgl_click)
  colnames(tri_diff) <- c("x", "y", "z")
  tri_dist = apply(win_centers[,1:2], 1, function(x,y){dist(rbind(x,y))}, y= tmp_click[1:2])
  ##

  tri_data <- cbind(tri_diff,tri_dist)
  tri_order <- order(tri_data[,4], decreasing = F)[1:ntri] # get closest centers sorted by window XY distance

  # print(cat("tri_data 1: ", class(tri_data), ", ", dim(tri_data)))
  # print(head(tri_data))
  # print(cat("tri_order: ", class(tri_order), ", ", length(tri_order)))
  # print(tri_order)

  # tri.i <- seq(1, nrow(tmp_tris),by = 3 )[tri_order] # make a dummy triangle index
  # tri.v <- as.vector(sapply(tri.i, FUN = function(X,Y) X + c(0:2))) # triangle vertex index

  tri_index <- seq(1, nrow(tmp_tris),by = 3 )[tri_order] # make a dummy triangle index
  tri.v <- as.vector(sapply(tri_index, FUN = function(X,Y) X + c(0:2))) # triangle vertex index
#
#   print(cat("tri_index: ", class(tri_index), ", ", length(tri_index)))
#   print(tri_index)
#   print(cat("tri.v: ", class(tri.v), ", ", length(tri.v)))
#   print(tri.v)

  # nearestCenters <- tmp_centers[tri_order,] # these are the scene coords of the nearest triangle centers
  # nearestTris <- tmp_verts[tri.v,] # these are the scene coords of the triangle vertices
  # near_wintris <- win_verts[tri.v,] # window coords of nearest triangles
  objtris <- tmp_verts[tri.v,] # mesh coords of the nearest triangles
  # rownames(near_wintris) <- tri.v

  # print(cat("objtris: ", class(objtris), dim(objtris)))
  # print(head(objtris))

  require(sp)

  # print("click_matrix:")
  # print(click_matrix)
  # print("tri_index:")
  # print(head(tri_index))
  # print("win_verts:")
  # print(head(win_verts))
  #
  # print(point.in.polygon(click_matrix[1,1],click_matrix[1,2], win_verts[tri_index[1]+c(0:2),1],win_verts[tri_index[1]+c(0:2),2]))


  pt.inside.tri <- logical(length = length(tri_index))
  pt.inside.tri_values <- c()

  for (i in 1:length(tri_index)){
    cur_tri <- tri_index[i] + c(0:2)
    X <- win_verts[cur_tri,]

    cur_ans <- sp::point.in.polygon(sgl_click[1], sgl_click[2], X[,1], X[,2])

    pt.inside.tri[[i]] <- cur_ans == 1
    pt.inside.tri_values[[i]] <- cur_ans
  }

  # pt.inside.tri <- sapply(X = tri_index, FUN = function(X,PT,TRI){
  #   (point.in.polygon(PT[1,1],PT[1,2], TRI[X+c(0:2),1],TRI[X+c(0:2),2])==1)
  # }, PT = click_matrix, TRI = win_verts)


  if (! 1 %in% pt.inside.tri){
    # tmp_thing <- tmp_tris == tmp_verts
    # print(cat("tmp_verts = tmp_tris: ", ! FALSE %in% tmp_thing))
    # print(cat("pt.inside.tri: ", class(pt.inside.tri), length(pt.inside.tri)))
    # print(pt.inside.tri)
    # print(all(pt.inside.tri_values == 1))
    # print(cat("pt.inside.tri_values: ", class(pt.inside.tri_values), length(pt.inside.tri_values)))
    # print(pt.inside.tri_values)
    # print(cat("click_matrix: ", class(click_matrix), click_matrix))

      # showToast(type = "error",
      #           message = "Click could not find triangles.",
      #           title = " Please reposition and attempt landmarking.",
      #           keepVisible = TRUE,
      #           .options = list(positionClass = "toast-top-center", closeButton = TRUE, progressBar = FALSE)
      # )

      error_click <- rgl::rgl.window2user(x=cbind(0.5,0.5,0.5), projection = tmp_proj)
      colnames(error_click) <- c("x", "y", "z")

      return(list("coords" = error_click,
                  "tris" = objtris,
                  "error" = TRUE,
                  # "user_click" = user_click,
                  # "win_centers" = win_centers,
                  # "win_verts" = win_verts,
                  # "tri_diff" = tri_diff,
                  # "tri_dist" = tri_dist,
                  # "tri_data" = tri_data,
                  # "tri_order" = tri_order,
                  # "tri_index" = tri_index,
                  # "tri.v" = tri.v,
                  # "objtris" = objtris
                  )
      )
  } else {
    subtending.tri.ind <- t(sapply(tri_index[pt.inside.tri], FUN = function(X) X+c(0:2)))

    # print(cat("subtending.tri.ind: ", class(subtending.tri.ind), dim(subtending.tri.ind)))
    # print(subtending.tri.ind)
    # print(t(subtending.tri.ind)[1:3])

    firstTRI <- mean(win_verts[t(subtending.tri.ind)[1:3],3]) <  mean(win_verts[t(subtending.tri.ind)[4:6],3])

    if(firstTRI){

      clktri <- tmp_verts[t(subtending.tri.ind)[1:3],]

    } else{

      clktri <- tmp_verts[t(subtending.tri.ind)[4:6],]

    }

    # CALC CLK COORDS
    decomptri<- stats::prcomp(as.matrix(clktri))
    pcvect <- as.data.frame(predict(decomptri,user_click))

    a = pcvect[1,]
    b = pcvect[2,]

    t = (0 - a[3])/ (b[3] - a[3])

    ptcoords <- a+unlist(lapply((b-a), FUN = function(X,Y) Y*X, Y=t))

    clkpt <- (as.matrix(ptcoords) %*% t(decomptri$rotation) ) + decomptri$center
    # print(cat("coords: ", clkpt))

    return(list("coords" = c(clkpt),
                "tris" = objtris,
                "error" = FALSE
                # "user_click" = user_click,
                # "win_centers" = win_centers,
                # "win_verts" = win_verts,
                # "tri_diff" = tri_diff,
                # "tri_dist" = tri_dist,
                # "tri_data" = tri_data,
                # "tri_order" = tri_order,
                # "tri_index" = tri_index,
                # "tri.v" = tri.v,
                # "objtris" = objtris
                )
    )
    }
}

MeshManager <- function(object, color = "gray", size = 1, center = FALSE){
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
  color <- color

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
    mesh$material$color <- color
  }

  # if the object is not a mesh object or a matrix, then we want to stop and give an error
  else {
    stop("File is not a shape3d/mesh3d object") #need to replace with an actual modal warning
  }

  return(list("specimen" = specimen, "mesh" = mesh, "ptsize" = ptsize))

}

alignRPar3d <- function(x,zoom){
  #x should be input$par3d which has been updated via shinyGetPar3d
  rgl::rgl.viewpoint(userMatrix = x$userMatrix, zoom = zoom)
  tmp_par <- rgl::rgl.projection()
  # tmp_par$zoom <- x$zoom
  # print("cur_par output:")
  # print(tmp_par)
  shinyjs::showElement(id = "submitLM")
  # hideElement(id = "getPar")

  return(tmp_par)
}

alignRListen <- function(x,zoom){
  #x should be input$par3d which has been updated via shinyGetPar3d
  rgl::rgl.viewpoint(userMatrix = x$userMatrix, zoom = zoom)
  int <- rgl::rgl.projection()
  init$pos <- c(x/int$viewport[3], 1 - y/int$viewport[4], 0.5)
  # tmp_par$zoom <- x$zoom
  # print("cur_par output:")
  # print(tmp_par)
  shinyjs::showElement(id = "submitLM")
  # hideElement(id = "getPar")

  return(int)
}

# shiny_listeners <- reactive{
#   shinyGetPar3d(c("listeners"), session)
#   return(input$par3d$listeners) #assign "listeners" to value
# }
#
# observe{
#   shinyGetPar3d(c("scale","listeners","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
#   tmp_par <- alignRPar3d(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
#
#   start_int <<- alignRListen(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
#
#
#   setUserCallbacks("right",
#                    begin = begin <- function(x, y) {
#                      shinyGetPar3d(c("scale","listeners","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
#                      tmp_par <- alignRPar3d(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
#                      start_int <<- alignRListen(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
#                      },
#                    update =   update <- function(x, y) {
#                        init <- start_int
#                        xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
#                        mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
#                        par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
#                      }
#                    )
# }
#
#
#
#
#
#
# pan3d <- function(button, dev = cur3d(), subscene = currentSubscene3d(dev)) {
#   start <- list()
#
#   begin <- function(x, y) {
#     activeSubscene <- par3d("activeSubscene", dev = dev) #get activeSubscene
#     print(activeSubscene)
#     start$listeners <<- par3d("listeners", dev = dev, subscene = activeSubscene) #assign "listeners" to value, but it is actually just a number for the subscene?
#     # print(unlist(start$listeners))
#     print(sub, start$listeners)
#     for (sub in "SpecimenPlot") {
#       init <- par3d(c("userProjection","viewport"), dev = dev, subscene = sub) #get user projections and viewport in the subscrene in listners
#       init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
#       start[[as.character(sub)]] <<- init
#     }
#   }
#
#   update <- function(x, y) {
#     for (sub in "SpecimenPlot") {
#       init <- start[[as.character(sub)]]
#       xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
#       mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
#       par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
#     }
#   }
#
#   list(rglbegin = begin, rglupdate = update)
#
#   rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
#   HTML("document.getElementById(this.attributes.rglSceneId.value).rglinstance.\n                   setMouseMode(this.value, \n                                button = parseInt(this.attributes.rglButton.value), \n                                subscene = parseInt(this.attributes.rglSubscene.value),\n                                stayActive = parseInt(this.attributes.rglStayActive.value))")
#   # changecode <- "document.getElementById(this.attributes.rglSceneId.value).rglinstance.\n                   setMouseMode(this.value, \n                                button = parseInt(this.attributes.rglButton.value), \n                                subscene = parseInt(this.attributes.rglSubscene.value),\n                                stayActive = parseInt(this.attributes.rglStayActive.value))"
#   # result <- tags$select(tagList(options), onchange = HTML(changecode),
#   #                       rglButton = button, rglSubscene = subscene, rglStayActive = as.numeric(stayActive),
#   #                       ...)
#   cat("Callbacks set on button", button, "of RGL device", dev, "in subscene", subscene, "\n")
# }
# open3d()
# shade3d(icosahedron3d(), col = "yellow")
# # This only works in the internal display...
# pan3d(2)
#
#
# "document.getElementById(SpecimenPlot).rglinstance.
# setMouseMode(selecting,
#              button = 2,
#              subscene = SpecimenPlot,
#              stayActive = parseInt(this.attributes.rglStayActive.value))"
