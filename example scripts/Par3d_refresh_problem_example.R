### Miniumum working example of landmarking problem ###

#function for simple rgl
ptdist <- function(X,Y){
  if(is.vector(Y)){
    Y = t(as.matrix(Y))
  }

  XY = as.matrix(Y - X)
  colnames(XY) <- c("x", "y", "z")
  XYdist = apply(Y[,1:2], 1, function(x,y){dist(rbind(x,y))}, y= X[1:2])

  return(cbind(XY,XYdist))
}

##simple rgl

spec <-dodecahedron3d(col=rainbow(12))

specimen <- scale(as.matrix(t(spec$vb)[, -4]), scale = FALSE)
spec$vb <- rbind(t(specimen), 1)

tri <- t(spec$vb)[spec$it,1:3]

open3d()
plot3d(specimen[, 1], specimen[, 2], specimen[, 3], size = 1,
       aspect = FALSE, box = FALSE, axes = FALSE,
       xlab = "",  ylab = "",  zlab = "")
shade3d(spec, meshColor = "vertices", add = TRUE)

#Grab shape id, vertecies and centers of triangles
ids = ids3d("shapes")
scntri <- rgl.attrib(id = ids[which(ids[,2]=="triangles"),1], attrib = "vertices")
trictrs <- rgl.attrib(id = ids[which(ids[,2]=="triangles"),1], attrib = "centers")

# user input click
rect <- rgl.select(button = "left", dev = cur3d(), subscene = currentSubscene3d())
clkprj <- rgl.projection()

# get window coordinates of triangle centers
wintrictrs <- rgl.user2window(trictrs, projection = clkprj)

# convert window click coords to scene coords
winvect <- cbind(matrix(rect,ncol = 2, byrow = T), c(0,1))
objvect <- rgl.window2user(x=winvect, projection = clkprj)

ntri <- 4
a <- ptdist(X = winvect[1,], Y =  wintrictrs) # calculate window XY and Z distances from
                                              # user click to triangle centres

aprime <- order(a[,4], decreasing = F)[1:ntri] # get closest centers sorted by window XY distance

tri.aprime <- seq(1, nrow(tri),by = 3 )[aprime] # make a dummy triangle index
verts <- as.vector(sapply(tri.aprime, FUN = function(X,Y) X + c(0:2))) # triangle vertex index

win_pts <- trictrs[aprime,] # these are the scene coords of the nearest triangle centers
plttri <- scntri[verts,] # these are the scene coords of the triangle vertices


clear3d()

wire3d(spec)
lines3d(objvect, col = "red")
points3d(trictrs[aprime,], col = "green", size = 5)
triangles3d(plttri, col = "blue")


### simple shiny

##UI
ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    mainPanel(title = "3D",
              setBackgroundColor(color = "SlateGray"),
              uiOutput("plot_3D_mousemode"),
              rglwidgetOutput("plot_3D"),
              verbatimTextOutput("LineData"),
              verbatimTextOutput("TriangleData"),
    ),
    sidebarPanel(style = "background-color:#c55347;border-color:#c55347;color:#fff",
                 fluidRow(
                   actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   actionButton("submitLM", "Submit", icon = icon("sync"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   actionButton("confirmLM", "Yes", icon = icon("check-circle"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   actionButton("clear", "Clear", icon = icon("eraser"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   align = "center",
                 )
    )
  )
)
##

##Sever
server <- function(input, output, session) {

  spec<- NULL
  verts <- NULL
  centers <- NULL
  spec_tri <- NULL

  # open3d(useNULL = TRUE)

  MeshData <- reactive({

    spec <<- sp_list[[1]]

    specimen <- scale(as.matrix(t(spec$vb)[, -4]), scale = FALSE)
    spec$vb <- rbind(t(specimen), 1)

    spec_tri <<- t(spec$vb)[spec$it,1:3]

    open3d(useNULL = TRUE)
    rgl.bg(color = "SlateGray")
    ids <- plot3d(specimen[, 1], specimen[, 2], specimen[, 3], size = 1,
           aspect = FALSE, box = FALSE, axes = FALSE,
           xlab = "",  ylab = "",  zlab = "")
    shade3d(spec, meshColor = "vertices", add = TRUE)

    sharedData <<- rglShared(ids["data"])

    temp_ids = ids3d("shapes")
    mesh_id = which(temp_ids[,2]=="triangles")

    verts <<- rgl.attrib(temp_ids[mesh_id,1], "vertices")
    centers <<- rgl.attrib(temp_ids[mesh_id,1], "centers")

    scene1 <- scene3d(minimal = FALSE)

    return(scene1)
  })

  output$plot_3D <- renderRglwidget({
    rgl.bg(color = "SlateGray")
    rglwidget(MeshData(),
              shared = sharedData,
              shinyBrush = "rgl_3D_brush") ## need to add shared and shinyBrush calls into the rglwidget
  })

  output$plot_3D_mousemode <- renderUI({
    rglMouse("plot_3D",
             default = "trackball",
             choices = c("trackball", "selecting"),
             style = "background-color:SlateGray;border-color:SlateGray;color:#fff")
  })

  observeEvent(input$clear, {
    clearVerts(input$n)
    clearLMs(input$n)
  })

  observeEvent(input$submitLM, {

    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene"), session)

    tmp_par <- rgl.projection()
    tmp_tris <- shinyTriangleDist(centers, verts, spec_tri, N=4, tmp_par, input$rgl_3D_brush)
    tmp_click <- shinyClickLine(tmp_par, input$rgl_3D_brush)

    output$plot_3D <- renderRglwidget({
      clear3d(type = "all")
      plot3d(MeshData(),add = TRUE)

      # wire3d(spec)
      lines3d(tmp_click$clickline, col = "blue")
      spheres3d(tmp_tris$coords, radius = 0.1, color = "purple", add = TRUE)
      triangles3d(tmp_tris$tris, color = "blue", add = TRUE)

      rgl.viewpoint(userMatrix = input$par3d$userMatrix)

      rglwidget(scene3d(minimal = FALSE),
                shared = sharedData,
                shinyBrush = "rgl_3D_brush")
    })

  })

  # output$LineData <- renderPrint({
  #   if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(cat("Waiting for click data"))
  #   cat("Line data from click selection:\n")
  #   print(rgl.projection())
  # })
  #
  # output$TriangleData <- renderPrint({
  #   if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(cat("Waiting for click data"))
  #   cat("Triangle data from click selection:\n")
  #   selected_tris()
  # })

}
##

shinyApp(ui, server)



#plot version from simple rgl
# lines3d(objvect, col = "red")
# points3d(trictrs[aprime,], col = "purple", size = 5)
# triangles3d(plttri, col = "red")

