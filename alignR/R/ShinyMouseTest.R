### Test of shiny mouse ###
# Use the mouse to select points
# Original version written by Yohann Demont

library(rgl)
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    mainPanel(tabsetPanel(id = "navbar",
                          selected = "3D",
                          tabPanel(title = "2D",
                                   plotOutput("plot_2D", brush = brushOpts(id = "plot_2D_brush",
                                                                           resetOnNew = TRUE,
                                                                           direction = "xy")),
                                   verbatimTextOutput("brush_info_2D")),
                          tabPanel(title = "3D",
                                   uiOutput("plot_3D_mousemode"),
                                   rglwidgetOutput("plot_3D"),
                                   verbatimTextOutput("brush_info_3D"),
                                   verbatimTextOutput("selected"))
    )),
    sidebarPanel(selectInput("plot_x", label = "x feature", choices = colnames(iris)[-5], selected = colnames(iris)[1]),
                 selectInput("plot_y", label = "y feature", choices = colnames(iris)[-5], selected = colnames(iris)[2]),
                 selectInput("plot_z", label = "z feature", choices = colnames(iris)[-5], selected = colnames(iris)[3]),
                 actionButton(inputId = "reset_brush", label = "reset brush"))
  ))

server <- function(input, output, session) {
  # 2D
  output$plot_2D <- renderPlot({
    plot(x = iris[, input$plot_x],
         y = iris[, input$plot_y],
         col = as.integer(iris[, "Species"]))
  })
  output$brush_info_2D <- renderPrint(str(input$plot_2D_brush))

  # 3D
  sharedData <- NULL ##create empty variable to collect individual landmark selections

  output$brush_info_3D <- renderPrint(print(input$rgl_3D_brush, verbose = TRUE)) #use this to print the point selection

  # How to use selectionFunction3d ?
  output$selected <- renderPrint({
    if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL)
    cat("Selections from crosstalk:\n")
    # Need as.logical because selection() might return NULL
    print(which(as.logical(sharedData$selection())))
    cat("Selections using function:\n")

    f <- selectionFunction3d(input$rgl_3D_brush)
    which(f(iris[, c(input$plot_x, input$plot_y, input$plot_z)]))
  })

  output$plot_3D_mousemode <-
    renderUI({
      rglMouse( default = "trackball",
                stayActive = FALSE,
                choices = c("trackball", "selecting"),
                sceneId = "plot_3D")
    })
  open3d(useNULL = TRUE)
  output$plot_3D <- renderRglwidget({
    clear3d()

    dat <- iris[, c(input$plot_x, input$plot_y, input$plot_z, "Species")]
    dat$id <-as.character(seq_len(nrow(iris)))

    plot3d(x = dat[, 1:3], type = "s", size = 1, col = as.integer(iris[, "Species"]), aspect = TRUE)

    sharedData <<- rglShared(id = text3d(dat[, 1:3], text = dat[, "id"], adj = -0.5),
                             group = "SharedData_plot_3D_ids",
                             deselectedFade = 0,
                             selectedIgnoreNone = FALSE)
    shinyResetBrush(session, "rgl_3D_brush")

    rglwidget(shared = sharedData,
              shinyBrush = "rgl_3D_brush") ## need to add shared and shinyBrush calls into the rglwidget

  })
  observeEvent(input$reset_brush, {
    session$resetBrush("plot_2D_brush")
    shinyResetBrush(session, "rgl_3D_brush")
  })
}

shinyApp(ui, server)


#######

ui <- fluidPage(
  sidebarLayout(
    mainPanel(title = "3D",
              uiOutput("plot_3D_mousemode"),
              rglwidgetOutput("plot_3D"),
              actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
              verbatimTextOutput("brush_info_3D"),
              verbatimTextOutput("selected"),
              uiOutput("landmarking")
    ),
    sidebarPanel(selectInput("plot_x", label = "x feature", choices = colnames(iris)[-5], selected = colnames(iris)[1]),
                 selectInput("plot_y", label = "y feature", choices = colnames(iris)[-5], selected = colnames(iris)[2]),
                 selectInput("plot_z", label = "z feature", choices = colnames(iris)[-5], selected = colnames(iris)[3]),
                 actionButton(inputId = "reset_brush", label = "reset brush"))
  ))

server <- function(input, output, session) {
  # 3D
  sharedData <- NULL ##create empty variable to collect individual landmark selections

  # output$brush_info_3D <- renderPrint(print(input$rgl_3D_brush, verbose = TRUE)$region) #use this to print the point selection
  output$brush_info_3D <- renderPrint(input$rgl_3D_brush$region[1:4]) #This gets the "x" and "y" index values from the brush click....but why is it only two dimensions?
  # output$brush_info_3D <- renderPrint(input$rgl_3D_brush[c("subscene","state","region","model","proj","view")]) #This gets the "x" and "y" index values from the brush click....but why is it only two dimensions?
  # output$brush_info_3D <- renderPrint(rgl.user2window(input$rgl_3D_brush))

  # output$brush_info_3D <- renderPrint(print(as.logical(sharedData$selection())))

  # output$selected <- renderPrint({
  #   if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL)
  #   cat("Selections from crosstalk:\n")
  #   # Need as.logical because selection() might return NULL
  #   print(which(as.logical(sharedData$selection())))
  #   cat("Selections using function:\n")
  #
  #   f <- selectionFunction3d(input$rgl_3D_brush)
  #   which(f(iris[, c(input$plot_x, input$plot_y, input$plot_z)]))
  # })


  output$selected <- renderPrint({
    if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL)
    cat("Selections from function:\n")

    f <- rgl.convertLandmark3d(input$rgl_3D_brush)
    which(f(iris[, c(input$plot_x, input$plot_y, input$plot_z)]))
  })


  output$plot_3D_mousemode <-
    renderUI({
      rglMouse( default = "trackball",
                stayActive = FALSE,
                choices = c("trackball", "selecting"),
                sceneId = "plot_3D")
    })

  open3d(useNULL = TRUE)

  PlotTemp <- reactive({
    clear3d()
    dat <- iris[, c(input$plot_x, input$plot_y, input$plot_z, "Species")]
    dat$id <-as.character(seq_len(nrow(iris)))

    ids <- plot3d(x = dat[, 1:3], type = "s", size = 1, col = as.integer(iris[, "Species"]), aspect = TRUE)
    sharedData <<- rglShared(ids["data"]) #works to do the selection, but wont print anything

    # sharedData <<- rglShared(id = text3d(dat[, 1:3], text = dat[, "id"], adj = -0.5), # this is only needed in order to run the text3d function
    #                          group = "SharedData_plot_3D_ids",
    #                          deselectedFade = 0,
    #                          selectedIgnoreNone = FALSE)

    # sharedData <<- rglShared(id = points3d(dat[, 1:3], size = 10, color = "red", add = TRUE),
    #                          group = "SharedData_plot_3D_ids",
    #                          deselectedFade = 0,
    #                          selectedIgnoreNone = FALSE)

    # sharedData <<- rglShared(id = selectpoints3d(value = FALSE),
    #                          group = "SharedData_plot_3D_ids",
    #                          deselectedFade = 0,
    #                          selectedIgnoreNone = FALSE)

    # selectpoints3d()

    scene1 <- scene3d(minimal = FALSE)
    return(scene1)
  })

  # MeshData <- reactive({
  #
  #   spec <-  sp_list[[1]]
  #   # spec <- #surface object
  #   ptsize <- 1
  #   center <- TRUE
  #
  #   spec.name <- deparse(substitute(spec))
  #   mesh <- NULL
  #   if (inherits(spec, "shape3d") == TRUE || inherits(spec, "mesh3d") ==
  #       TRUE) {
  #     if (center == TRUE) {
  #       specimen <- scale(as.matrix(t(spec$vb)[, -4]), scale = FALSE)
  #       spec$vb <- rbind(t(specimen), 1)
  #     }
  #     if (center == FALSE) {
  #       specimen <- as.matrix(t(spec$vb)[, -4])
  #     }
  #     mesh <- spec
  #     if (is.null(mesh$material))
  #       mesh$material$color <- "gray"
  #     if (is.null(mesh$material$color))
  #       mesh$material$color <- "gray"
  #   }
  #   else if (inherits(spec, "matrix") == FALSE) {
  #     stop("File is not a shape3d/mesh3d object or xyz matrix")
  #   }
  #   else if (inherits(spec, "matrix") == TRUE && dim(spec)[2] ==
  #            3) {
  #     if (center == TRUE) {
  #       specimen <- scale(spec, scale = FALSE)
  #     }
  #     if (center == FALSE) {
  #       specimen <- spec
  #     }
  #   }
  #   else {
  #     stop("File is not matrix in form: vertices by xyz")
  #   }
  #
  #   clear3d()
  #   # rgl.open(useNULL =T) #this is something to help with not plotting separate rgl window, but it doesn't work yet...
  #   ids <- plot3d(specimen[, 1], specimen[, 2], specimen[, 3], size = ptsize,
  #                 aspect = FALSE, box = FALSE, axes = FALSE,
  #                 xlab = "",  ylab = "",  zlab = "")
  #
  #   sharedData <<- rglShared(ids["data"])
  #
  #   if (!is.null(mesh)) {
  #     shade3d(mesh, meshColor = "legacy", add = TRUE)
  #   }
  #
  #   temp_scene <- scene3d(minimal = FALSE)
  #
  #   observeEvent(input$goLM, {
  #
  #     LM_ids <- temp_scene[["objects"]][[1]]["id"]
  #
  #     keep <- ans <- NULL
  #     keep <- selectpoints3d(LM_ids, value = FALSE, button = "left")[2]
  #
  #     # keep <- ans <- NULL
  #     # keep <- rgl.landmarking(1, temp_scene, specimen)
  #     #
  #     # points3d(specimen[keep, 1], specimen[keep, 2], specimen[keep,3],
  #     #          size = 10, color = "red", add = TRUE)
  #     #
  #     LM_coords <- c(specimen[keep, 1], specimen[keep, 2], specimen[keep,3])
  #
  #     output$landmarking <- renderPrint(LM_coords)
  #
  #   })
  #
  #   scene1 <- scene3d(minimal = FALSE)
  #
  #   # rgl.close()
  #   return(scene1)
  #
  # })


  output$plot_3D <- renderRglwidget({
    rgl.bg(color = "SlateGray")
    rglwidget(PlotTemp(),
              shared = sharedData,
              shinyBrush = "rgl_3D_brush") ## need to add shared and shinyBrush calls into the rglwidget
  })

  # observeEvent(input$goLM, {
  #       LM_ids <- PlotTemp()[["objects"]][[1]]["id"]
  #
  #       keep <- ans <- NULL
  #       keep <- selectpoints3d(LM_ids, value = FALSE, button = "left")[2]
  #
  #       # keep <- ans <- NULL
  #       # keep <- rgl.landmarking(1, temp_scene, specimen)
  #       #
  #       # points3d(specimen[keep, 1], specimen[keep, 2], specimen[keep,3],
  #       #          size = 10, color = "red", add = TRUE)
  #       #
  #       LM_coords <- c(specimen[keep, 1], specimen[keep, 2], specimen[keep,3])
  #
  #       output$landmarking <- renderPrint(LM_coords)
  #     })
}

shinyApp(ui, server)


