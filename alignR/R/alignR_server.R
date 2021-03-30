alignR_server <- function(input, output, session) {

  # hide(list("SpecimenPlot2", "SpecimenPlot3"))

  sharedData <- NULL
  verts <- NULL

  ## Update the landmark selection menu based on the total number of landmarks selected in Lm_n
  reactive({
    if (input$tab_n==3){
      updateNumericInput(session,"n", value = 6)
      # output$n <- NULL
    } else {
      updateNumericInput(session,"n", value = 10)
    }
  })

  LM_values <- reactive({
    if (input$tab_n==3){
      return(list("Dorsal Midpoint" = 1, "Ventral Midpoint" = 2,"Anterior Midpoint" = 3, "Posterior Midpoint" = 4,"Right Lateral Midpoint" = 5, "Left Lateral Midpoint" = 6))
    } else {
      return(c(1:input$n))
    }
  })


  # list(paste("LM", c(1:10), sep = ""))

  output$Lm_n <- renderUI({
    selectInput("Lm_n","Landmark to digitize", LM_values())
    # h2(LM_values())
  })
  ##

  # cur_spec <- get('cur_spec', envir = .GlobalEnv) #not sure if this should be specified outside the shinyApp first and then called or set inside the server
  cur_sp <- reactiveVal(1)
  sp_list <- get('sp_list', envir = .GlobalEnv)
  sp_n <- length(sp_list)

  output$cur_specimen <- renderPrint({
    cat("Specimen #", cur_sp(), sep="")
  })

  #need to create an input that is the initial specimen value
  # should use a hidden ui or reactiveValue?
  # reactive{
  #   spec_list <- sp_list
  # }

  open3d(useNULL = TRUE)

  ## Create base Specimen Plot in rglWidget object
  MeshData <- reactive({

    spec <-  sp_list[[cur_sp()]]
    # spec <- scallopPLY$ply
    # spec <- #surface object
    ptsize <- 1
    center <- TRUE

    spec.name <- deparse(substitute(spec))
    mesh <- NULL
    if (inherits(spec, "shape3d") == TRUE || inherits(spec, "mesh3d") ==
        TRUE) {
      if (center == TRUE) {
        specimen <- scale(as.matrix(t(spec$vb)[, -4]), scale = FALSE)
        spec$vb <- rbind(t(specimen), 1)
      }
      if (center == FALSE) {
        specimen <- as.matrix(t(spec$vb)[, -4])
      }
      mesh <- spec
      if (is.null(mesh$material))
        mesh$material$color <- "gray"
      if (is.null(mesh$material$color))
        mesh$material$color <- "gray"
    }
    else if (inherits(spec, "matrix") == FALSE) {
      stop("File is not a shape3d/mesh3d object or xyz matrix")
    }
    else if (inherits(spec, "matrix") == TRUE && dim(spec)[2] ==
             3) {
      if (center == TRUE) {
        specimen <- scale(spec, scale = FALSE)
      }
      if (center == FALSE) {
        specimen <- spec
      }
    }
    else {
      stop("File is not matrix in form: vertices by xyz")
    }

    clear3d()
    # rgl.open(useNULL =T) #this is something to help with not plotting separate rgl window, but it doesn't work yet...
    ids <- plot3d(specimen[, 1], specimen[, 2], specimen[, 3], size = ptsize,
                  aspect = FALSE, box = FALSE, axes = FALSE,
                  xlab = "",  ylab = "",  zlab = "")

    if (!is.null(mesh)) {
      shade3d(mesh, meshColor = "legacy", add = TRUE)
    }

    rgl.bg(color = "SlateGray")

    temp_scene <- scene3d(minimal = FALSE)

    mesh_obj <- temp_scene[["objects"]][[1]]["id"]
    verts <<- rgl.attrib(mesh_obj, "vertices")

    scene1 <- scene3d(minimal = FALSE)

    # rgl.close()
    return(scene1)

  })

  # observeEvent(input$tab_n, {
  #   if (input$tab_n==1){
  #     output$SpecimenPlot1 <- renderRglwidget({
  #       rgl.bg(color = "SlateGray")
  #       rglwidget(MeshData()) })
  #   }
  #
  #   if (input$tab_n==2){
  #     output$SpecimenPlot2 <- renderRglwidget({
  #       rgl.bg(color = "SlateGray")
  #       rglwidget(MeshData()) })
  #   }
  #
  #   if (input$tab_n==3){
      output$SpecimenPlot3 <- renderRglwidget({
        rgl.bg(color = "SlateGray")
        rglwidget(MeshData(),
                  shared = sharedData,
                  shinyBrush = "rgl_3D_brush") ## need to add shared and shinyBrush calls into the rglwidget
      })
  #   }
  # })

      output$plot_3D_mousemode <-
        renderUI({
          rglMouse( default = "trackball",
                    stayActive = FALSE,
                    button = 1,
                    choices = c("trackball", "selecting"),
                    sceneId = "SpecimenPlot3",
                    style = "background-color:SlateGray;border-color:SlateGray;color:#fff")
        })

      selected_LM <- reactive({
        tmp_coords <- shinySelectPoints3d(verts, input$par3d, input$rgl_3D_brush)
        return(tmp_coords)
      })

      centroid <- reactive({
        tmp_center <- rgl.user2window(c(4.415091, -48.092817, 23.688965), input$rgl_3D_brush$proj)
        return(tmp_center)
      })

      observeEvent(input$getPar, {
        shinyGetPar3d(c("modelMatrix","projMatrix", "viewport", "userMatrix","userProjection"), session)
      })

      observeEvent(input$goLM, {
        click("getPar")
        updateSelectInput(session, "plot_3D_mousemode", selected = "selecting")

        tmp_LMs <- selected_LM()
        tmp_center <- centroid()

        # output$plot_3D <- renderRglwidget({
        #   rgl.spheres(tmp_LMs[,1], tmp_LMs[,2], tmp_LMs[,3],
        #               radius = 0.5, color = c("red","blue","green"), add = TRUE)
        #
        #   rgl.spheres(tmp_center[1], tmp_center[2], tmp_center[3],
        #               radius = 0.5, color = "purple", add = TRUE)
        #
        #   # line_to_centroid <- lines3d(rbind(tmp_LMs,tmp_center), color = "yellow", add = TRUE)
        #   arrow_to_centroid <- arrow3d(p0=c(tmp_LMs),p1=c(tmp_center),color="green",plot=TRUE)
        #
        #   # output$landmarking <- renderPrint({
        #   # print(arrow_to_centroid)
        #   # })
        #
        #   ## works to add selected points, but changes the center based on position of landmarks
        #   ## should be less of an issue when landmarks are correctly on the mesh
        #   ## but is there a way to ensure the center is maintained?
        #
        #   rglwidget(scene3d(minimal = FALSE),
        #             shared = sharedData,
        #             shinyBrush = "rgl_3D_brush")
        # })

      })


  observeEvent(input$Next_Sp,{
    if (!is.null(landmarks)) {

      # shinyalert(
      #   title = "Hello",
      #   text = "This is a modal",
      #   size = "s",
      #   closeOnEsc = TRUE,
      #   closeOnClickOutside = FALSE,
      #   html = FALSE,
      #   type = "success",
      #   showConfirmButton = TRUE,
      #   showCancelButton = TRUE,
      #   confirmButtonText = "OK",
      #   confirmButtonCol = "#AEDEF4",
      #   cancelButtonText = "Cancel",
      #   timer = 0,
      #   imageUrl = "",
      #   animation = TRUE,
      #   callbackR = function(x) {
      #     if(x){
      #       write.lms(landmarks,cur_sp())
      #       clearLMs(input$n)
      #     }
      #   }
      # )

      write.lms(landmarks,cur_sp())
      clearLMs(input$n)
      next_spec <- next.sp(cur_sp(),sp_n)
      cur_sp(next_spec)
    } else{
      next_spec <- next.sp(cur_sp(),sp_n)
      cur_sp(next_spec)
    }
  })

  observeEvent(input$Last_Sp,{
    if (!is.null(landmarks)) {
      write.lms(landmarks,cur_sp())
      clearLMs(input$n)
      next_spec <- prev.sp(cur_sp(),sp_n)
      cur_sp(next_spec)
    } else{
      next_spec <- prev.sp(cur_sp(),sp_n)
      cur_sp(next_spec)
    }
  })

  observeEvent(input$Next_LM_1,{
    current_lm <- as.numeric(input$Lm_n)

    if (current_lm < input$n){
      next_lm <- current_lm + 1
      updateSelectInput(session, "Lm_n", selected = next_lm)
    }
    click("goLM")
  })

  observeEvent(input$Last_LM_1,{
    current_lm <- as.numeric(input$Lm_n)

    if (current_lm > 1){
      next_lm <- current_lm - 1
      updateSelectInput(session, "Lm_n", selected = next_lm)
    }
    click("goLM")
  })
}







# observeEvent(input$goLM, {
#   current_lm <- input$Lm_n
#
#   output$LM_n_title <- renderUI({
#     titlePanel(paste("Select landmark ", current_lm, sep = ""))
#   })
#
#   keep <- ans <- NULL
#   keep <- rgl.landmarking(current_lm, temp_scene, specimen)
#
#   points3d(specimen[keep, 1], specimen[keep, 2], specimen[keep,3],
#            size = 10, color = "red", add = TRUE)
#
#   LM_coords <- c(specimen[keep, 1], specimen[keep, 2], specimen[keep,3])
#
#   saveLMs(input$n, current_lm, LM_coords)
#
#   output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
#     loadLMs()
#   })
#
#   next_lm <- as.numeric(current_lm) + 1
#   updateSelectInput(session, "Lm_n", selected = next_lm)
#
#   if (next_lm > as.numeric(current_lm)){
#     output$LM_n_title <- renderUI({
#       titlePanel(paste("Landmark ", next_lm, " accepted", sep = ""))
#     })
#   }
#
#   # scene1 <- scene3d(minimal = FALSE)
#   # return(scene1)
#
# })


