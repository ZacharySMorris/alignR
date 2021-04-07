alignR_server <- function(input, output, session) {

  currentTab <- reactive({
      current_n <- isolate(input$tab_n)
      current_tab <<- paste("SpecimenPlot", current_n, sep = "")
      return(current_tab)
    })

  tmp_specimen <- NULL
  sharedData <- NULL ##create empty variable to collect individual landmark selections
  verts <- NULL
  centers <- NULL
  spec_tri <- NULL
  tmp_coords <- NULL
  all_LMs <- NULL

  observeEvent(input$clear, {
    clearVerts(input$n)
    clearLMs(input$n)
  })

## Update the landmark selection menu based on the total number of landmarks selected in Lm_n
  reactive({
    if (input$tab_n==3){
      updateNumericInput(session,"n", value = 6)
      # output$n <- NULL
    } else {
      updateNumericInput(session,"n", value = 10)
    }
  })
##

## create a reactive object that contains the numbers or names of landmarks to select
## could update to allow for a user to input a list of names for discrete landmarking rather than just numbers
  LM_values <- reactive({
    if (input$tab_n==3){
      return(list("Dorsal Midpoint" = 1, "Ventral Midpoint" = 2,"Anterior Midpoint" = 3, "Posterior Midpoint" = 4,"Right Lateral Midpoint" = 5, "Left Lateral Midpoint" = 6))
    } else {
      return(c(1:input$n))
    }
  })
##

## create dropdown menu to choose the landmark to digitize
  output$Lm_n <- renderUI({
    tagList(
      fluidRow(
        h5("Landmark to digitize"),
        align = "center",
      ),
      selectInput("Lm_n",NULL, LM_values())
      # div(style="display:inline-block;", h5("Landmark to digitize:")),
      # div(style="display:inline-block;vertical-align:bottom;", selectInput("Lm_n",NULL, LM_values()))
    )
  })
##

## create dropdown menu to choose whether mouse is for rotating or landmarking
  output$plot_3D_mousemode <- renderUI({
    tagList(
      div(style="display:inline-block;", h5("Mouse Mode:", style = "color:#fff")),
      div(style="display:inline-block",
          rglMouse("SpecimenPlot3",
                   default = "trackball",
                   choices = c("trackball", "selecting"),
                   labels = c("Rotation", "Landmarking"),
                   stayActive = FALSE,
                   # style = "outline-color:SlateGray;background-color:SlateGray;border-color:SlateGray;color:#fff"
          )),
      div(style="display:inline-block", actionButton("Last_Sp", "Previous specimen", icon = icon("arrow-alt-circle-left"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray")),
      div(style="display:inline-block", actionButton("Next_Sp", "Next specimen", icon = icon("arrow-alt-circle-right"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray")),
    )
  })
## padding-left:20px;

## create object to contain the current specimen number and load in specimen list
  # cur_spec <- get('cur_spec', envir = .GlobalEnv) #not sure if this should be specified outside the shinyApp first and then called or set inside the server
  cur_sp <- reactiveVal(1)
  sp_list <- get('sp_list', envir = .GlobalEnv)
  sp_n <- length(sp_list)

  output$cur_specimen <- renderUI({
    h4(paste("Specimen #", cur_sp(), sep=""), style = "padding-left:20px; color:#fff")
  })
##

  #need to create an input that is the initial specimen value
  # should use a hidden ui or reactiveValue?
  # reactive{
  #   spec_list <- sp_list
  # }

  open3d(useNULL = TRUE)

  ## Create base Specimen Plot in rglWidget object
  MeshData <- reactive({

    spec <-  sp_list[[cur_sp()]]
    tmp_specimen <<- MeshManager(spec)

    clear3d()
    rgl.bg(color = "SlateGray")
    # rgl.open(useNULL =T) #this is something to help with not plotting separate rgl window, but it doesn't work yet...
    ids <- plot3d(tmp_specimen$specimen[, 1], tmp_specimen$specimen[, 2], tmp_specimen$specimen[, 3],
                  size = tmp_specimen$ptsize, aspect = FALSE, box = FALSE, axes = FALSE,
                  xlab = "",  ylab = "",  zlab = "")

    sharedData <<- rglShared(ids["data"])

    shade3d(tmp_specimen$mesh, meshColor = "vertices", add = TRUE)

    temp_ids = ids3d("shapes")
    mesh_id = which(temp_ids[,2]=="triangles")

    verts <<- rgl.attrib(temp_ids[mesh_id,1], "vertices")
    centers <<- rgl.attrib(temp_ids[mesh_id,1], "centers")
    spec_tri <<- t(tmp_specimen$mesh$vb)[tmp_specimen$mesh$it,1:3]

    scene1 <- scene3d(minimal = FALSE)

    return(scene1)
  })

  # observeEvent(input$tab_n, {
  #   if (input$tab_n==1){
  #     output$SpecimenPlot1 <- renderRglwidget({
  #       rgl.bg(color = "SlateGray")
  #       rglwidget(MeshData(),
  #                 shared = sharedData,
  #                 shinyBrush = "rgl_3D_brush")
  #       })
  #     }
  #   if (input$tab_n==2){
  #     output$SpecimenPlot2 <- renderRglwidget({
  #       rgl.bg(color = "SlateGray")
  #       rglwidget(MeshData(),
  #                 shared = sharedData,
  #                 shinyBrush = "rgl_3D_brush")
  #       })
  #     }
  #   if (input$tab_n==3){
      output$SpecimenPlot3 <- renderRglwidget({
        rglwidget(MeshData(),
                  shared = sharedData,
                  shinyBrush = "rgl_3D_brush")
      })
  #   }
  # })

  observeEvent(input$submitLM, {
    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene"), session)

    tmp_par <- rgl.projection()
    tmp_tris <- shinySelectPoints3d(centers, verts, spec_tri, N=20, tmp_par, input$rgl_3D_brush)
    # tmp_click <- shinyClickLine(tmp_par, input$rgl_3D_brush)
    tmp_coords <<- isolate(tmp_tris$coords)

    # output$testing <- renderPrint({
    #   tmp_tris
    # })

    output$SpecimenPlot3 <- renderRglwidget({
      clear3d(type = "all")
      # clear3d()
      rgl.bg(color = "SlateGray")
      plot3d(MeshData(),add = TRUE)
      rgl.viewpoint(userMatrix = input$par3d$userMatrix)

      # wire3d(tmp_specimen$mesh)
      # lines3d(tmp_click$clickline, col = "red")
      spheres3d(tmp_tris$coords, radius = 0.1, color = "green", add = TRUE)
      # triangles3d(tmp_tris$tris, color = "blue", add = TRUE)

      rglwidget(scene3d(minimal = FALSE),
                shared = sharedData,
                shinyBrush = "rgl_3D_brush")
      })

    if (isolate(input$NoWarnings) == FALSE){
      showToast(type = "warning",
                message = "Click confirm or select again.",
                title = "Is this landmark correctly placed?",
                keepVisible = TRUE,
                .options = list(positionClass = "toast-top-center", closeButton = TRUE, progressBar = FALSE)
                )
      }
    showElement(id = "confirmLM")
    })

## if confirmed, save the specimen coords to landmarks and save rglwindow coords to vert list (invisible)
  observeEvent(input$confirmLM, {
    current_lm <- as.numeric(input$Lm_n)

    tmp_LMs <- tmp_coords

    # saveVerts(input$n, current_lm, tmp_verts)
    saveLMs(input$n, current_lm, tmp_LMs[1,])

    output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
      loadLMs()
    })

    #reload rglwidget with rgl.spheres from accepted landmarks

    all_LMs <- loadLMs()

    output$SpecimenPlot3 <- renderRglwidget({
      clear3d(type = "all")
      # clear3d()
      plot3d(MeshData(),add = TRUE)
      rgl.spheres(all_LMs[,1], all_LMs[,2], all_LMs[,3],
                  radius = 0.1, color = c("red"), add = TRUE)

      rglwidget(scene3d(minimal = FALSE),
                shared = sharedData,
                shinyBrush = "rgl_3D_brush")
    })

    if (current_lm < input$n){
      next_lm <- current_lm + 1
      updateSelectInput(session, "Lm_n", selected = next_lm)
    }

    hide("confirmLM")
  })

  observeEvent(input$Next_Sp,{
    if (!is.null(landmarks)) {
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

  observeEvent(input$Next_LM,{
    current_lm <- as.numeric(input$Lm_n)

    if (current_lm < input$n){
      next_lm <- current_lm + 1
      updateSelectInput(session, "Lm_n", selected = next_lm)
    }
    # click("goLM")
  })

  observeEvent(input$Last_LM,{
    current_lm <- as.numeric(input$Lm_n)

    if (current_lm > 1){
      next_lm <- current_lm - 1
      updateSelectInput(session, "Lm_n", selected = next_lm)
    }
    # click("goLM")
  })
}


# selected_LM <- reactive({
#   tmp_coords <- shinySelectPoints3d(verts, input$par3d, input$rgl_3D_brush)
#   return(tmp_coords)
# })
#
# centroid <- reactive({
#   tmp_center <- rgl.user2window(c(4.415091, -48.092817, 23.688965), input$rgl_3D_brush$proj)
#   return(tmp_center)
# })

# observeEvent(input$getPar, {
#   shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene"), session)
#   cur_par <<- input$par3d
#     })

# observeEvent(input$goLM, {
#   HTML(
#     "document.getElementById(this.attributes.rglSceneId.value).rglinstance.
# setMouseMode('selecting',
#        button = parseInt(this.attributes.rglButton.value),
#        subscene = parseInt(this.attributes.rglSubscene.value),
#        stayActive = parseInt(this.attributes.rglStayActive.value))"
#   )
#
#   # session$sendCustomMessage("selectingMouse", 'value = "selecting"')
#
#   # session$sendCustomMessage("selectingMouse",
#   #                           list(subscene = currentSubscene3d(cur3d()),
#   #                                parameter = "mouseMode",
#   #                                value = c("selecting","zoom","fov","pull")))
#
#   # shiny:::toJSON(list(subscene = currentSubscene3d(cur3d()),
#   #                     parameter = "mouseMode",
#   #                     value = c("selecting","zoom","fov","pull")))
#
#   # shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene"), session)
#   # shinyResetBrush(session, "rgl_3D_brush")
#   # tmp_proj <- shinyUserProj(input$par3d)
#   # delay(1000)
# })


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


