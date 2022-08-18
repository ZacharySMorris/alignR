alignR_server <- function(input, output, session) {

  tab_n <- reactiveVal(1)
  stylesheets <- c("www/fixed.css","www/mixed.css","www/auto.css")

  # rv <- reactiveValues()
  # rv$setupComplete <- FALSE
  #
  # output$setupComplete <- reactive({
  #   return(rv$setupComplete)
  # })

  ## initialize UI
  output$header <- renderUI({
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = stylesheets[tab_n()]),
      ))

    # rv$setupComplete <- TRUE

  })
  ##
  ## hide landmark number input if automated analysis selected
  # if (tab_n()==3){
  #   hide("n")
  #
  # }
  # ##

  ## initial Lm dropdown
  # output$Lm_n <- renderUI({
  #   tmp_n <- as.numeric(input$n)
  #   tagList(
  #     fluidRow(
  #       h5("Landmark to digitize"),
  #       align = "center",
  #     ),
  #     selectInput("Lm_n", NULL, tmp_n)
  #   )
  #     })
  ##

  ## update UI based on analysis clicked
  ## need to add warnings about changing the UI after starting one kind of analysis and also function to clear data when changing analyses
  observeEvent(input$tab1,{
    current_tab <- isolate(tab_n())

    # pop-up to confirm the change in analysis
    # if (exists("landmarks")){
    #   shinyalert(
    #     title = "",
    #     text = "Changing analyses will delete all current landmark data. \n Do you still want to proceed?",
    #     size = "xs",
    #     closeOnEsc = FALSE,
    #     closeOnClickOutside = FALSE,
    #     html = TRUE,
    #     type = "warning",
    #     showConfirmButton = TRUE,
    #     showCancelButton = TRUE,
    #     confirmButtonText = "Yes!",
    #     confirmButtonCol = "SteelBlue",
    #     cancelButtonText = "No!",
    #     timer = 0,
    #     animation = TRUE,
    #     callbackR = function(x) {
    #       print(x)
    #       if(x){
    #         updateNumericInput(session,inputId = "n", value = 10)
    #         showElement(id="n")
    #         tab_n(1)
    #         shinyjs::disable('tab1')
    #         shinyjs::enable('tab2')
    #         shinyjs::enable('tab3')
    #
    #       } else{
    #         click(paste("tab", current_tab, sep = ""))
    #       }
    #
    # })
    # } else {
      updateNumericInput(session,inputId = "n", value = 10)
      showElement(id="n")
      tab_n(1)
      shinyjs::disable('tab1')
      shinyjs::enable('tab2')
      shinyjs::enable('tab3')
    # }
  })
  observeEvent(input$tab2,{
    # output$header <- renderUI(
    #   tagList(
    #     tags$head(
    #       tags$link(rel = "stylesheet", type = "text/css", href = "www/mixed.css"),
    #     )))

    updateNumericInput(session,inputId = "n", value = 10)
    showElement(id="n")
    tab_n(2)
    shinyjs::enable('tab1')
    shinyjs::disable('tab2')
    shinyjs::enable('tab3')

  })
  observeEvent(input$tab3,{
    # output$header <- renderUI(
    #   tagList(
    #     tags$head(
    #       tags$link(rel = "stylesheet", type = "text/css", href = "www/auto.css"),
    #     )))

    updateNumericInput(session,inputId = "n", value = 6)
    hideElement(id="n")
    tab_n(3)
    shinyjs::enable('tab1')
    shinyjs::enable('tab2')
    shinyjs::disable('tab3')


    # tmp_n <- c(1:as.numeric(input$n))
    #
    # output$Lm_n <- renderUI({
    #   tagList(
    #     fluidRow(
    #       h5("Landmark to digitize"),
    #       align = "center",
    #     ),
    #     selectInput("Lm_n", NULL, tmp_n)
    #     # div(style="display:inline-block;", h5("Landmark to digitize:")),
    #     # div(style="display:inline-block;vertical-align:bottom;", selectInput("Lm_n",NULL, LM_values()))
    #   )
    # })

  })
  ##

  tmp_values <- reactiveValues(
    mesh = NULL,
    sharedData = NULL,
    verts  = NULL,
    centers = NULL,
    tringles = NULL,
    cur_LM  = NULL,
    coords  = NULL
  )

  tmp_specimen <- NULL
  sharedData <- NULL ##create empty variable to collect individual landmark selections
  verts <- NULL
  centers <- NULL
  spec_tri <- NULL
  tmp_coords <- NULL
  all_LMs <- NULL

  observeEvent(input$clear, {
    clearLMs(input$n)
    tmp_coords <- NULL
  })

# Update the landmark selection menu based on the total number of landmarks selected in Lm_n
# observeEvent(input$tab_n, {
#   if (as.numeric(input$tab_n)==3){
#     updateNumericInput(session,inputId = add_tab(input$tab_n,"n"), value = 6)
#     # output$n <- NULL
#   } else {
#     updateNumericInput(session,inputId = add_tab(input$tab_n,"n"), value = 10)
#   }
# })
# #

## create a reactive object that contains the numbers or names of landmarks to select
## could update to allow for a user to input a list of names for discrete landmarking rather than just numbers
  LM_values <- reactive({
    if (tab_n()==3){
      clearLMs(6)
      return(list("Top" = 1, "Bottom" = 2,"Front" = 3, "Back" = 4,"Right" = 5, "Left" = 6))
    } else {
      tmp_n <- as.numeric(input$n)
      clearLMs(input$n)
      return(c(1:tmp_n))
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
      selectInput("Lm_n", NULL, LM_values())
      # div(style="display:inline-block;", h5("Landmark to digitize:")),
      # div(style="display:inline-block;vertical-align:bottom;", selectInput("Lm_n",NULL, LM_values()))
    )
  })
##

  current_lm <- reactive(as.numeric(input$Lm_n))

## create dropdown menu to choose whether mouse is for rotating or landmarking
  output$plot_3D_mousemode <- renderUI({
    tagList(
      div(style="display:inline-block;", h5("Mouse Mode:", style = "color:#fff")),
      div(style="display:inline-block",
          rglMouse("SpecimenPlot",
                   default = "trackball",
                   choices = c("trackball", "selecting"),
                   labels = c("Rotation", "Landmarking"),
                   stayActive = FALSE,
                   # style = "outline-color:SlateGray;background-color:SlateGray;border-color:SlateGray;color:#fff"
          )),
      div(style="display:inline-block", actionButton("Last_Sp", "Previous specimen", icon = icon("arrow-alt-circle-left"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray; outline-color: SlateGray")),
      div(style="display:inline-block", actionButton("Next_Sp", "Next specimen", icon = icon("arrow-alt-circle-right"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray; outline-color: SlateGray")),
    )
  })
## padding-left:20px;

## create object to contain the current specimen number and load in specimen list
  # cur_spec <- get('cur_spec', envir = .GlobalEnv) #not sure if this should be specified outside the shinyApp first and then called or set inside the server
  cur_sp <- reactiveVal(1)
  sp_list <- get('sp_list', envir = .GlobalEnv)
  lm_array <- get('lm_list', envir = .GlobalEnv)
  sp_n <- length(sp_list)

  output$cur_specimen <- renderUI({
    cur_sp_name <- names(sp_list)[[cur_sp()]]
    # h4(cur_sp_name, style = "padding-left:20px; color:#fff")
    h4(paste(cur_sp_name,cur_sp(),sep = " = sp. #"), style = "padding-left:20px; color:#fff")
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

  # output$testing <- renderPrint({
  #   print(current_lm())
  # })

  output$SpecimenPlot <- renderRglwidget({
    clear3d()
    rgl.bg(color = "SlateGray")

    if (!exists(names(lm_array)[cur_sp()],where=lm_array,mode="numeric")){
      rglwidget(MeshData(),
                shared = sharedData,
                shinyBrush = "rgl_3D_brush")
    } else{

      all_LMs <- printLMs()

      output$SpecimenPlot <- renderRglwidget({
        clear3d(type = "all")
        # clear3d()
        plot3d(MeshData(),add = TRUE)
        rgl.spheres(all_LMs[,1], all_LMs[,2], all_LMs[,3],
                    radius = 0.1, color = c("red"), add = TRUE)

        rglwidget(scene3d(minimal = FALSE),
                  shared = sharedData,
                  shinyBrush = "rgl_3D_brush")
      })
    }
    })

  observeEvent(input$getPar, {
    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    tmp_par <- alignRPar3d(input$par3d)
    # output$testing <- renderPrint({
    #   cat(unlist(tmp_par), input$par3d$zoom, sep = "\n")
    #   })
  })


  observeEvent(input$submitLM, {
    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    tmp_par <- alignRPar3d(input$par3d)

    if (is.nan(tmp_par$model) || all(tmp_par$model[,1]==0)){
      shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
      alignRPar3d(input$par3d)

      click(id = "submitLM")
    } else {

    tmp_tris <- shinySelectPoints3d(centers, verts, spec_tri, N=20, tmp_par, input$rgl_3D_brush)
    tmp_coords <<- isolate(tmp_tris$coords)

    # print(tmp_tris$coords)

    output$SpecimenPlot <- renderRglwidget({
      clear3d(type = "all")
      # clear3d()
      rgl.bg(color = "SlateGray")
      plot3d(MeshData(),add = TRUE)
      rgl.viewpoint(zoom = input$par3d$zoom, userMatrix = input$par3d$userMatrix)
      # observer3d(input$par3d$observer)

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

    # saveLMs(input$n, current_lm(), tmp_tris$coords)

    # output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
    #   loadLMs()
    # })

    showElement(id = "confirmLM")
    hideElement(id = "submitLM")
    showElement(id = "getPar")

    }

    })


## if confirmed, save the specimen coords to landmarks and save rglwindow coords to vert list (invisible)
  observeEvent(input$confirmLM, {
    current_lm <- as.numeric(input$Lm_n)

    tmp_LMs <- tmp_coords
    # print(tmp_LMs)

    saveLMs(input$n, current_lm, tmp_LMs)

    output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
      printLMs()
    })

    #reload rglwidget with rgl.spheres from accepted landmarks

    all_LMs <- printLMs()

    output$SpecimenPlot <- renderRglwidget({
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

    hideElement(id = "submitLM")
    hideElement(id = "confirmLM")
    showElement(id = "getPar")

  })

  observeEvent(input$Next_Sp,{
    if (!is.null(landmarks)) {
      lm_array[[cur_sp()]] <<- landmarks
    }

      next_spec <- next.sp(cur_sp(),sp_n)
      if (exists(names(lm_array)[next_spec],where=lm_array,mode="numeric")){

        clearLMs(input$n)
        loadLMs(lm_array,next_spec)

        output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
          printLMs()
        })

      # }
      cur_sp(next_spec)
    } else{
      clearLMs(input$n)
      next_spec <- next.sp(cur_sp(),sp_n)
      cur_sp(next_spec)
      }
  })

  # observeEvent(input$Next_Sp,{
  #   if (!is.null(landmarks)) {
  #     click("save")
  #   }
  #     clearLMs(input$n)
  #     next_spec <- next.sp(cur_sp(),sp_n)
  #     cur_sp(next_spec)
  #
  #     click("load")
  # })

  observeEvent(input$Last_Sp,{
    if (!is.null(landmarks)) {
      lm_array[[cur_sp()]] <<- landmarks

    next_spec <- prev.sp(cur_sp(),sp_n)
    if (exists(names(lm_array)[next_spec],where=lm_array,mode="numeric")){
      clearLMs(input$n)
      loadLMs(lm_array,next_spec)

      output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
        printLMs()
      })

    }
    cur_sp(next_spec)
  }else{
      clearLMs(input$n)
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

  observeEvent(input$load, {
  lm_array <<- get('lm_list', envir = .GlobalEnv)
    loadLMs(lm_array, cur_sp(), as.numeric(input$n))

    output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
      printLMs()
    })

    all_LMs <- printLMs()

    output$SpecimenPlot <- renderRglwidget({
      clear3d(type = "all")
      # clear3d()
      plot3d(MeshData(),add = TRUE)
      rgl.spheres(all_LMs[,1], all_LMs[,2], all_LMs[,3],
                  radius = 0.1, color = c("red"), add = TRUE)

      rglwidget(scene3d(minimal = FALSE),
                shared = sharedData,
                shinyBrush = "rgl_3D_brush")
    })

  })

  observeEvent(input$save, {
    lm_array[[cur_sp()]] <<- landmarks
    assign('lm_list', lm_array, envir = .GlobalEnv)
    list2XML4R(list=list("shapes"=lm_array), file="Landmarks.txt") ##Add something to pull which kind of landmarks are being collected??
  })

  observeEvent(input$quit, {
    lm_array[[cur_sp()]] <<- landmarks
    assign('lm_list', lm_array, envir = .GlobalEnv)
    list2XML4R(list=list("shapes"=lm_array), file="Landmarks.txt") ##Add something to pull which kind of landmarks are being collected??
    warnings()
    stopApp()

  })


  # session$onSessionEnded(function(){
  #   cat(warnings())
  #   stopApp()
  # })

}



# tmp_par <- rgl.projection()
# print("tmp_par:")
# print(tmp_par)

# tmp_par <- rgl.projection()

# output$testing <- renderPrint({
#   cat(centers, verts, spec_tri, tmp_par, input$rgl_3D_brush, sep = "\n")
#   # cat(class(input$n), class(current_lm), class(isolate(tmp_tris$coords)), sep = "\n")
# })


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


