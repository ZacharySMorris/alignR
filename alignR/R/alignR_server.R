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
    updateNumericInput(session,inputId = "n", value = 10)
    showElement(id="n")
    tab_n(2)
    shinyjs::enable('tab1')
    shinyjs::disable('tab2')
    shinyjs::enable('tab3')
    })

  observeEvent(input$tab3,{
    updateNumericInput(session,inputId = "n", value = 6)
    hideElement(id="n")
    tab_n(3)
    shinyjs::enable('tab1')
    shinyjs::enable('tab2')
    shinyjs::disable('tab3')
    })
  ##

  tmp_values <- reactiveValues(
    mesh = NULL,
    sharedData = NULL,
    verts  = NULL,
    centers = NULL,
    tringles = NULL,
    cur_LM  = reactive(as.numeric(input$Lm_n)),
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
    tmp_values$coords <- NULL
    output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {printLMs()})
    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    tmp_par <- alignRPar3d(input$par3d,1)
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
  file_name <<- get('file_name', envir = .GlobalEnv)
  sp_list <<- get('sp_list', envir = .GlobalEnv)
  lm_array <<- get('lm_list', envir = .GlobalEnv)
  sp_n <<- length(sp_list)
  point_sizes <<- get('point_sizes', envir = .GlobalEnv)

  # output$spec_name <- renderUI({
  #   print(is.numeric(tmp_values$coords))
  # #   cur_sp_name <- names(sp_list)[[cur_sp()]]
  #   # h4(paste(class(tmp_values$coords)), style = "padding-left:20px; color:#fff")
  #   # h4(paste(cur_sp_name,cur_sp(),sep = " = sp. #"), style = "padding-left:20px; color:#fff")
  # })

  # output$cur_specimen <- renderUI({
  #   # tags$style("
  #   #     #cur_specimen ~ .selectize-input.full {
  #   #         background-color: SlateGray;
  #   #         border-color: SlateGray;
  #   #         outline-color: SlateGray;
  #   #         color: #fff;
  #   #         font-size: 20px
  #   #     }
  #   #     ")
  #   selectInput("cur_specimen", NULL, names(sp_list), width = "1000px")
  # })

  cur_sp <- reactive(grep(input$cur_specimen,names(sp_list)))

  observeEvent(input$cur_specimen, {
    loadLMs(lm_array, cur_sp(), as.numeric(input$n))
    tmp_values$coords <- landmarks
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

    shade3d(tmp_specimen$mesh, override = TRUE, meshColor = "vertices", add = TRUE)
    # light3d()

    temp_ids = ids3d("shapes")
    mesh_id = which(temp_ids[,2]=="triangles")

    verts <<- rgl.attrib(temp_ids[mesh_id,1], "vertices")
    centers <<- rgl.attrib(temp_ids[mesh_id,1], "centers")
    spec_tri <<- t(tmp_specimen$mesh$vb)[tmp_specimen$mesh$it,1:3]

    scene1 <- scene3d(minimal = FALSE)

    return(scene1)
  })

  # rglwidget(scene1,
  #           shared = sharedData,
  #           shinyBrush = "rgl_3D_brush")

  # ,lit=FALSE #can be used to turn of lighting on specific parts (maybe would make landmarks pop more?)

  # output$testing <- renderPrint({
    # print(material3d("color", "alpha","lit","shininess"))
  # })

  output$SpecimenPlot <- renderRglwidget({
    # clear3d(type = "light") #gives us some weird issues with texture/lighting, but better than ever increasing brightness
    clear3d()
    rgl.bg(color = "SlateGray")

    # if (!exists(names(lm_array)[cur_sp()],where=lm_array,mode="numeric")){
    if (is.null(tmp_values$coords)){
      rglwidget(MeshData(),
                shared = sharedData,
                shinyBrush = "rgl_3D_brush")
    } else{
        plot3d(MeshData(),add = TRUE)

        if (is.matrix(tmp_values$coords)){
          rgl.spheres(tmp_values$coords[,1], tmp_values$coords[,2], tmp_values$coords[,3],
                      radius = point_sizes[cur_sp()], color = c("Red"), add = TRUE) # SteelBlue
        } else{
          rgl.spheres(tmp_values$coords[1], tmp_values$coords[2], tmp_values$coords[3],
                      radius = point_sizes[cur_sp()], color = c("Green"), add = TRUE) # #f1e180
        }
        rglwidget(scene3d(minimal = FALSE),
                  shared = sharedData,
                  shinyBrush = "rgl_3D_brush")
        }
    })

  observeEvent(input$getPar, {
    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    tmp_par <- alignRPar3d(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
    # tmp_zoom <- input$par3d$zoom
    # output$testing <- renderPrint({
    #   list(tmp_par[1],tmp_par$zoom)
    #   })

    # updateSelectInput("mouseMode")
    # rgl.setMouseCallbacks()

    if (!isolate(input$NoWarnings)){
      showToast(type = "warning",
                message = "Now that the position is set, change mouse mode to 'landmarking' and select point to be landmarked in the rgl window. Click the button AFTER identifying the point in the rgl window.",
                title = "Where is the landmark to be placed?",
                keepVisible = TRUE,
                .options = list(positionClass = "toast-top-center", closeButton = TRUE, progressBar = FALSE)
      )
    }

  })


  observeEvent(input$submitLM, {
    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    tmp_par <- alignRPar3d(input$par3d,zoom=input$par3d$zoom)
    # tmp_zoom <- input$par3d$zoom
    # tmp_par$proj <- tmp_par$proj*tmp_zoom


    if (is.nan(tmp_par$model) || all(tmp_par$model[,1]==0)){
      shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
      alignRPar3d(input$par3d,zoom=input$par3d$zoom)

      click(id = "submitLM")
    } else {

    tmp_tris <- shinySelectPoints3d(centers, verts, spec_tri, N=20, tmp_par, input$rgl_3D_brush)
    tmp_values$coords <<- isolate(tmp_tris$coords)

    # output$testing <- renderPrint({
    #   list(tmp_par,tmp_tris)
    # })

    if (!isolate(input$NoWarnings)){
      showToast(type = "warning",
                message = "Click confirm or select again.",
                title = "Is this landmark correctly placed?",
                keepVisible = TRUE,
                .options = list(positionClass = "toast-top-center", closeButton = TRUE, progressBar = FALSE)
                )
    }

    showElement(id = "confirmLM")
    hideElement(id = "submitLM")
    showElement(id = "getPar")

    }

    })


## if confirmed, save the specimen coords to landmarks and save rglwindow coords to vert list (invisible)
  observeEvent(input$confirmLM, {
    current_lm <- as.numeric(input$Lm_n)
    tmp_LMs <- tmp_values$coords

    saveLMs(input$n, current_lm, tmp_LMs)
    output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {printLMs()})

    lm_array[[cur_sp()]] <<- landmarks
    tmp_values$coords <- landmarks

    if (current_lm < input$n){
      next_lm <- current_lm + 1
      updateSelectInput(session, "Lm_n", selected = next_lm)
    }

    hideElement(id = "submitLM")
    hideElement(id = "confirmLM")
    showElement(id = "getPar")

  })

  observeEvent(input$Next_Sp,{
    #check for whether there are unsaved landmarks (compare landmarks and lm_array[[cur_sp()]])
    #if yes, open warning asking for user input
    #if save, then save
    #if do not save, then clear and load in next specimen landmarks


    # if (!is.null(landmarks)) {
    #   lm_array[[cur_sp()]] <<- landmarks
    # }

      next_spec <- next.sp(cur_sp(),sp_n)
      if (exists(names(lm_array)[next_spec],where=lm_array,mode="numeric")){
        clearLMs(input$n)
        loadLMs(lm_array,next_spec, as.numeric(input$n))
        tmp_values$coords <- landmarks
        } else{
          clearLMs(input$n)
        }
      output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {printLMs()})
      updateSelectInput(session,"cur_specimen",selected = names(sp_list)[next_spec])
  })

  observeEvent(input$Last_Sp,{
    # if (!is.null(landmarks)) {
    #   lm_array[[cur_sp()]] <<- landmarks

    next_spec <- prev.sp(cur_sp(),sp_n)
    if (exists(names(lm_array)[next_spec],where=lm_array,mode="numeric")){
      clearLMs(input$n)
      loadLMs(lm_array,next_spec, as.numeric(input$n))
      tmp_values$coords <- landmarks
      }else{
        clearLMs(input$n)
      }
    output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {printLMs()})
    updateSelectInput(session,"cur_specimen",selected = names(sp_list)[next_spec])
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
    file_load <<- file.choose()
    lm_array <<- readLandmarks(file_load)

    loadLMs(lm_array, cur_sp(), as.numeric(input$n))

    output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
      printLMs()
    })

    tmp_values$coords <- landmarks
  })

  observeEvent(input$save, {
    lm_array[[cur_sp()]] <<- landmarks
    # assign('lm_list', lm_array, envir = .GlobalEnv)
    # list2XML4R(list=list("shapes"=lm_array), file="Landmarks.txt") ##Add something to pull which kind of landmarks are being collected??
    # save(lm_array,file="lm_list.rda")
    writeLandmarks(lm_array,file_name)
    })

  observeEvent(input$quit, {
    lm_array[[cur_sp()]] <<- landmarks
    # assign('lm_list', lm_array, envir = .GlobalEnv)
    # list2XML4R(list=list("shapes"=lm_array), file="Landmarks.txt") ##Add something to pull which kind of landmarks are being collected??
    # save(lm_array,file="lm_list.rda")
    writeLandmarks(lm_array,file_name)
    warnings()
    stopApp()

  })


  # session$onSessionEnded(function(){
  #   cat(warnings())
  #   stopApp()
  # })

}

