alignR_server <- function(input, output, session) {

  tab_n <- reactiveVal(1)
  stylesheets <- c("fixed.css","mixed.css","auto.css")

  ## initialize UI
  output$header <- renderUI({
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = stylesheets[tab_n()]),
      ))
  })

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
      shinyjs::showElement(id="n")
      shinyjs::hideElement(id="auto_align")
      tab_n(1)
      shinyjs::disable('tab1')
      shinyjs::enable('tab2')
      shinyjs::enable('tab3')
    # }
  })
  observeEvent(input$tab2,{
    updateNumericInput(session,inputId = "n", value = 10)
    shinyjs::showElement(id="n")
    shinyjs::hideElement(id="auto_align")
    tab_n(2)
    shinyjs::enable('tab1')
    shinyjs::disable('tab2')
    shinyjs::enable('tab3')
    })

  observeEvent(input$tab3,{
    updateNumericInput(session,inputId = "n", value = 6)
    shinyjs::hideElement(id="n")
    shinyjs::showElement(id="auto_align")
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
    coords  = NULL,
    orig_view = NULL
  )

  tmp_specimen <- NULL
  sharedData <- NULL ##create empty variable to collect individual landmark selections
  verts <- NULL
  centers <- NULL
  spec_tri <- NULL
  tmp_coords <- NULL
  all_LMs <- NULL
  orig_view <- NULL

  observeEvent(input$clear, {
    clearLMs(LM_values())
    tmp_values$coords <- NULL
    # output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {printLMs()})
    updateSelectInput(session,"cur_specimen",selected = names(sp_list)[isolate(cur_sp())])

    # shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    # tmp_par <- alignRPar3d(input$par3d,zoom=1)
    # tmp_zoom <- input$par3d$zoom
    # output$testing <- renderPrint({
    #   list(tmp_par[1],tmp_zoom)
    #   })
  })

  # output$testing <- renderText({
  #   start_int
  #   # is.matrix(tmp_values$coords)
  # #   isolate(LM_values())
  # #   # return(input$SetupComplete)
  # # #   # validate(need(MeshData(),"MeshData() not found"))
  # # #   # validate(need(isolate(output$SpecimenPlot),"MeshData() not found"))
  # # #   return(cat(rv$setupComplete,!is.null(isolate(MeshData())),sep = "\n"))
  # # #
  #   })



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
      updateNumericInput(session, "n",value=6)
      return(c("Top", "Bottom","Front", "Back","Right", "Left"))
      # return(list("Top" = 1, "Bottom" = 2,"Front" = 3, "Back" = 4,"Right" = 5, "Left" = 6))
    } else {
      tmp_n <- as.numeric(input$n)
      clearLMs(c(paste("LM ", 1:tmp_n, sep = "")))
      return(c(paste("LM ", 1:tmp_n, sep = "")))
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

  current_lm <- reactive(as.numeric(match(input$Lm_n,LM_values())))



  ## create dropdown menu to choose the landmark to digitize
  output$auto_align <- renderUI({
    tagList(
      fluidRow(
        em(h6("Perform once all specimens have alignment landmarks")),
        align = "center",
      ),
      # actionButton("auto_align_btn","Align Surface Landmarks!", icon = icon("cube")),
      # actionButton("auto_align_btn","Align Surface Landmarks!", icon = icon("magic")),
      # actionButton("auto_align_btn","Align Surface Landmarks!", icon = icon("cubes")),
      actionButton("auto_align_btn","Align Surface Landmarks!", icon = icon("object-ungroup"))
    )
  })
  ##

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
          )),
      div(style="display:inline-block", actionButton("Last_Sp", "Previous specimen", icon = icon("circle-left"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray; outline-color: SlateGray")),
      div(style="display:inline-block", actionButton("Next_Sp", "Next specimen", icon = icon("circle-right"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray; outline-color: SlateGray")),
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
  #   # output$value <- reactive({
  #     verbatimTextOutput(
  #       cat("The value of rv$setupComplete is:", isolate(rv$setupComplete),
  #           "The value of !is.null(isolate(MeshData())) is:", !is.null(isolate(MeshData())),
  #           "The value of output$setupComplete is:", isolate(output$setupComplete),
  #           sep = "\n")
  #     )
    # })
    # print(is.numeric(tmp_values$coords))
  #   cur_sp_name <- names(sp_list)[[cur_sp()]]
    # h4(paste(class(tmp_values$coords)), style = "padding-left:20px; color:#fff")
    # h4(paste(cur_sp_name,cur_sp(),sep = " = sp. #"), style = "padding-left:20px; color:#fff")
  # })

  output$cur_specimen <- renderUI({
    # tags$style("
    #     #cur_specimen ~ .selectize-input.full {
    #         background-color: SlateGray;
    #         border-color: SlateGray;
    #         outline-color: SlateGray;
    #         color: #fff;
    #         font-size: 20px
    #     }
    #     ")
    selectInput("cur_specimen", NULL, names(sp_list), width = "600px")
  })

  cur_sp <- reactive({
    if (is.null(input$cur_specimen)){
      return(1)
    }else{
      grep(input$cur_specimen,names(sp_list))
    }
  })

  observeEvent(input$cur_specimen, {
    if(exists(names(lm_array)[[cur_sp()]],where=lm_array,mode="numeric")){
      tmp_values$coords <- loadLMs(lm_array, cur_sp(), LM_values())
    } else{
      clearLMs(LM_values())
      tmp_values$coords <- NULL
    }

    #I don't think this bit is set up properly to work yet, but want to use it to set up viewpoint when switching specimens...might need to go with the rglwidget section...
    # if(!is.null(orig_view)){
    #   shinySetPar3d(userMatrix = orig_view)
    # }

  })
##

  #need a way to reset rgl viewpoint back to default...save initial value (non-reactive?)...but then how to reset it
  # rgl.viewpoint(tmp_values$orig_view)
  # par3d(userMatrix = tmp_values$orig_view)

  open3d(useNULL = TRUE)

  observe({
  ## Create base Specimen Plot in rglWidget object
  output$SpecimenPlot <- renderRglwidget({

    spec <-  sp_list[[cur_sp()]]
    tmp_specimen <- MeshManager(spec)

    clear3d()
    rgl.bg(color = "SlateGray")

    ids <- plot3d(tmp_specimen$specimen[, 1], tmp_specimen$specimen[, 2], tmp_specimen$specimen[, 3],
                  size = tmp_specimen$ptsize, aspect = FALSE, box = FALSE, axes = FALSE,
                  xlab = "",  ylab = "",  zlab = "")

    sharedData <- rglShared(ids["data"])

    shade3d(tmp_specimen$mesh, override = TRUE, meshColor = "vertices", add = TRUE)

    temp_ids = ids3d("shapes")
    mesh_id = which(temp_ids[,2]=="triangles")

    verts <<- rgl.attrib(temp_ids[mesh_id,1], "vertices")
    centers <<- rgl.attrib(temp_ids[mesh_id,1], "centers")
    spec_tri <<- t(tmp_specimen$mesh$vb)[tmp_specimen$mesh$it,1:3]

    # if (!is.null(tmp_values$coords)){
    #     if (is.matrix(tmp_values$coords)){
    #       rgl.spheres(tmp_values$coords[,1], tmp_values$coords[,2], tmp_values$coords[,3],
    #                   radius = point_sizes[cur_sp()], color = c("Red"), add = TRUE) # SteelBlue
    #     } else{
    #       rgl.spheres(tmp_values$coords[1], tmp_values$coords[2], tmp_values$coords[3],
    #                   radius = point_sizes[cur_sp()], color = c("Green"), add = TRUE) # #f1e180
    #     }}

    if (!is.null(tmp_values$coords)){
      if (is.matrix(tmp_values$coords)){
        rgl.spheres(tmp_values$coords[,1], tmp_values$coords[,2], tmp_values$coords[,3],
                    radius = 2, color = c("Red"), add = TRUE) # SteelBlue
      } else{
        rgl.spheres(tmp_values$coords[1], tmp_values$coords[2], tmp_values$coords[3],
                    radius = 2, color = c("Green"), add = TRUE) # #f1e180
      }}

    shinyPan3d <- local({
      # dev = cur3d()
      # subscene = currentSubscene3d(dev)
      start <- list()
      begin <- function(x, y) {
      # activeSubscene <- par3d("activeSubscene", dev = dev) #get activeSubscene
      # start$listeners <<- par3d("listeners", dev = dev, subscene = activeSubscene) #assign "listeners" to value, but it is actually just a number for the subscene?
      # for (sub in start$listeners) {
      #   init <- par3d(c("userProjection","viewport"), dev = dev, subscene = sub) #get user projections and viewport in the subscrene in listners
      #   init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
      #   start[[as.character(sub)]] <<- init
      # }
          shinyGetPar3d(c("scale","listeners","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
          tmp_par <- alignRPar3d(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
          start_int <<- alignRListen(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
          print(start_int)
        }

      update <- function(x, y) {
        for (sub in start$listeners) {
          init <- start[[as.character(sub)]]
          xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
          mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
          par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
        }
        # init <- start_int
        # xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
        # mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
        # shinySetPar3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = "SpecimenPlot" )
      }
      list(rglbegin = begin, rglupdate = update)
    })

    rglbegin <- shinyPan3d$rglbegin
    rglupdate <- shinyPan3d$rglupdate

    # Install both
    setUserCallbacks("right",
                     begin = rglbegin,
                     update = rglupdate)

    # setUserCallbacks("right",
    #                  begin = begin <- function(x, y) {
    #                    shinyGetPar3d(c("scale","listeners","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    #                    tmp_par <- alignRPar3d(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
    #                    start_int <<- alignRListen(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
    #                  },
    #                  update =   update <- function(x, y) {
    #                    init <- start_int
    #                    xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
    #                    mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
    #                    shinySetPar3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = "SpecimenPlot" )
    #                  }
    # )


        rglwidget(scene3d(minimal = FALSE),
                  shared = rglShared(ids["data"]),
                  shinyBrush = "rgl_3D_brush")


    })
  updateRadioButtons(session, "SetupComplete", selected = 'yes')
  # pan3d(2)
  })


  ##landmark table output
  output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {tmp_values$coords})

  ##Text to try putting landmarks as an "add to scene"
  # currentSubscene3d(dev = cur3d())
  # useSubscene3d(subscene)
  # addToSubscene3d(ids, subscene = currentSubscene3d())
  # delFromSubscene3d(ids, subscene = currentSubscene3d())

  observeEvent(input$getPar, {
    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    tmp_par <- alignRPar3d(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))

    # output$testing <- renderText({
    #   cat(tmp_par$model)
    #   paste("any", any(is.nan(tmp_par$model)), "all", all(tmp_par$model[,1]==0), "both", any(is.nan(tmp_par$model)) || all(tmp_par$model[,1]==0), sep = "\n")
    # })

    # updateSelectInput("mouseMode")
    # rgl.setMouseCallbacks()

    if (!isolate(input$NoWarnings)){
      shinyFeedback::showToast(type = "warning",
                message = "Now that the position is set, change mouse mode to 'landmarking' and select point to be landmarked in the rgl window. Click the button AFTER identifying the point in the rgl window.",
                title = "Where is the landmark to be placed?",
                keepVisible = TRUE,
                .options = list(positionClass = "toast-top-center", closeButton = TRUE, progressBar = FALSE)
      )
    }

  })


  observeEvent(input$submitLM, {
    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
    tmp_par <- alignRPar3d(isolate(input$par3d),zoom=isolate(input$par3d$zoom))

    if (any(is.nan(tmp_par$model)) || all(tmp_par$model[,1]==0)){
      shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
      alignRPar3d(input$par3d,zoom=input$par3d$zoom)

      click(id = "submitLM")
    } else {

    tmp_tris <- shinySelectPoints3d(centers, verts, spec_tri, N=20, tmp_par, isolate(input$rgl_3D_brush))
    tmp_lm <- checkLMs(input$Lm_n, tmp_tris$coords)
    # tmp_lm <- array(isolate(tmp_tris$coords), dim = c(1,3), dimnames = list(LM_values()[[as.numeric(input$Lm_n)]], c("X","Y","Z")))
    tmp_values$coords <<- tmp_lm


    if (!isolate(input$NoWarnings)){
      shinyFeedback::showToast(type = "warning",
                message = "Click confirm or select again.",
                title = "Is this landmark correctly placed?",
                keepVisible = TRUE,
                .options = list(positionClass = "toast-top-center", closeButton = TRUE, progressBar = FALSE)
                )
    }

    shinyjs::showElement(id = "confirmLM")
    shinyjs::hideElement(id = "submitLM")
    shinyjs::showElement(id = "getPar")

    }

    })


## if confirmed, save the specimen coords to landmarks and save rglwindow coords to vert list (invisible)
  observeEvent(input$confirmLM, {
    tmp_LMs <- tmp_values$coords

    tmp_data <- saveLMs(lm_array, cur_sp(), current_lm(), LM_values(), tmp_LMs)

    lm_array[[cur_sp()]]  <<- tmp_data
    tmp_values$coords <- tmp_data

    if (current_lm() < input$n){
      next_lm <- current_lm() + 1
      updateSelectInput(session, "Lm_n", selected = LM_values()[next_lm])
    }

    shinyjs::hideElement(id = "submitLM")
    shinyjs::hideElement(id = "confirmLM")
    shinyjs::showElement(id = "getPar")

  })

  observeEvent(input$Next_Sp,{
    #check for whether there are unsaved landmarks (compare landmarks and lm_array[[cur_sp()]])
    #if yes, open warning asking for user input
    #if save, then save
    #if do not save, then clear and load in next specimen landmarks

      next_spec <- next.sp(cur_sp(),sp_n)
      if (exists(names(lm_array)[next_spec],where=lm_array,mode="numeric")){
        clearLMs(LM_values())
        tmp_values$coords <- loadLMs(lm_array, next_spec, LM_values())
        } else{
          clearLMs(LM_values())
        }
      updateSelectInput(session,"cur_specimen",selected = names(sp_list)[next_spec])
  })

  observeEvent(input$Last_Sp,{

    next_spec <- prev.sp(cur_sp(),sp_n)
    if (exists(names(lm_array)[next_spec],where=lm_array,mode="numeric")){
      clearLMs(LM_values())
      tmp_values$coords <- loadLMs(lm_array,next_spec, LM_values())
      }else{
        clearLMs(LM_values())
      }
    updateSelectInput(session,"cur_specimen",selected = names(sp_list)[next_spec])
  })

  observeEvent(input$Next_LM,{
   if (current_lm() < input$n){
      next_lm <- current_lm() + 1
      updateSelectInput(session, "Lm_n", selected = next_lm)
    }
    # click("goLM")
  })

  observeEvent(input$Last_LM,{
     if (current_lm() > 1){
      next_lm <- current_lm() - 1
      updateSelectInput(session, "Lm_n", selected = next_lm)
    }
    # click("goLM")
  })

  observeEvent(input$load, {

    if (any(!is.na(lm_array)) || !is.null(tmp_values$coords)){
      shinyalert(
        html = TRUE,
        text = tagList(
          h3("Loading data will overwrite existing landmarks."),
          h4("Do you still want to proceed?"),
        ),

        # .sweet-alert button:active {box-shadow:inset 0 3px 5px rgba(0,0,0,.125) !important;}
        #
        #
        #
        # .sweet-alert button {color: #fff; background-color: Red !important; border-color: SlateGray; outline-color: SlateGray;}
        #     .sweet-alert button:hover {color: #fff; background-color: Red;  border-color: SlateGray; outline-color: SlateGray!important;}
        #         .sweet-alert button:focus {color: #fff; background-color: Red; border-color: SlateGray; outline-color: SlateGray;box-shadow:none !important;}
        #             .sweet-alert button:active {background-color: Red !important; border-color: SlateGray; outline-color: SlateGray; box-shadow:inset 0 3px 5px rgba(0,0,0,.125) !important;}
        #
        #           .sweet-alert .btn:active {box-shadow:inset 0 3px 5px rgba(0,0,0,.125)
            size = "xs",
            closeOnEsc = FALSE,
            closeOnClickOutside = FALSE,
            type = "warning",
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "Yes!",
            # confirmButtonCol = "#F8BB86",
            cancelButtonText = "No!",
            timer = 0,
            animation = TRUE,
            callbackR = function(x) {
              # print(x)
              if(x){
                file_load <- file.choose()
                lm_array  <<- suppressWarnings(readLandmarks(file_load))
                tmp_values$coords <- loadLMs(lm_array, cur_sp(), LM_values())

              }
        })
    }else{
      file_load <- file.choose()
      lm_array  <<- suppressWarnings(readLandmarks(file_load))
      tmp_values$coords <- loadLMs(lm_array, cur_sp(), LM_values())
    }
  })

  observeEvent(input$save, {
    lm_array[[cur_sp()]] <- tmp_values$coords

  #   output$spec_name <- renderUI({
  #     cat(class(lm_array) == "list",
  #           names(lm_array),
  #               is.null(lm_array[[6]]),
  #               class(lm_array[[6]]) == "logical",
  #               class(lm_array[[6]]) == c('matrix',"array"),
  #               any(unlist(lapply(lm_array,class)) == "logical") | !all(unlist(sapply(lm_array,class)) == c("matrix","array")),
  #               sep = "\n"
  #   )
  # })


    # assign('lm_list', lm_array, envir = .GlobalEnv)
    # list2XML4R(list=list("shapes"=lm_array), file="Landmarks.txt") ##Add something to pull which kind of landmarks are being collected??
    # save(lm_array,file="lm_list.rda")
    writeLandmarks(lm_array,file_name)
    })

  observeEvent(input$quit, {
    lm_array[[cur_sp()]] <- tmp_values$coords
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



# reactive({showModal(startupModal())})

# rv <- reactiveValues()
# rv$setupComplete <- FALSE
#
# setupComplete <- reactiveVal(FALSE)

# startupModal <- function() {
#       modalDialog(
#         {tagList(
#           h2("Loading dataset..."),
#           tags$link(rel = "stylesheet", type = "text/css", href = c("www/startup.css"))
#         )},
#                   title = NULL,
#                   footer = NULL,
#                   size = "l",
#                   easyClose = TRUE,
#                   fade = FALSE)
#   }

# observeEvent(input$SetupComplete, {
#   # if(input$SetupComplete == 'no'){
#   #   showModal(startupModal())
#   # } else {
#     # removeModal()
#   # }
#   showModal(startupModal())
#   if(input$SetupComplete == 'yes'){
#     removeModal()
#   }
#     # else {
#   #   showModal(startupModal())
#   # }
# # } else {if(input$SetupComplete == 'yes'){removeModal()}
# # }
# })

# observe(
#   if(input$SetupComplete == 'no'){startupModal()}
#   else {if(input$SetupComplete == 'yes'){removeModal()}}
# )

# observeEvent(input$SetupComplete,{
#   rv$setupComplete <- TRUE
#   setupComplete(TRUE)
#   return(cat(input$SetupComplete,isolate(setupComplete())))
#   rv <<- reactiveValues(setupComplete = TRUE)
#
#   removeModal()
# })

# observeEvent(input$SetupComplete, {

# output$hide_panel <- eventReactive(input$SetupComplete, TRUE, ignoreInit = TRUE)

# outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)

# observeEvent(input$SetupComplete,{
#   shinyjs::show("cp1")
# })

# observeEvent(input$SetupComplete,{
#   shinyjs::hide("cp1")
#   removeModal()
# })

# output$setupComplete <- reactive({
#   cat(input$SetupComplete)
#   return(input$SetupComplete)
#   outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
# })

# if(isolate(setupComplete())==TRUE){
#   removeModal()
# }      # else{
# showModal(startupModal())
# }



# })

# output$setupComplete <- reactive({
#   if(isolate(rv$setupComplete)==TRUE){
#     removeModal()
#   }else{
#     showModal(startupModal())
#   }
#   return(rv$setupComplete)
#   })

# setupComplete <- reactive({
#   if(isolate(rv$setupComplete)==TRUE){
#     removeModal()
#     }
# })
