alignR_server <- function(input, output, session) {

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

  cur_spec <- reactiveVal(1)

  #need to create an input that is the initial specimen value
  # should use a hidden ui or reactiveValue?
  # reactive{
  #   spec_list <- sp_list
  # }

  ## Create base Specimen Plot in rglWidget object
  MeshData <- reactive({

    spec <-  get('sp_list', envir = .GlobalEnv)[[1]]
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

    observeEvent(input$goLM, {
      current_lm <- input$Lm_n

      output$LM_n_title <- renderUI({
        titlePanel(paste("Select landmark ", current_lm, sep = ""))
      })

      keep <- ans <- NULL
      keep <- rgl.landmarking(current_lm, temp_scene, specimen)

      points3d(specimen[keep, 1], specimen[keep, 2], specimen[keep,3],
               size = 10, color = "red", add = TRUE)

      LM_coords <- c(specimen[keep, 1], specimen[keep, 2], specimen[keep,3])

      saveLMs(input$n, current_lm, LM_coords)

      output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
        loadLMs()
      })

      next_lm <- as.numeric(current_lm) + 1
      updateSelectInput(session, "Lm_n", selected = next_lm)

      if (next_lm > as.numeric(current_lm)){
        output$LM_n_title <- renderUI({
          titlePanel(paste("Landmark ", next_lm, " accepted", sep = ""))
        })
      }

      # scene1 <- scene3d(minimal = FALSE)
      # return(scene1)

    })

    scene1 <- scene3d(minimal = FALSE)

    # rgl.close()
    return(scene1)

  })

  observeEvent(input$tab_n, {
    if (input$tab_n==1){
      output$SpecimenPlot1 <- renderRglwidget({
        rgl.bg(color = "SlateGray")
        rglwidget(MeshData()) })
    }

    if (input$tab_n==2){
      output$SpecimenPlot2 <- renderRglwidget({
        rgl.bg(color = "SlateGray")
        rglwidget(MeshData()) })
    }

    if (input$tab_n==3){
      output$SpecimenPlot3 <- renderRglwidget({
        rgl.bg(color = "SlateGray")
        rglwidget(MeshData()) })
    }
  })

  observeEvent(input$Next_Sp,{

    write.csv(landmarks,file="landmarkingTEST.csv") #this writes it to a csv, which will be the ultimate condition
    ##should replace with something that it saves it to a data.frame with a specimen ID
    clearLMs(input$n)
    # clear3d()
    # MeshData()
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

