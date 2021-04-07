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

tags$option('value="trackball" >trackball')
tags$option('value="selecting" selected>selecting')
tags$div("class='sweet-overlay' tabindex='-1' style='opacity:0.00;display:active;'")

# <option value="trackball">trackball</option>
# <option value="selecting" selected>selecting</option>
# <div class="sweet-overlay" tabindex="-1" style="opacity:0.00;display:active;"></div>

# modal1 .fade.in {opacity: 0}
#modal1 .modal-backdrop.in {opacity:0.00;display:active}
.modal-open

'<div id="shiny-modal" class="modal fade in" tabindex="-1" data-backdrop="active" data-keyboard="false" style="display: block;"></div>'
"data:image/png;base64,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"

#toast-container .toast.before { position: relative;
# font-family: FontAwesome;
# font-size: 24px;
# line-height: 18px;
# float: left;
# margin-left: -1em;
# color: #FFF;
# padding-right: 0.5em;
# margin-right: 0.5em;
# }

#toast-container .toast-warning.before {content: '\f059';}

# background-image: url(data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgZm9jdXNhYmxlPSJmYWxzZSIgZGF0YS1wcmVmaXg9ImZhciIgZGF0YS1pY29uPSJxdWVzdGlvbi1jaXJjbGUiIGNsYXNzPSJzdmctaW5saW5lLS1mYSBmYS1xdWVzdGlvbi1jaXJjbGUgZmEtdy0xNiIgcm9sZT0iaW1nIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA1MTIgNTEyIj48cGF0aCBmaWxsPSJjdXJyZW50Q29sb3IiIGQ9Ik0yNTYgOEMxMTkuMDQzIDggOCAxMTkuMDgzIDggMjU2YzAgMTM2Ljk5NyAxMTEuMDQzIDI0OCAyNDggMjQ4czI0OC0xMTEuMDAzIDI0OC0yNDhDNTA0IDExOS4wODMgMzkyLjk1NyA4IDI1NiA4em0wIDQ0OGMtMTEwLjUzMiAwLTIwMC04OS40MzEtMjAwLTIwMCAwLTExMC40OTUgODkuNDcyLTIwMCAyMDAtMjAwIDExMC40OTEgMCAyMDAgODkuNDcxIDIwMCAyMDAgMCAxMTAuNTMtODkuNDMxIDIwMC0yMDAgMjAwem0xMDcuMjQ0LTI1NS4yYzAgNjcuMDUyLTcyLjQyMSA2OC4wODQtNzIuNDIxIDkyLjg2M1YzMDBjMCA2LjYyNy01LjM3MyAxMi0xMiAxMmgtNDUuNjQ3Yy02LjYyNyAwLTEyLTUuMzczLTEyLTEydi04LjY1OWMwLTM1Ljc0NSAyNy4xLTUwLjAzNCA0Ny41NzktNjEuNTE2IDE3LjU2MS05Ljg0NSAyOC4zMjQtMTYuNTQxIDI4LjMyNC0yOS41NzkgMC0xNy4yNDYtMjEuOTk5LTI4LjY5My0zOS43ODQtMjguNjkzLTIzLjE4OSAwLTMzLjg5NCAxMC45NzctNDguOTQyIDI5Ljk2OS00LjA1NyA1LjEyLTExLjQ2IDYuMDcxLTE2LjY2NiAyLjEyNGwtMjcuODI0LTIxLjA5OGMtNS4xMDctMy44NzItNi4yNTEtMTEuMDY2LTIuNjQ0LTE2LjM2M0MxODQuODQ2IDEzMS40OTEgMjE0Ljk0IDExMiAyNjEuNzk0IDExMmM0OS4wNzEgMCAxMDEuNDUgMzguMzA0IDEwMS40NSA4OC44ek0yOTggMzY4YzAgMjMuMTU5LTE4Ljg0MSA0Mi00MiA0MnMtNDItMTguODQxLTQyLTQyIDE4Ljg0MS00MiA0Mi00MiA0MiAxOC44NDEgNDIgNDJ6Ij48L3BhdGg+PC9zdmc+) !important
# h4(style = "color:#fff;font-size:45px", icon('question-circle'))

# tags$head(tags$style("#modal1 .modal-content {left:80%;top:25%;width:150;background-color:#c55347;border-color:#c55347;color:#fff;text-align:center}
#                       #modal1 .modal-open {opacity:0.00;display:active}
#                      "),


fa_i("question-circle")
HTML("Next <br/> landmark")
alpha("SlateGray", 0.9)
alpha("SteelBlue", 0.9)
alpha("#648f7b", 0.9)
alpha("#c55347", 0.9)

muted("SlateGray",c=0)
# outline-color

ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  tags$head(tags$style("#toast-container .toast {width:350px;}
                       #toast-container .toast-warning {background-color:#c55347;color:#fff;}
                       #Lm_n .selectize-input.full {background-color:#c55347;border-color:#c55347;outline-color:#b04a3c;color:#fff;}
                       #Lm_n .selectize-control.single .selectize-input:after {border-color:#fff transparent transparent transparent}
                       #Lm_n .selectize-control.single .selectize-input.dropdown-active:after {border-color:transparent transparent #fff transparent}
                       #Lm_n .selectize-dropdown {background-color:#C55347E6;color:#fff;}
                       #Lm_n .selectize-dropdown-content .active {background-color:#b04a3c;color:#fff;}
                       #plot_3D_mousemode select {border:0px;outline:0px;background-color:SlateGray;color:#fff;}
                       #plot_3D_mousemode select option {border:0px;outline:0px;background-color:#708090E6;border-color:#708090E6;outline-color:#708090E6;color:#fff;}
                       #plot_3D_mousemode select option:hover {background-color:#6F7881;color:#fff;}
                       ")
            # tags$script("# Shiny.addCustomMessageHandler('selectingMouse', function(selecting){
            #        setMouseMode(selecting)
            # });
            #             ")

            ),
  sidebarLayout(
    mainPanel(setBackgroundColor(color = "SlateGray"),
              rglwidgetOutput("plot_3D"),
              uiOutput("plot_3D_mousemode")
    ),
    sidebarPanel(style = "background-color:#c55347;border-color:#c55347;color:#fff",
                 hidden(numericInput("n", "Number of fixed landmarks", 6)),
                 uiOutput("Lm_n"),
                 fluidRow(
                   # actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   # actionButton("getPar", "Get Parameters", icon = icon("sliders"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   actionButton("submitLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   hidden(actionButton("confirmLM", "Confirm", icon = icon("check-circle"), style = "color: #fff; background-color: #c55347; border-color: #c55347")),
                   # actionButton("clear", "Clear", icon = icon("eraser"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   align = "center",
                   ),
                 fluidRow(
                   actionButton("Last_LM_1", "Previous landmark", icon = icon("arrow-circle-left"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   actionButton("Next_LM_1", "Next landmark", icon = icon("arrow-circle-right"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                   align = "center",
                   ),
                 fluidRow(
                   uiOutput("landmarks"),
                   align = "center",
                   ),
                 fluidRow(
                   checkboxInput("NoWarnings", "Do not remind me to confirm landmark selections.", value = FALSE, width = NULL),
                   align = "center",
                   ),
                 )
    )
  )

server <- function(input, output, session) {
  # 3D

  sharedData <- NULL ##create empty variable to collect individual landmark selections
  verts <- NULL
  centers <- NULL
  spec_tri <- NULL
  tmp_coords <- NULL
  # all_LMs <- NULL

  observeEvent(input$clear, {
    clearVerts(input$n)
    clearLMs(input$n)
  })

  LM_values <- reactive({
      return(list("Dorsal Midpoint" = 1, "Ventral Midpoint" = 2,"Anterior Midpoint" = 3, "Posterior Midpoint" = 4,"Right Lateral Midpoint" = 5, "Left Lateral Midpoint" = 6))
  })

  output$Lm_n <- renderUI({
    tagList(
      fluidRow(
        h5("Landmark to digitize"),
        align = "center",
        ),
      selectInput("Lm_n",NULL, LM_values())
            )
  })

  output$plot_3D_mousemode <- renderUI({
    tagList(
      div(style="display:inline-block;", h5("Mouse Mode:", style = "padding-left:20px; color:#fff")),
      div(style="display:inline-block",
        rglMouse("plot_3D",
                 default = "trackball",
                 choices = c("trackball", "selecting"),
                 labels = c("rotation", "landmarking"),
                 stayActive = FALSE,
                 # style = "outline-color:SlateGray;background-color:SlateGray;border-color:SlateGray;color:#fff"
                 )
        )
      )
    })

  open3d(useNULL = TRUE)

  MeshData <- reactive({

    spec <-  sp_list[[1]]
    tmp_specimen <<- MeshManager(spec, size = 1)

    clear3d()
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

  output$plot_3D <- renderRglwidget({
    rgl.bg(color = "SlateGray")
    rglwidget(MeshData(),
              shared = sharedData,
              shinyBrush = "rgl_3D_brush") ## need to add shared and shinyBrush calls into the rglwidget
  })

  # observeEvent(input$getPar, {
  #   shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene"), session)
  #     cur_par <<- input$par3d
  # })

  # input$plot_3D_mousemode

  observeEvent(input$goLM, {
    HTML(
      "document.getElementById(this.attributes.rglSceneId.value).rglinstance.
      setMouseMode('selecting',
             button = parseInt(this.attributes.rglButton.value),
             subscene = parseInt(this.attributes.rglSubscene.value),
             stayActive = parseInt(this.attributes.rglStayActive.value))"
    )

    # session$sendCustomMessage("selectingMouse", 'value = "selecting"')

    # session$sendCustomMessage("selectingMouse",
    #                           list(subscene = currentSubscene3d(cur3d()),
    #                                parameter = "mouseMode",
    #                                value = c("selecting","zoom","fov","pull")))

    # shiny:::toJSON(list(subscene = currentSubscene3d(cur3d()),
    #                     parameter = "mouseMode",
    #                     value = c("selecting","zoom","fov","pull")))

    # shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene"), session)
    # shinyResetBrush(session, "rgl_3D_brush")
    # tmp_proj <- shinyUserProj(input$par3d)
    # delay(1000)
  })

  observeEvent(input$submitLM, {

    shinyGetPar3d(c("scale","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene"), session)

    tmp_par <- rgl.projection()
    tmp_tris <- shinyTriangleDist(centers, verts, spec_tri, N=20, tmp_par, input$rgl_3D_brush)
    tmp_click <- shinyClickLine(tmp_par, input$rgl_3D_brush)

    tmp_coords <<- tmp_tris$coords

    output$plot_3D <- renderRglwidget({
      clear3d(type = "all")
      rgl.bg(color = "SlateGray")
      # plot3d(MeshData(),add = TRUE)
      rgl.viewpoint(userMatrix = input$par3d$userMatrix)

      wire3d(tmp_specimen$mesh)
      lines3d(tmp_click$clickline, col = "red")
      spheres3d(tmp_tris$coords, radius = 0.25, color = "green", add = TRUE)
      triangles3d(tmp_tris$tris, color = "blue", add = TRUE)

      rglwidget(scene3d(minimal = FALSE),
                shared = sharedData,
                shinyBrush = "rgl_3D_brush")
    })

    if (isolate(input$NoWarnings) == FALSE){
      showToast(type = "warning",
                message = "Click confirm or select again.",
                title = "Is this landmark correctly placed?",
                keepVisible = TRUE,
                .options = list(positionClass = "toast-top-left", closeButton = TRUE, progressBar = FALSE)
                )
    }
    show("confirmLM")
  })

  #if confirmed, save the specimen coords to landmarks and save rglwindow coords to vert list (invisible)
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

      output$plot_3D <- renderRglwidget({
        clear3d(type = "all")
        plot3d(MeshData(),add = TRUE)
        rgl.spheres(all_LMs[,1], all_LMs[,2], all_LMs[,3],
                    radius = 0.5, color = c("red"), add = TRUE)

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
}

vwr = dialogViewer('alignR', width = 800, height = 600) # Set app window size
runGadget(shinyApp(ui, server), viewer = vwr)  # Run app in presized window

shinyApp(ui, server)

### this is the html that runs on a change in the rglMouse valuse
HTML(
"document.getElementById(this.attributes.rglSceneId.value).rglinstance.
setMouseMode('selecting',
             button = parseInt(this.attributes.rglButton.value),
             subscene = parseInt(this.attributes.rglSubscene.value),
             stayActive = parseInt(this.attributes.rglStayActive.value))"
)
###

#######

output$brush_info_3D <- renderPrint({
  if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL)
  cat("Selections from shinyWindowProj:\n")
  print(input$rgl_3D_brush$region)
})

output$selected <- renderPrint({
  if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL)
  cat("Selections from shinyUserProj:\n")
  shinyUserProj(input$par3d)
  # selected_vertex()
  # selected_coords()
  # selected_tris()
  # rgl.user2window(input$rgl_3D_brush$region[1],y=input$rgl_3D_brush$region[2], input$rgl_3D_brush$proj)
})

# selected_vertex <- reactive({
#   tmp_vert <- shinySelectPoints3d(specimen, verts, input$par3d, input$rgl_3D_brush)
#   return(tmp_vert$vert)
# })
#
# selected_coords <- reactive({
#   tmp_coords <- shinySelectPoints3d(specimen, verts, input$par3d, input$rgl_3D_brush)
#   return(tmp_coords$coords)
# })

centroid <- reactive({
  tmp_center <- rgl.user2window(c(4.415091, -48.092817, 23.688965), input$rgl_3D_brush$proj)
  return(tmp_center)
})

selected_tris <- reactive({
  cur_par <- input$par3d
  tmp_tri_centers <- shinyTriangleDist(centers, verts, spec_tri, N=20, cur_par, input$rgl_3D_brush)
  return(list("coords" = tmp_tri_centers$coords,
              "tris" = tmp_tri_centers$tris))
})

shiny_click <- reactive({
  cur_par <- input$par3d
  tmp_out <- shinyClickLine(cur_par, input$rgl_3D_brush)
  return(tmp_out)

  # return(win_line)
})



#pop-up to confirm the selected point
# showModal(tags$div(id="modal1", modalDialog(
#   tagList(
#     # tags$div("class='sweet-overlay' tabindex='-1' style='opacity:0.00;display:active;'"),
#     tags$style(".btn-default {color: #fff; background-color: #c55347; border-color: #c55347}
#                .btn:hover {color: #fff; background-color: #c55347; border-color: #c55347}"),
#     h4(style = "color:#fff;font-size:45px", icon('question-circle')),
#     h4("Is this landmark \n correctly placed?"),
#     br(),
#     fluidRow(
#       modalButton("No", icon = icon("times-circle")),
#       actionButton("confirmLM", "Yes!", icon = icon("check-circle")),
#       align = "center")
#   ),
#   title = NULL,
#   size = "s",
#   footer = NULL
# )))

# mouse_list <- list()
# mouse_list$mouseMode <- "selecting"
# mouse_list$viewport <- input$par3d$viewport
# #Need to figure out how to use shinySetPar3d, it says you can use this...but I keep getting this error:
# # "Warning: Error in shinySetPar3d: Parameters must all be named"
# shinySetPar3d("mouseMode" = c("selecting"), session)

# output$plot_3D_mousemode <- renderUI({
#   rglMouse("plot_3D", default = "selecting", choices = c("trackball", "selecting"))
#   })

class(rbind(c(1,2,3), c(4.415091, -48.092817, 23.688965)))

LM_ids <- temp_scene[["objects"]][[1]]["id"]

test_verts <- rgl.attrib(LM_ids, "vertices")


rgl.projection()

c("modelMatrix","projMatrix","userMatrix","userProjection")

test_list <- list()
test_list[["proj"]] <- matrix(c(3.732051,0,0,0,0,3.732051,0,0,0,0,-3.863703,-378.855804,0,0,-1,0), nrow = 4, ncol = 4)
test_list[["region"]] <- c(4.415091, -48.092817)

shinySelectPoints3d(test_verts, test_list)

rgl.user2window(test_verts, test_list$proj)

grep("model",names(environment(f)$proj))

# output$brush_info_3D <- renderPrint(input$rgl_3D_brush$region) #This gets the "x" and "y" index values from the brush click....but why is it only two dimensions?
# output$brush_info_3D <- renderPrint({
#   # shinyGetPar3d("projMatrix", session)
#   cat("Output from input$rgl_3D_brush:\n")
#   print(class(input$rgl_3D_brush))
#
#   cat("\nOutput from input$par3d:\n")
#   print(class(input$par3d))
#   })
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

# observeEvent(input$rejectLM, {
#   removeModal(session)
#   click("goLM")
# })

# shinyalert(
#   title = "",
#   text = "Is this landmark correctly placed?",
#   size = "xs",
#   closeOnEsc = FALSE,
#   closeOnClickOutside = FALSE,
#   html = FALSE,
#   type = "success",
#   showConfirmButton = TRUE,
#   showCancelButton = TRUE,
#   confirmButtonText = "Yes",
#   confirmButtonCol = "#AEDEF4",
#   cancelButtonText = "No",
#   timer = 0,
#   imageUrl = "https://fontawesome.com/icons/question-circle?style=light",
#   animation = TRUE,
#   callbackR = function(x) {
#     if(x){
#       current_lm <- input$Lm_n
#       tmp_verts <- selected_vertex()
#       tmp_LMs <- selected_coords()
#
#       saveVerts(input$n, current_lm, tmp_verts)
#       saveLMs(input$n, current_lm, tmp_LMs)
#
#       output$landmarks <- renderTable(rownames = TRUE, align = "c", spacing = "xs", {
#         loadLMs()
#       })
#
#       all_verts <- loadVerts()
#
#       output$plot_3D <- renderRglwidget({
#         clear3d(type = "all")
#         plot3d(MeshData(),add = TRUE)
#         rgl.spheres(all_verts[,1], all_verts[,2], all_verts[,3],
#                     radius = 0.5, color = c("red"), add = TRUE)
#         rglwidget(scene3d(minimal = FALSE),
#                   shared = sharedData,
#                   shinyBrush = "rgl_3D_brush")
#       })
#     }
#   }
# )
# })
