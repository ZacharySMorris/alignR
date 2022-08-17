### Shiny Modules

UI_style <-function(id, color) {
  color <- color
  transparent_color <- alpha(color, 0.9)
  outline_color <-muted(color, l=45, c=80)
  Lm_n_label <- paste("#",id, "Lm_n ", sep = "")
  tagList(
    tags$head(tags$style(
      paste("#toast-container .toast-warning {background-color:", color, ";color:#fff;}
            ",
            Lm_n_label, ".selectize-input.full {background-color:", color, ";border-color:", color, ";outline-color:", outline_color, ":#fff;}
            ",
            Lm_n_label, ".selectize-control.single .selectize-input:after {border-color:#fff transparent transparent transparent}
            ",
            Lm_n_label, ".selectize-control.single .selectize-input.dropdown-active:after {border-color:transparent transparent #fff transparent}
            ",
            Lm_n_label, ".selectize-dropdown {background-color:", transparent_color, ";color:#fff;}
            ",
            Lm_n_label, ".selectize-dropdown-content .active {background-color:", color, ";color:#fff;}
            "))
      )
    )
}

style_Server <- function(id, color) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })
    })
}

sidebar_UI <- function(color) {
  color <- color
  tmp_style = paste("background-color:", color, ";border-color:", color,";color:#fff", sep = "")
  tagList(
    sidebarPanel(style = tmp_style,
                 hidden(numericInput("n", "Number of fixed landmarks", value=6, step=1)),
                 uiOutput("Lm_n"),
                 fluidRow(
                   actionButton("submitLM", "Landmark!", icon = icon("crosshairs"), style = tmp_style),
                   hidden(actionButton("confirmLM", "Confirm", icon = icon("check-circle"), style = tmp_style)),
                   # actionButton(ns("clear"), "Clear", icon = icon("eraser"), style = tmp_style),
                   align = "center",
                 ),
                 fluidRow(
                   actionButton("Last_LM", "Previous LM", icon = icon("arrow-circle-left"), style = tmp_style),
                   actionButton("Next_LM", "Next LM", icon = icon("arrow-circle-right"), style = tmp_style),
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
}


sidebar_UI <- function(id, color) {
  ns <- NS(id)
  color <- color
  tmp_style = paste("background-color:", color, ";border-color:", color,";color:#fff", sep = "")
  tagList(
    sidebarPanel(style = tmp_style,
                 hidden(numericInput(ns("n"), "Number of fixed landmarks", value=6, step=1)),
                 uiOutput(ns("Lm_n")),
                 fluidRow(
                   actionButton(ns("submitLM"), "Landmark!", icon = icon("crosshairs"), style = tmp_style),
                   hidden(actionButton(ns("confirmLM"), "Confirm", icon = icon("check-circle"), style = tmp_style)),
                   # actionButton(ns("clear"), "Clear", icon = icon("eraser"), style = tmp_style),
                   align = "center",
                   ),
                 fluidRow(
                   actionButton(ns("Last_LM"), "Previous LM", icon = icon("arrow-circle-left"), style = tmp_style),
                   actionButton(ns("Next_LM"), "Next LM", icon = icon("arrow-circle-right"), style = tmp_style),
                   align = "center",
                   ),
                 fluidRow(
                   uiOutput(ns("landmarks")),
                   align = "center",
                   ),
                 fluidRow(
                   checkboxInput(ns("NoWarnings"), "Do not remind me to confirm landmark selections.", value = FALSE, width = NULL),
                   align = "center",
                   ),
                 )
    )
  }

sidebar_Server <- function(id, color) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })

      if (id == c("Tab1")){
        showElement(paste(id,"-n"))
        updateNumericInput(session,inputId = paste(id,"-n"), value = 10)
      }

      if (id == c("Tab1")){
        showElement(paste(id,"-n"))
        updateNumericInput(session,inputId = paste(id,"-n"), value = 10)
      }

      if (id == c("Tab3")){
        updateNumericInput(session,inputId = paste(id,"-n"), value = 6)
      }

      output$'Tab1-Lm_n' <- output$'Tab2-Lm_n' <- output$'Tab3-Lm_n' <- renderUI({
        tagList(
          fluidRow(
            h5("Landmark to digitize"),
            align = "center",
          ),
          selectInput("Lm_n",NULL, LM_values())
        )
      })

    }
    )
}

rglwidget_UI <- function(id) {
  ns <- NS(id)
  tagList(
    rglwidgetOutput(ns("SpecimenPlot"), width = "600px", height = "600px"),
  )
}


rglwidget_Server <- function(id) {
  ns <- NS(id)
  tagList(
    rglwidget(MeshData(),
              shared = ns(sharedData),
              shinyBrush = ns("rgl_3D_brush"))
  )
}
