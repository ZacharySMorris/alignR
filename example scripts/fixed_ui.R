#### Making my own navbar UI ####

addResourcePath(prefix = 'www', directoryPath = './www')

fixed_ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  uiOutput("header"),
  titlePanel(
    tagList(wellPanel(class = "header",
      div(style="display:inline-block;", h5("landmark alignR", class = "logo"),
        # div(style="display:inline-block;",
            actionButton("tab1","Discrete Landmark Analysis"),
            actionButton("tab2","Mixed Landmarking"),
            actionButton("tab3","Automated Pseudolandmark Alignment")
        ))
  )),
  sidebarLayout(
    sidebarPanel(class = "sidebar",
      numericInput("n", "Number of fixed landmarks", value=10, step=1),
      uiOutput("Lm_n"),
      fluidRow(
        actionButton("submitLM", "Landmark!", icon = icon("crosshairs")),
        hidden(actionButton("confirmLM", "Confirm", icon = icon("check-circle"))),
        # actionButton(ns("clear"), "Clear", icon = icon("eraser"), style = tmp_style),
        align = "center",
      ),
      fluidRow(
        actionButton("Last_LM", "Previous LM", icon = icon("arrow-circle-left")),
        actionButton("Next_LM", "Next LM", icon = icon("arrow-circle-right")),
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
    ),
    mainPanel(
      uiOutput("LM_n_title"),
      uiOutput("cur_specimen"),
      # verbatimTextOutput("testing", placeholder = TRUE),
      rglwidgetOutput("SpecimenPlot", width = "600px", height = "600px"),
      fluidRow(
        uiOutput("plot_3D_mousemode"),
        align = "center"
      )
    )
  )
  )


fixed_server <- alignR_server <- function(input, output, session) {


  output$testing <- renderPrint(
    cat("Tab1:", input$tab1, "\n Tab2:", input$tab2, "\n Tab3:", input$tab3, sep = "")
  )

  # analysis_style <- reactive({
  #
  # })

  ## initialize UI
  output$header <- renderUI({
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/startup.css"),
        ))
    })
  ##
  ## update UI based on analysis clicked
  ## need to add warnings about changing the UI after starting one kind of analysis and also function to clear data when changing analyses
  observeEvent(input$tab1,{
               output$header <- renderUI(
                 tagList(
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "www/fixed.css"),
                     )))
               })
  observeEvent(input$tab2,{
               output$header <- renderUI(
                 tagList(
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "www/mixed.css"),
                     )))
               })
  observeEvent(input$tab3,{
               output$header <- renderUI(
                 tagList(
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "www/auto.css"),
                     )))
               })
  ##

}

shinyApp(fixed_ui,alignR_server)

shinyApp(alignR_ui, alignR_server)



# wellPanel(style = "max-height:50px;max-width:fit-content;padding:0000;color: #fff; background-color: Steelblue; border-color: Steelblue",
#   actionButton("tab1","Discrete Landmark Analysis",style = "color: #fff; background-color: Steelblue; border-color: Steelblue")
# ),
# wellPanel(style = "max-height:50px;max-width:fit-content;padding:0000;color: #fff; background-color: #648f7b; border-color: #648f7b",
#           actionButton("tab2","Mixed Landmarking",style = "color: #fff; background-color: #648f7b; border-color: #648f7b")
# ),
;outline-color: Steelblue
