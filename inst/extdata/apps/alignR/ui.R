### Automated Pseudolandmark Alignment App ###
#' @import shiny
#' @import shinyjs
#' @import shinyFeedback
#' @importFrom rgl rglwidgetOutput
#'
# shiny::addResourcePath(prefix = 'www', directoryPath = '~/www') #this line is needed to find the right css files...not sure how to make it correct for R package

jscode <- "shinyjs.init = function() {
    document.getElementById('SpecimenPlot').addEventListener('contextmenu', event => event.preventDefault());
  }"

ui <- shiny::fluidPage(
  # withSpinner(rglwidgetOutput("SpecimenPlot"), type = 6, color = "#4682B4E6", color.background ="SlateGray", size = 2),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jscode, functions = c()),
  # useShinyalert(),
  shinyFeedback::useShinyFeedback(),
  tags$head(HTML("<title>landmark alignR</title>")), #Without company logo
  #tags$head(HTML("<title>landmark alignR</title> <link rel='icon' type='image/gif/png' href='alignR_logo.png'>")), #WIth company logo
  uiOutput("header"),
  conditionalPanel(condition = "input.SetupComplete == 'no'",
                   tags$style(
                     HTML('body {background-color: SlateGray; border-color: SlateGray;}
                        #LoadMessage {color: #fff; background-color: SlateGray; border-color: SlateGray;padding:100px;}')
                   ),
                   fluidRow(id="LoadMessage",
                            h2("Loading dataset..."),
                            img(src="www/Alligator_FS18e_MaxillaryProliferation.gif", width='500px'),
                            align = "center",),
                   ),
  conditionalPanel(condition = "input.SetupComplete == 'yes'",
  titlePanel(
    tagList(wellPanel(class = "header",
                      div(style="display:inline-block;", h5("landmark alignR", class = "logo"),
                          # tags$i(class="fa-brands fa-r-project"),
                          # div(style="display:inline-block;",
                          actionButton("tab1","Discrete Landmark Analysis"),
                          actionButton("tab2","Mixed Landmarking"),
                          actionButton("tab3","Automated Pseudolandmark Alignment"),
                      ))
    )),
  # conditionalPanel(condition = "input.SetupComplete == 'yes'",
  sidebarLayout(
    sidebarPanel(class = "sidebar",
                 numericInput("n", "Number of fixed landmarks", value=10, step=1),
                 uiOutput("Lm_n"),
                 # uiOutput("curLM"),
                 fluidRow(
                   actionButton("getPar", "Set Position", icon = icon("sliders")),
                   shinyjs::hidden(actionButton("submitLM", "Landmark!", icon = icon("crosshairs"))),
                   shinyjs::hidden(actionButton("confirmLM", "Confirm", icon = icon("circle-check"))),
                   # actionButton("auto_align","Align Surface Landmarks!", icon = icon("cube")),
                   align = "center",
                 ),
                 fluidRow(
                   actionButton("Last_LM", "Previous LM", icon = icon("circle-arrow-left")),
                   actionButton("Next_LM", "Next LM", icon = icon("circle-arrow-right")),
                   align = "center",
                 ),
                 fluidRow(
                   actionButton("clear", "Clear All", icon = icon("eraser")),
                   align = "center",
                 ),
                 fluidRow(
                   uiOutput("landmarks"),
                   align = "center",
                 ),
                 fluidRow(
                   shinyjs::hidden(uiOutput("auto_align")),
                   align = "center",
                 ),
                 fluidRow(
                   checkboxInput("NoWarnings", "Do not remind me to confirm landmark selections.", value = FALSE, width = NULL),
                   align = "center",
                 ),
                 fluidRow(
                   shinyjs::hidden(radioButtons(inputId = "SetupComplete", label = NULL, choices = c('yes','no'),selected = 'no')),
                   align = "center",
                 ),

    ),
    mainPanel(
      uiOutput("spec_name"),
      uiOutput("cur_specimen"),
      # selectInput("cur_specimen", NULL, names(sp_list),width = "600px"), #is it ok to have the pointing to a specific named object in memory?
      verbatimTextOutput("testing", placeholder = FALSE),
      rgl::rglwidgetOutput("SpecimenPlot", width = "600px", height = "600px"),
      fluidRow(
        uiOutput("plot_3D_mousemode"),
        align = "center"
      ),
      fluidRow(
        actionButton("load", "Load", icon = icon("upload")),
        downloadButton("save", "Save", icon = icon("floppy-disk")),
        downloadButton("quit", "Save & Quit", icon = icon("right-from-bracket")),
        # actionButton("save", "Save", icon = icon("floppy-disk")),
        # actionButton("quit", "Save & Quit", icon = icon("right-from-bracket")),
        align = "center"
      )
    )
    )
  )
)
# )

# , style = "color: #fff; background-color: SlateGray; border-color: SlateGray; outline-color: SlateGray"
#
# .form-control[disabled], .form-control[readonly], fieldset[disabled] .form-control {
#   background-color: slategray;
#   opacity: 1;
# }
#
# .btn-default {
#   color: #fff;
#     background-color: slategray;
#   border-color: slategray;
# }



#### OLD CODE TO DELETE BEFORE FINISHING ####



# alignR_ui <- fluidPage(
#   useShinyjs(),
#   useShinyFeedback(),
#   setBackgroundColor(color = "SlateGray"),
#   tags$head(tags$style(".navbar-static-top {border-width:0000;}
#                         .navbar-default .navbar-nav {font-weight: 500;}
#                         .navbar-default .navbar-nav .active a[data-value='1'] {background-color:#4682B4E6;color:#fff;}
#                         .navbar-default .navbar-nav .active a[data-value='2'] {background-color:#648F7BE6;color:#fff;}
#                         .navbar-default .navbar-nav .active a[data-value='3'] {background-color:#C55347E6;color:#fff;}
#                         #toast-container {position:relative;left:20%;}
#                         #toast-container .toast {box-sizing:content-box;}
#                         input[type=number]::-webkit-outer-spin-button,
#                         input[type=number]::-webkit-inner-spin-button {-webkit-appearance: none;margin: 0;}
#                        ")),
#   uiOutput("header"),
#   navbarPage("landmark alignR",
#              id = "tab_n",
#              tabPanel("Discrete Landmark Analysis", value = 1,
#                       sidebarLayout(
#                         sidebar_UI("Tab1", color = "SteelBlue"),
#                         mainPanel(
#                           h2("Tab 1"),
#                           uiOutput("testing"),
#                           rglwidget_UI("Tab1")
#                         )
#                       )
#              ),
#              tabPanel("Mixed Landmarking", value = 2,
#                       sidebarLayout(
#                         sidebar_UI("Tab2", color = "#648f7b"),
#                         mainPanel(
#                           h2("Tab 2"),
#                           uiOutput("testing2"),
#                           rglwidget_UI("Tab2")
#                         )
#                       )
#              ),
#              tabPanel("Automated Pseudolandmark Alignment", value = 3,
#                       sidebarLayout(
#                         sidebar_UI("Tab3", color = "#c55347"),
#                         # mainUI("LM_panel")
#                         mainPanel(
#                           uiOutput("LM_n_title"),
#                           uiOutput("cur_specimen"),
#                           # verbatimTextOutput("testing", placeholder = FALSE),
#                           rglwidget_UI("Tab3"),
#                           # rglwidgetOutput("SpecimenPlot3", width = "600px", height = "600px"),
#                           fluidRow(
#                             uiOutput("plot_3D_mousemode"),
#                             # div(style="display:inline-block", actionButton("Last_Sp", "Previous specimen", icon = icon("arrow-alt-circle-left"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray")),
#                             # div(style="display:inline-block", actionButton("Next_Sp", "Next specimen", icon = icon("arrow-alt-circle-right"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray")),
#                             align = "center"
#                           )
#                         )
#                       )
#              )
#   )
# )




# tags$head(tags$style(".navbar-static-top {border-width:0000;}
#                        .navbar-default .navbar-nav {font-weight: 500;}
#                        .navbar-default .navbar-nav .active a[data-value='1'] {background-color:#4682B4E6;color:#fff;}
#                        .navbar-default .navbar-nav .active a[data-value='2'] {background-color:#648F7BE6;color:#fff;}
#                        .navbar-default .navbar-nav .active a[data-value='3'] {background-color:#C55347E6;color:#fff;}
#                        #toast-container {position:relative;left:20%;}
#                        #toast-container .toast {box-sizing:content-box;}
#                        #toast-container .toast-warning {background-color:#c55347;color:#fff;}
#                        #Lm_n .selectize-input.full {background-color:#c55347;border-color:#c55347;outline-color:#b04a3c;color:#fff;}
#                        #Lm_n .selectize-control.single .selectize-input:after {border-color:#fff transparent transparent transparent}
#                        #Lm_n .selectize-control.single .selectize-input.dropdown-active:after {border-color:transparent transparent #fff transparent}
#                        #Lm_n .selectize-dropdown {background-color:#C55347E6;color:#fff;}
#                        #Lm_n .selectize-dropdown-content .active {background-color:#b04a3c;color:#fff;}
#                        #plot_3D_mousemode select {border:0px;outline:0px;background-color:SlateGray;color:#fff;}
#                        ")


# actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
# actionButton("getPar", "Get Parameters", icon = icon("sliders"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),


# sidebarPanel(style = "background-color:#c55347;border-color:#c55347;color:#fff",
#              hidden(numericInput("n", "Number of fixed landmarks", 6)),
#              uiOutput("Lm_n"),
#              fluidRow(
#                actionButton("submitLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
#                hidden(actionButton("confirmLM", "Confirm", icon = icon("check-circle"), style = "color: #fff; background-color: #c55347; border-color: #c55347")),
#                # actionButton("clear", "Clear", icon = icon("eraser"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
#                align = "center",
#              ),
#              fluidRow(
#                actionButton("Last_LM", "Previous LM", icon = icon("arrow-circle-left"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
#                actionButton("Next_LM", "Next LM", icon = icon("arrow-circle-right"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
#                align = "center",
#              ),
#              fluidRow(
#                uiOutput("landmarks"),
#                align = "center",
#              ),
#              fluidRow(
#                checkboxInput("NoWarnings", "Do not remind me to confirm landmark selections.", value = FALSE, width = NULL),
#                align = "center",
#              ),
# ),



# tabPanel("Discrete Landmark Analysis", value = 1,
#          sidebarLayout(
#            sidebarPanel( style = "background-color:SteelBlue;border-color:SteelBlue;color:#fff",
#                          numericInput("n", "Number of fixed landmarks", 10),
#                          uiOutput("Lm_n"),
#                          fluidRow(
#                            actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: SteelBlue; border-color: SteelBlue"),
#                            align = "center",
#                          ),
#                          fluidRow(
#                            actionButton("Last_LM_1", "Previous landmark", icon = icon("arrow-circle-left"), style = "color: #fff; background-color: SteelBlue; border-color: SteelBlue"),
#                            actionButton("Next_LM_1", "Next landmark", icon = icon("arrow-circle-right"), style = "color: #fff; background-color: SteelBlue; border-color: SteelBlue"),
#                            align = "center",
#                          ),
#                          fluidRow(
#                            uiOutput("landmarks"),
#                            align = "center",
#                          ),
#            ),
#            mainPanel(
#              titlePanel("Placeholder for rgl1"),
#              # uiOutput("LM_n_title"),
#              rglwidgetOutput("SpecimenPlot1", width = "512px", height = "512px"),
#              # uiOutput("keep_title"),
#              actionButton("Last_Sp", "Previous specimen", icon = icon("arrow-alt-circle-left"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray"),
#              actionButton("Next_Sp", "Next specimen", icon = icon("arrow-alt-circle-right"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray"),
#            ),
#          ),
# ),
# tabPanel("Mixed Landmarking", value = 2,
#          sidebarLayout(
#            sidebarPanel(  style = "background-color:#648f7b;border-color:#648f7b;color:#fff",
#                           numericInput("n", "Number of fixed landmarks", 10),
#                           uiOutput("Lm_n"),
#                           fluidRow(
#                             actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #648f7b; border-color: #648f7b"),
#                             align = "center",
#                           ),
#                           fluidRow(
#                             actionButton("Next_LM_1", "Previous landmark", icon = icon("arrow-circle-left"), style = "color: #fff; background-color: #648f7b; border-color: #648f7b"),
#                             actionButton("Next_LM_1", "Next landmark", icon = icon("arrow-circle-right"), style = "color: #fff; background-color: #648f7b; border-color: #648f7b"),
#                             align = "center",
#                           ),
#            ),
#            mainPanel(
#              titlePanel("Placeholder for rgl2"),
#              # uiOutput("LM_n_title"),
#              # hidden(
#              #   rglwidgetOutput("SpecimenPlot2", width = "512px", height = "512px")
#              # ),
#              # uiOutput("keep_title"),
#              actionButton("Last_Sp", "Previous specimen", icon = icon("arrow-alt-circle-left"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray"),
#              actionButton("Next_Sp", "Next specimen", icon = icon("arrow-alt-circle-right"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray"),
#            ),
#          ),
# ),


