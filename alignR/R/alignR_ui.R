### Automated Pseudolandmark Alignment App ###

#plot_3D_mousemode select option {border:0px;outline:0px;background-color:#708090E6;border-color:#708090E6;outline-color:#708090E6;color:#fff;}
#plot_3D_mousemode select option:hover {background-color:#6F7881;color:#fff;}

alignR_ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  setBackgroundColor(color = "SlateGray"),
  tags$head(tags$style(".navbar-static-top {border-width:0000;}
                       .navbar-default .navbar-nav {font-weight: 500;}
                       .navbar-default .navbar-nav .active a[data-value='1'] {background-color:#4682B4E6;color:#fff;}
                       .navbar-default .navbar-nav .active a[data-value='2'] {background-color:#648F7BE6;color:#fff;}
                       .navbar-default .navbar-nav .active a[data-value='3'] {background-color:#C55347E6;color:#fff;}
                       #toast-container {position:relative;left:20%;}
                       #toast-container .toast {box-sizing:content-box;}
                       #toast-container .toast-warning {background-color:#c55347;color:#fff;}
                       #Lm_n .selectize-input.full {background-color:#c55347;border-color:#c55347;outline-color:#b04a3c;color:#fff;}
                       #Lm_n .selectize-control.single .selectize-input:after {border-color:#fff transparent transparent transparent}
                       #Lm_n .selectize-control.single .selectize-input.dropdown-active:after {border-color:transparent transparent #fff transparent}
                       #Lm_n .selectize-dropdown {background-color:#C55347E6;color:#fff;}
                       #Lm_n .selectize-dropdown-content .active {background-color:#b04a3c;color:#fff;}
                       #plot_3D_mousemode select {border:0px;outline:0px;background-color:SlateGray;color:#fff;}
                       ")
            # tags$script("# Shiny.addCustomMessageHandler('selectingMouse', function(selecting){
            #        setMouseMode(selecting)
            # });
            #             ")

  ),
  navbarPage("landmark alignR",
             id = "tab_n",
             tabPanel("Discrete Landmark Analysis", value = 1),
             tabPanel("Mixed Landmarking", value = 2),
             tabPanel("Automated Pseudolandmark Alignment", value = 3,
                      sidebarLayout(
                        sidebarPanel(style = "background-color:#c55347;border-color:#c55347;color:#fff",
                                     hidden(numericInput("n", "Number of fixed landmarks", 6)),
                                     uiOutput("Lm_n"),
                                     fluidRow(
                                       actionButton("submitLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                                       hidden(actionButton("confirmLM", "Confirm", icon = icon("check-circle"), style = "color: #fff; background-color: #c55347; border-color: #c55347")),
                                       # actionButton("clear", "Clear", icon = icon("eraser"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                                       align = "center",
                                     ),
                                     fluidRow(
                                       actionButton("Last_LM", "Previous LM", icon = icon("arrow-circle-left"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                                       actionButton("Next_LM", "Next LM", icon = icon("arrow-circle-right"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
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
                        # mainUI("LM_panel")
                        mainPanel(
                          uiOutput("LM_n_title"),
                          uiOutput("cur_specimen"),
                          # verbatimTextOutput("testing", placeholder = FALSE),
                          rglwidgetOutput("SpecimenPlot3", width = "600px", height = "600px"),
                          fluidRow(
                            uiOutput("plot_3D_mousemode"),
                            # div(style="display:inline-block", actionButton("Last_Sp", "Previous specimen", icon = icon("arrow-alt-circle-left"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray")),
                            # div(style="display:inline-block", actionButton("Next_Sp", "Next specimen", icon = icon("arrow-alt-circle-right"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray")),
                            align = "center"
                          )
                        )
                      )
             )
  )
)

# actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
# actionButton("getPar", "Get Parameters", icon = icon("sliders"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),

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


