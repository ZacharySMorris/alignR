### Automated Pseudolandmark Alignment App ###

alignR_ui <- fluidPage(
  useShinyjs(),
  useShinyalert(),
  navbarPage("landmark alignR",
             id = "tab_n",
             tabPanel("Discrete Landmark Analysis", value = 1),
             tabPanel("Mixed Landmarking", value = 2),
             tabPanel("Automated Pseudolandmark Alignment", value = 3,
                      sidebarLayout(
                        sidebarPanel( style = "background-color:#c55347;border-color:#c55347;color:#fff",
                                      hidden(numericInput("n", "Number of fixed landmarks", 6)),
                                      uiOutput("Lm_n"),
                                      fluidRow(
                                        actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                                        hidden(actionButton("getPar", "Get Parameters", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347")),
                                        align = "center",
                                      ),
                                      fluidRow(
                                        actionButton("Last_LM_1", "Previous landmark", icon = icon("arrow-circle-left"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                                        actionButton("Next_LM_1", "Next landmark", icon = icon("arrow-circle-right"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
                                        align = "center",
                                      ),
                                      fluidRow(
                                        # DT::dataTableOutput("landmarks", width = 300), tags$hr(),
                                        uiOutput("landmarks"),
                                        align = "center",
                                      ),
                                      # width = 4,
                        ),
                        # mainUI("LM_panel")
                        mainPanel(
                          setBackgroundColor(color = "SlateGray"),
                          uiOutput("LM_n_title"),
                          uiOutput("plot_3D_mousemode"),
                          verbatimTextOutput("brush_info_3D"),
                          # registerSceneChange(),
                          # conditionalPanel(condition="input.tabselected==1",
                          rglwidgetOutput("SpecimenPlot3", width = "512px", height = "512px"),
                          uiOutput("cur_specimen"),
                          actionButton("Last_Sp", "Previous specimen", icon = icon("arrow-alt-circle-left"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray"),
                          actionButton("Next_Sp", "Next specimen", icon = icon("arrow-alt-circle-right"), style = "color: #fff; background-color: SlateGray; border-color: SlateGray"),
                          # width = 8,
                        )
                      )
             )
  )
)



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


