### Automated Pseudolandmark Alignment App ###

alignR_ui <- fluidPage(
  useShinyjs(),
  useShinyalert(),
  navbarPage("landmark alignR",
             id = "tab_n",
             tabPanel("Discrete Landmark Analysis", value = 1),
             tabPanel("Mixed Landmarking", value = 2),
             tabPanel("Automated Pseudolandmark Alignment", value = 3,
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        # Sidebar panel for inputs ----
                        sidebarPanel( style = "background-color:#c55347;border-color:#c55347;color:#fff",
                                      # conditionalPanel(condition="input.tabselected==3",
                                      # tags$style(".well {background-color:#c55347;border-color:#c55347;color:#fff}"),
                                      numericInput("n", "Number of fixed landmarks", 6),
                                      # uiOutput("n"),
                                      uiOutput("Lm_n"),
                                      fluidRow(
                                        actionButton("goLM", "Landmark!", icon = icon("crosshairs"), style = "color: #fff; background-color: #c55347; border-color: #c55347"),
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
                          # Output: Histogram ----
                          uiOutput("LM_n_title"),

                          #testing click extraction, replace with more specific calls when working
                          uiOutput("plot_3D_mousemode"),
                          verbatimTextOutput("brush_info_3D"),
                          #end of ui code for testing click extraction

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



