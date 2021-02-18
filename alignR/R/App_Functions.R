###Function to add coordinates to data.frame

saveLMs <- function(x,current_lm,keep) {
  if (exists("landmarks")){
    landmarks[as.numeric(current_lm),] <<- keep
  } else{
    landmarks <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
    landmarks[as.numeric(current_lm),] <<- keep
  }
}

loadLMs <- function() {
  if (exists("landmarks")) {
    landmarks
  }
}

clearLMs <- function(x){
  if (exists("landmarks")){
    landmarks <<- array(NA, dim = c(x,3), dimnames = list(c(paste("LM", 1:x, sep = "")), c("X","Y","Z")))
  }
}


rgl.landmarking <- function(x, temp_scene, specimen) {
  current_lm <- as.numeric(x)

  LM_ids <- temp_scene[["objects"]][[1]]["id"]

  keep <- ans <- NULL
  keep <- selectpoints3d(LM_ids, value = FALSE, button = "right")[2]

  points3d(specimen[keep, 1], specimen[keep, 2], specimen[keep,3],
           size = 10, color = "red", add = TRUE)

  return(keep)
}







runApp(
  list(ui = bootstrapPage(pageWithSidebar(
    headerPanel("Rummy"),
    sidebarPanel( tags$hr() ),

    mainPanel(

      tableOutput("dummy"),
      # change style:
      tags$head(tags$style("#dummy table {background-color: red; }", media="screen", type="text/css"))
    )

  )
  )

  ,
  server = function(input, output) {
    output$dummy <- renderTable({ data.frame(A=1:4,B=2:5,C=rep("aaa",4)) })
  }

  )
)

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(3, tags$strong("Right")),
          column(3, tags$strong("Wrong")),
          column(3, tags$strong("Skipped")),
          column(3, tags$strong("ToGo"))
        ),
        fluidRow(
          column(3, span(style = "color:green;font-weight:bold;", textOutput("right"))),
          column(3, textOutput("wrong")),
          column(3, textOutput("skipped")),
          column(3, textOutput("togo"))
        )
      ),

      mainPanel(

      )
    )
  ),

  server = function(input, output, session) {
    correct <- reactiveValues(num = 7)
    wrong <- reactiveValues(num = 4)
    skipped <- reactiveValues(num = 9)

    togo = 80

    output$right <- renderText(wrong$num)
    output$wrong <- renderText(wrong$num)
    output$skipped <- renderText(skipped$num)
    output$togo <- renderText(togo)
  }
)

