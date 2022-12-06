##ShinyPan3d ###

#' shinyPan3d
#'
#' This function uses the shinyBrush to locate position of click for moving specimen in rglwidget.
#'
#' @details
#' This internal function
#'
#' @param par_input An input from shinyGetPar3d & alignRPar3d to get the model, projection, and view for the current orientation of the mesh in the rglwidget.
#' @param shinyBrush The ID for the shinyBrush shared from the rglwidget which contains the output of the click
#'
#' @export
shinyPan3d <- function(par_input, shinyBrush, session){
  #par_input is an input from shinyGetPar3d to get the model, projection, and view for the current orientation of the shape in the rglwidget
  #shinyBrush contains the output of the click

  if(is.null(shinyBrush)){
    print("inactive")
    return()
  }

  dev = rgl::cur3d()
  subscene = rgl::currentSubscene3d(dev)

  ## load in variables
  tmp_proj <- par_input
  move_points <- c(shinyBrush$region[1:4])
  ##
  ## make click matrix, single click value, and convert to click to user coordinate values
  int_point <- move_points[1:2]
  fnl_point <- move_points[3:4]
  move_matrix <- cbind(matrix(move_points,ncol = 2, byrow = T), c(0,1))
  ##
  # print("inital click_matrix:")
  # print(click_matrix)
  # print("inside tmp_proj:")
  # print(tmp_proj)

  user_move <- rgl::rgl.window2user(x=move_matrix, projection = tmp_proj)
  colnames(user_move) <- c("x", "y", "z")

  xlat <- 2*(c(user_move[2,"x"]/tmp_proj$view[3], 1 - user_move[2,"y"]/tmp_proj$view[4], 0.5) - c(user_move[1,"x"]/tmp_proj$view[3], 1 - user_move[1,"y"]/tmp_proj$view[4], 0.5))

  mouseMatrix <- rgl::translationMatrix(xlat[1], xlat[2], xlat[3])
  newProjection <- mouseMatrix %*% tmp_proj$proj
  # rgl::shinySetPar3d(userProjection = newProjection, session = session, subscene = subscene )

  # session$sendCustomMessage("shinySetPar3d",
  #                           list(subscene = subscene,
  #                                parameter = "proj",
  #                                value = newProjection))

  return(list("newProjection" = newProjection, "session" = session, "subscene" = subscene))

}

#
#
# shinyPan3d <- local({
#   dev = rgl::cur3d()
#   subscene = rgl::currentSubscene3d(dev)
#   start <- list()
#   begin <- function(x, y) {
#     # activeSubscene <- par3d("activeSubscene", dev = dev) #get activeSubscene
#     # start$listeners <<- par3d("listeners", dev = dev, subscene = activeSubscene) #assign "listeners" to value, but it is actually just a number for the subscene?
#     # for (sub in start$listeners) {
#     init <- rgl::par3d(c("userProjection","viewport"), dev = dev, subscene = subscene) #get user projections and viewport in the subscrene in listners
#     init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
#     start[[as.character(sub)]] <<- init
#     start_int <<- init
#     # }
#     # rgl::shinyGetPar3d(c("scale","listeners","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
#     # tmp_par <- alignRPar3d(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
#     # start_int <<- alignRListen(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
#     # print(start_int)
#   }
#
#   update <- function(x, y) {
#     # for (sub in start$listeners) {
#     #   init <- start[[as.character(sub)]]
#     #   xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
#     #   mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
#     #   par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
#     # }
#     init <- start_int
#     xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
#     mouseMatrix <- rgl::translationMatrix(xlat[1], xlat[2], xlat[3])
#     shinySetPar3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = "SpecimenPlot" )
#   }
#   list(rglbegin = begin, rglupdate = update)
# })
#
# rglbegin <- shinyPan3d$rglbegin
# rglupdate <- shinyPan3d$rglupdate
#
# dev <- rgl::cur3d()
# subscene <- rgl::currentSubscene3d(dev)
# start <- list()
# activeSubscene <- rgl::par3d("activeSubscene", dev = dev) #get activeSubscene
# rgl::shinyGetPar3d(c("activeSubscene"), session)
# start$listeners <- rgl::par3d("listeners", dev = dev) #assign "listeners" to value, but it is actually just a number for the subscene?
#
# # for (sub in start$listeners) {
# init <- rgl::par3d(c("userProjection","viewport"), dev = dev, subscene = subscene) #get user projections and viewport in the subscrene in listners
#
# tmp_tris <- shinyPan3d(init, isolate(input$rgl_3D_brush))
#
# # init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
# # start[[as.character(sub)]] <<- init
# # }
#
# # output$testing <- renderText({
# #   paste("start$listeners", start$listeners, "init$viewport", init$viewport, "init$viewport[3]", init$viewport[3], "init$viewport[4]", init$viewport[4], sep = "\n")
# # })
#
# # Install both
# rgl::setUserCallbacks("right",
#                       begin = rglbegin,
#                       update = rglupdate)
#
# # rgl::setUserCallbacks("right",
# #                  begin = begin <- function(x, y) {
# #                    rgl::shinyGetPar3d(c("scale","listeners","modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene", "zoom", "observer"), session)
# #                    tmp_par <- alignRPar3d(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
# #                    start_int <<- alignRListen(input$par3d, zoom = ifelse(is.null(input$par3d$zoom),1,input$par3d$zoom))
# #                  },
# #                  update =   update <- function(x, y) {
# #                    init <- start_int
# #                    xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
# #                    mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
# #                    shinySetPar3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = "SpecimenPlot" )
# #                  }
# # )
