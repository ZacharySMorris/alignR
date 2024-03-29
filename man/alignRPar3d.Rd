% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/App_Functions.R
\name{alignRPar3d}
\alias{alignRPar3d}
\title{alignRPar3d}
\usage{
alignRPar3d(x, zoom)
}
\arguments{
\item{x}{Must be input$par3d, the shiny relative object that contains the rgl parameters
for the scene after updating via shinyGetPar3d.}

\item{zoom}{The zoom of the rgl scene.}
}
\value{
A list which contains parameters of the rgl scene (model, projection, & viewport)
}
\description{
An internal function for collecting the correct rgl scene values (i.e., model, projection,
& viewport) prior to collecting click data to place landmarks.
}
\details{
This is an internal function which assists the alignR application in properly updating and
collecting the projection values of from rglwidget. It is called when the "get parameters"
and "confirm landmark" buttons are clicked. Shiny does not provide instant access to these
values, so by running shinyGetPar3d in the main app and then running this function the app
is able to properly locate the position of the surface mesh in the rgl window. This is critical
for identifying the position of a landmarking click on the mesh itself, which is performed
by shinySelectPoints3d().
}
\keyword{internal}
