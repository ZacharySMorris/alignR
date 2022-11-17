### alignR function wrapper ###
#' Landmark alignR
#'
#' Shiny application to collect 3D landmark data on surface files
#'
#' @details
#' This function prepares mesh object data for landmarking and loads the landmarking application.
#' Using a combination of rgl, Rvcg, and shiny functions, meshes are loaded into an rglwidget which can
#' be interactively manipulated. Some compromises still exist, but this interface enables easier
#' switching between specimens and re-landmarking, as landmarks can be placed in any order and
#' can be loaded for later reassessment.
#'
#' There are three options for type of landmarking: 1. fixed / discrete landmarks, 2. a mix of discrete
#' and surface pseudolandmarks, & 3. alignment landmarks for fitting surface pseudolandmarks. Switching
#' between analysis type is done interactively in the open application.
#'
#' @param file_dir A directory containing surface files to be analyzed
#' @param file_name A name for the landmark data file to be generated
#' @param loadAll A logical value which determines whether all surface files should be loaded into a list or if they should be loaded only as needed for digitizing
#'
#' @export
alignR <- function(file_dir,file_name="Landmarks.txt",loadAll=TRUE){
  #file_dir is a file directory where surface files are located on user's computer
  #loadAll is a logical value determining whether all surface files should be loaded into a list or if they should be loaded only as needed for digitizing
   file_dir <<- file_dir
   loadAll <- loadAll
   file_name <<- file_name

  # file_dir <- "~/Dropbox/alignR/alignR/data/turts/input"
  # loadAll <- TRUE

  #list of loadable filetypes
  file_type <- c("ply","stl","off")

  #create list of files and specimen names
  file_list <- dir(file_dir, full.names = TRUE)
  name_list <- tools::file_path_sans_ext(dir(file_dir))

  #file type check
  file_chk <- tools::file_ext(file_list) %in% file_type

  if(!all(file_chk)){
    warning(immediate. = TRUE, "alignR only accepts surface files (ply, stl, off) but the selected directory includes other filetypes. Only accepted types will be used.")
  }

  #create list of specimen names and file directories
  file_list <- file_list[file_chk]
  name_list <- name_list[file_chk]

  # if(loadAll){
    sp_list <<- list()

    for (i in 1:length(file_list)){
      tmp_file <- file_list[i]
      tmp_name <- name_list[i]
      tmp_mesh <- Rvcg::vcgImport(tmp_file)

      sp_list[[tmp_name]] <<- tmp_mesh
    }

    #create a list to store landmarks eventually
    lm_list <<- rep(list(NA),length(sp_list))
    names(lm_list) <<- name_list

    #create a list of specimen centroid sizes
    cs_list <- sapply(sp_list,Morpho::cSize)
    point_sizes <<- (cs_list / min(cs_list)) * 0.2

    # run shiny application to capture landmarks
    app_dir <- file.path(path.package("alignR"), "extdata", "apps", "alignR")
    shiny::runApp(appDir = app_dir)
    # shinyApp(alignR_ui, alignR_server)
}
