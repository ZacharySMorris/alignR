### alignR function wrapper ###

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
