### alignR function wrapper ###

alignR <- function(file_dir,loadAll=TRUE){
  #file_dir is a file directory where surface files are located on user's computer
  #loadAll is a logical value determining whether all surface files should be loaded into a list or if they should be loaded only as needed for digitizing
  # file_dir <- file_dir
  # loadAll <- loadAll

  file_dir <- "surfacefiles"
  loadAll <- TRUE

  #list of loadable filetypes
  file_type <- c("ply","stl")

  #create list of files and specimen names
  file_list <- dir(file_dir, full.names = TRUE)
  name_list <- file_path_sans_ext(dir(file_dir))

  #file type check
  file_chk <- file_ext(file_list) %in% file_type

  if(!all(file_chk)){
    warning("alignR only accepts surface files (ply, stl) but the selected directory includes other filetypes. Only accepted types will be used.")
  }

  #create list of specimen names and file directories
  file_list <- file_list[file_chk]
  name_list <- name_list[file_chk]

  if(loadAll){
    sp_list <- list()

    for (i in 1:length(file_list)){
      tmp_file <- file_list[i]
      tmp_name <- name_list[i]
      tmp_mesh <- vcgImport(tmp_file)

      sp_list[[tmp_name]] <- tmp_mesh
    }

    # # get the name of the passed object
    # object_to_change <- deparse(substitute(sp_list))
    #
    # # get the object from a given environment
    # val <- get(object_to_change, envir = .GlobalEnv)
    # # ?environment
    #
    # # Save the object as a reactive value
    # values <- reactiveValues(x = val)

    cur_spec <- reactiveVal(1)

    shinyApp(alignR_ui, alignR_server)
    # lm_list <- runApp("alignR_all")

  } else{

    shinyApp(alignR_ui, alignR_server)
    # lm_list <- runApp("alignR")
  }







  sp_list <- list()

  tmp.sf <- vcgImport("surfacefiles/Giraffe skull.ply")


  match(file_ext(tmp),file_type)

  if (file_ext(tmp))
    file_ext(tmp)

}
