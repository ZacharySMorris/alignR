### Functions for interacting with data from alignR ###

# writing out alignR landmark data (built off of list2XML4R in the StereoMorph package)
writeLandmarks <- function(x, file, ind=0){
  #x is a list or array of landmark data created in alignR

  # x = lm_list

  if(length(x) == 0) return()

  str <- '' #create blank XML string to be built up

  if(class(x) == "list"){

    for(i in 1:length(x)){

      name <- names(x)[i]

      if(is.null(x[[name]])) next

      if(class(x[[name]]) == "logical") {
        str <- c(str, paste(paste(rep('\t', ind), collapse=''), '<', name,'>\n', sep = ""))
        str <- c(str, paste("NA\n"))
        str <- c(str, paste(paste(rep('\t', ind), collapse=''), '</', name,'>\n', sep = ""))
      }

      if(class(x[[name]]) == c('matrix',"array")){
        if(!is.null(rownames(x[[name]])) || is.null(colnames(x[[name]]))){
          row_names <- rownames(x[[name]])
          col_names <- colnames(x[[name]])
        } else{
          row_names <- paste("LM",seq(nrow(x[[name]])),sep = "")
          col_names <- c("X","Y","Z")[seq(ncol(x[[name]]))]
        }

        # OPENING TAG
        str <- c(str, paste(paste(rep('\t', ind), collapse=''), '<', name,'>\n', sep = ""))
        # SEND LIST RECURSIVELY
        # str <- c(str, writeLandmarks(x[[name]], ind=ind+1))
        str <- c(str, paste(paste(rep('\t', ind), collapse=''), '<type=matrix rownames=', paste(row_names,collapse = ","), ' colnames=', paste(col_names,collapse = ","),' nrow=', nrow(x[[name]]),' ncol=', ncol(x[[name]]),'>\n', sep = ""))

        # str <- c(str, '\t', paste(rep('\t', ind+1), collapse=''))
        # str <- c(str, '\t')
        str <- c(str, paste0('\t\t',paste(col_names, collapse='\t'), '\n'))

        # MATRIX VALUES
        for(r in 1:nrow(x[[name]])){
          # str <- c(str, '\t', paste(rep('\t', ind), collapse=''))
          str <- c(str, paste0('\t',row_names[r],'\t',paste(x[[name]][r, ], collapse='\t'), '\n'))
        }
        str <- c(str, paste(paste(rep('\t', ind), collapse=''), '</', name,'>\n', sep = ""))
      }
      }

    }

    # if (any(unlist(lapply(x,class)) == "logical") | !all(unlist(sapply(x,class)) == c("matrix","array"))){
    #   warning("Some specimens lack a matrix of landmark data.")
    #   }

  # if(class(x) == "array"){
  #
  # }

  # str <- paste(sub("\n$","",str), collapse='')
  str <- sub("\n$", "", paste(str, collapse=''))

  if(file != "") write(str, file)

}
#

# reading in alignR landmark data
readLandmarks <- function(x){
  #x is the name of an alignR landmarks file

  file <- readLines(x)
  lines <- sub("\t", "", file)
  id_names <- gsub("<|>","",grep("<[[:alnum:]|_.]+[._|>]", lines,value=T)) #grabs specimen names and removes extra text

  landmark_df <- list()
  row_list <- list()
  col_list <- list()

  for (i in seq(length(id_names))){
    tmp_id <- id_names[i]
    tmp_ends <- grep(tmp_id,lines)
    if(!length(tmp_ends)==2){
      warning("Two or more specimen have identical names.")
    }
    tmp_rows <- seq(tmp_ends[1]+1,tmp_ends[2]-1)
    tmp_lines <- lines[tmp_rows] #save single specimen lines
    if(length(tmp_lines)==1 && "NA"==tmp_lines){
      landmark_df[[tmp_id]] <- NA
      next
    }
    tmp_data <- grep("^<type|^\t",tmp_lines,value=T,invert=T) #subset to the rows with landmark data + rownames
    tmp_var <- strsplit(grep("<[type=[:alnum:]|_.]+[ ]", tmp_lines,value=T)," ") #split lines that identify type into parts

    row_n <- as.numeric(unique(gsub("nrow=","",grep("nrow=",unlist(tmp_var),value=T)))) #saves number of rows
    row_lab <- unlist(strsplit(gsub("rownames=","",grep("rownames=",unlist(tmp_var),value=T)),",")) #this grabs the rownames

    col_n <- as.numeric(unique(gsub("ncol=|>","",grep("ncol=",unlist(tmp_var),value=T)))) #saves number of columns
    col_lab <- unlist(strsplit(gsub("colnames=","",grep("colnames=",unlist(tmp_var),value=T)),",")) #grab and prep colnames

    pre_matrix <- as.numeric(unlist(strsplit(sub("\\w*\t", "", tmp_data),"\t")))
    tmp_matrix <- matrix(pre_matrix,ncol=col_n,byrow = T,dimnames =list(row_lab,col_lab))
    tmp_matrix

    row_list[[tmp_id]] <- row_lab
    col_list[[tmp_id]] <- col_lab

    landmark_df[[tmp_id]] <- tmp_matrix
  }

  #check that all matrices have the same lengths (#LM*#dimensions)
  if (all(sapply(landmark_df,length) == length(landmark_df[[1]]))){
    tmp_array <- array(dim=c(length(row_list[[1]]),length(col_list[[1]]),length(id_names)),
                       dimnames=list(row_list[[1]], col_list[[1]],id_names)) #create an array based on the dimensions of the first specimen

    for (i in seq(length(id_names))){
      tmp_array[,,i] <- landmark_df[[i]] #loop through to fill array with proper values
    }

    return(tmp_array)
  } else{
    warning("The number of landmarks or dimensions do not match across all specimens.\nThe landmark data have been output as a list, rather than an array.")
    return(landmark_df)

    }
}
#
