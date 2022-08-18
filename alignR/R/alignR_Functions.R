### Functions for interacting with data from alignR ###

# reading in data
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
    if(!length(tmp_rows)==2){
      warning("Two or more specimen have identical names.")
    }
    tmp_rows <- seq(tmp_ends[1]+1,tmp_ends[2]-1)
    tmp_lines <- lines[tmp_rows] #save single specimen lines
    tmp_data <- grep("^\t", sub("\t", "", grep("^\t", tmp_lines,value=T)),value=T,invert=T) #subset to the rows with landmark data + rownames
    tmp_var <- strsplit(grep("<[type=[:alnum:]|_.]+[ ]", tmp_lines,value=T)," ") #split lines that identify type into parts

    row_n <- as.numeric(unique(gsub("nrow=","",grep("nrow=",unlist(tmp_var),value=T)))) #saves number of rows
    row_lab <- unique(grep("[[:alpha:]]",unlist(strsplit(tmp_data,"\t")),value=T)) #this grabs the rownames

    col_n <- as.numeric(unique(gsub("ncol=","",grep("ncol=",unlist(tmp_var),value=T)))) #saves number of columns
    col_lab <- unlist(strsplit(gsub("^\t\t","", grep("^\t\t", tmp_lines,value=T)),"\t")) #grab and prep colnames

    pre_matrix <- as.numeric(unlist(strsplit(sub("\\w*\t", "", tmp_data),"\t")))
    tmp_matrix <- matrix(pre_matrix,ncol=3,byrow = T,dimnames =list(row_lab,col_lab))
    tmp_matrix

    row_list[[tmp_id]] <- row_lab
    col_list[[tmp_id]] <- col_lab

    landmark_df[[tmp_id]] <- tmp_matrix
  }

#
#   col_list[[1]] == col_list
#
#   rownames(landmark_df[[1]])
#
#   lapply(landmark_df,"rownames")
#   unique(unlist(lapply(landmark_df,"colnames")))
#
#
#   if( length(unique(gsub("nrow=|ncol=","",grep("nrow=|ncol=",paste(lines,sep = "\n"),value=T)))) == 1 )
#   length(unique(gsub("nrow=","",grep("nrow=",unlist(test_line),value=T)))) == 1 #checks that all data have the same number of rows
#   length(unique(gsub("ncol=","",grep("ncol=",unlist(test_line),value=T)))) == 1 #checks that all data have the same number of columns
#   grep("^\t", temp_data2,value=T) == grep("^\t", temp_data2,value=T)[1] #check that all col names match

  tmp_array <- array(dim=c(length(row_list[[1]]),length(col_list[[1]]),length(id_names)),dimnames=list(row_list[[1]], col_list[[1]],id_names))

  for (i in seq(length(id_names))){
    tmp_array[,,i] <- landmark_df[[i]]
  }

  return(tmp_array)
}


test_read <- readLandmarks("Landmarks_test.txt")

test_gpa <- gpagen(test_read)
test_pca <- gm.prcomp(test_gpa$coords)
plot(test_pca)


# fixed_lms

scan("Landmarks_test.txt", what = "list") == "type="
read_xml("Landmarks_test.txt")

grep("type=",read_lines)


file <- readLines("Landmarks_test.txt")

if (!file.exists(file))
  stop(paste0("No file by the name '", file, "' could not be found in the current working directory."))
read_lines <- readLines("Landmarks_test.txt")
read_lines <- paste(read_lines, collapse = "\n")
read_lines <- gsub("(>)([[:print:]])", "\\1\n\\2", read_lines)
read_lines <- gsub("([[:print:]])(</)", "\\1\n\\2", read_lines)
lines <- strsplit(x = read_lines, split = "\n")[[1]]
lines <- gsub("^[\t]*", "", lines)
object_type <- "vector"
read_xml_lines <- readXMLLines(lines[2:7])
rlist <- list(read_xml_lines$rlist)
names(rlist)[1] = read_xml_lines$obj.name
rlist


read_lines <- readLines("Landmarks_test.txt")
lines <- sub("\t", "", read_lines)
test_line <- strsplit(lines[grep("<[type=[:alnum:]|_.]+[ ]", lines)]," ") #split lines that identify type into parts

strsplit(read_lines, "<[[:alnum:]|_.]+[._|>]|_.</[[:alnum:]|_.]+[._|>]")

grep("</[[:alnum:]|_.]+[._|>]", lines,value=T)

gsub("<|>","",grep("<[[:alnum:]|_.]+[._|>]", lines,value=T))

str_which(lines, "type=")

temp_data <- grep("^\t", lines,value=T)
temp_data2 <- sub("\t", "", temp_data)
temp_data3 <- grep("^\t", temp_data2,value=T,invert=T)

grep("[[:alpha:]+*\t]", temp_data3,value=T) ##still need to grab landmark names

temp_data4 <- as.numeric(unlist(strsplit(sub("\\w*\t", "", temp_data3),"\t")))

matrix(temp_data4,ncol=3,byrow = T)

strsplit(temp_data2,"\t")
grep("[[:alpha:]]",unlist(strsplit(temp_data3,"\t")),value=T) #this grabs the rownames

grep("[[:alpha:]|\t]",temp_data2[2],value=T)

length(unique(gsub("nrow=","",grep("nrow=",unlist(test_line),value=T)))) == 1 #checks that all data have the same number of rows
row_n <- as.numeric(unique(gsub("nrow=","",grep("nrow=",unlist(test_line),value=T)))) #saves number of rows

row_lab <- unique(grep("[[:alpha:]]",unlist(strsplit(temp_data3,"\t")),value=T)) #this grabs the rownames

length(unique(gsub("ncol=","",grep("ncol=",unlist(test_line),value=T)))) == 1 #checks that all data have the same number of columns
col_n <- as.numeric(unique(gsub("ncol=","",grep("ncol=",unlist(test_line),value=T)))) #saves number of columns

grep("^\t", temp_data2,value=T) == grep("^\t", temp_data2,value=T)[1] #check that all col names match
col_lab <- paste("c(", gsub("\t",",",gsub("^\t","", grep("^\t", temp_data2,value=T)[1])), ")",sep = "") #grab and prep colnames
col_lab <- unlist(strsplit(gsub("^\t","", grep("^\t", temp_data2,value=T)[1]),"\t")) #grab and prep colnames

id_names <- gsub("<|>","",grep("<[[:alnum:]|_.]+[._|>]", lines,value=T)) #grabs specimen names and removes extra text

##works but puts data in by col instead of by row!
tmp_array <- array(dim=c(row_n,col_n,length(id_names)),dimnames=list(row_lab,col_lab,id_names))

end_lms <- grep(row_lab[row_n],temp_data3)
start_lms <- c(1,end_lms[-length(end_lms)]+1)

for (i in seq(length(id_names))){
  tmp_array[,,i] <- matrix(temp_data4,ncol=col_n,byrow = T)[start_lms[i]:end_lms[i],]
}


##potential for breaking up the text into individual specimens first, but then need to write the rest for individuals rather than the whole dataset
landmark_df<-list()
for (i in seq(length(id_names))){
  tmp_id <- id_names[i]
  tmp_rows <- grep(tmp_id,lines)
  if(!length(tmp_rows)==2){
    warning("Two or more specimen have identical names.")
  }
  tmp_data <- seq(tmp_rows[1],tmp_rows[2])
  landmark_df[[tmp_id]] <- lines[tmp_data]
  }


landmark_df

dimnames(tmp_array)[[3]]

as.numeric(matrix(temp_data4,ncol=3,byrow = T))


grep("\t", lines,value=T)

all(sub("<type=","",grep("<type=",unlist(test_line),value = TRUE)) == "matrix") #grabs parts that identify type and check that they are all matrix

all(as.logical(gsub("rownames=","",grep("rownames=",unlist(test_line),value = TRUE)))) #grabs parts that identify if rownames are present



"rownames=TRUE" %in% unlist(test_line)

test_linelapply(test_line, "grep", pattern="type=")

writeLines("<[type=[:alnum:]|_.]+[ ]")

read_lines


gsub("<","",lines[2])
gsub(" >","",lines[2])
gsub(">","",lines)

strsplit(lines[2], "[[:space:]]+")

readXMLLines(lines[2:7])

inlist <- strsplit(readLines("Landmarks_test.txt"), "[[:space:]]+")
pathways <- lapply(inlist, tail, n = -1)
names(pathways) <- lapply(inlist, head, n = 1)

lines[grep("<[[:alnum:]|_.]+[ |>]",lines)]



reg_expr_open <- regexpr(pattern = "<[[:alnum:]|_.]+[ |>]",
                         text = lines)
lines[reg_expr_open[2]]

reg_expr_close <- regexpr(pattern = "</[ ]*[[:alnum:]|_.]+[ ]*>",
                          text = lines[7])

object_name <- gsub("<|[\t]", "", substr(lines[2], reg_expr_open,
                                         reg_expr_open + attr(reg_expr_open, "match.length") -
                                           2))

#
