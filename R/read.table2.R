globalVariables("course.number", add = TRUE)
read.table2 <- function(file.name, course.num=course.number, na.strings=".", ...)  {
  # Reads in a table.
  # 'file.name' is the name of the data set to be scanned.
  # 'course.num' is the course number and may be numeric or character.
  # 'na.strings' is a vector of strings which are to be interpreted as 'NA' values.
  # Blank fields are also considered to be missing values.
  # '...' are the optional arguments used in 'read.table'.
  if ( !is.character(file.name) )   stop("'file.name' must be in character format.")
  if ( !is.character(course.num) & !is.numeric(course.num) )  
     stop("'course.num' must be in character or numeric format.")
  if ( !is.character(na.strings) )  stop("'na.strings' must be in character format.")
  full.name <- paste("http://educ.jmu.edu/~garrenst/math", as.numeric(course.num),
                     ".dir/datasets/", file.name, sep="")
  return(read.table(full.name, na.strings=na.strings, ...))
} # Garren19

