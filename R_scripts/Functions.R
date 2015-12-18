convert.magic <- function(obj, type){
    # Converts columns in df to class specified
    FUN1 <- switch(type,
                   character = as.character,
                   numeric = as.numeric,
                   factor = as.factor)
    out <- lapply(obj, function(x) FUN1(as.character(x)))
    as.data.frame(out)
}

convert.magic <- function(obj, type){
    out <- lapply(obj, function(x) FUN1(as.character(x)))
    as.data.frame(out)
}

convert.names <- function(df) {
    # Seperates names into first and last names, converts them to upper case, pastes them together with last name first, and removes punctuation from names
    df$first_name <- lapply(strsplit(as.character(df$Player), " "), "[", 1)
    df$last_name <- lapply(strsplit(as.character(df$Player), " "), "[", 2)
    
    df$first_name <- toupper(df$first_name)
    df$last_name <- toupper(df$last_name)
    
    df$Player <- paste0(df$last_name, df$first_name, sep = "")

    df$Player <- gsub("[[:punct:]]", "", df$Player)
    df <- select(df, -first_name, -last_name)
    return(df)
}

team.names <- function(df, column) {
    df$column[df$column == "WSH", ] <<- "WAS"
    return(df)
}



