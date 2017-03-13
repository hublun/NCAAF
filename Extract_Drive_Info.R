convert_os <- function(os_string, sep_symbol=","){
  #print(os_string)
  pom <- unlist(str_split(os_string, ",\\s?"))
  
  #str_sub(os_string, start = , end= )
  
  
  result <- NULL
  num_plays <- 0
  
  if (str_detect(pom[1], "play")){
    result <- unlist(str_split(pom[1], "\\d{1,2}\\s"))[1]
    num_plays <- as.integer(str_extract(pom[1], "\\d{1,2}"))
  }
  
  yards <- 0
  
  if (str_detect(pom[2], "yard.?")){
    pos <- str_locate(pom[2],"\\d{1,2}")
    yards <- as.integer(str_sub(pom[2], start=1, end=pos[1]))
  }
  #=============== time used and scores ===========
  os_time <- 0
  away_score <- 0
  home_score <- 0
  #-----------------------------------------------
  if (str_detect(pom[3], ":")){
    os_time_str <- str_extract(pom[3], "\\d{1,2}:\\d{2}")
    print(os_time_str)
    ptr <- str_locate(os_time_str, ":")
    minutes <- as.integer(str_sub(os_time_str, start=1, end=ptr[1]-1))
    print(str_length(os_time_str))
    seconds <- as.integer(str_sub(os_time_str, start=ptr[2]+1,end=str_length(os_time_str)))
    print(seconds)
    os_time <- minutes*60 + seconds
    
  }
  #-----------------------------------------------
  return (list(result, num_plays, yards, os_time))
}