#============functions==================

#----------- processing game clock ---------

convert_game_clock <- function(time_string, sep_symbol=":"){
  time_string <- str_replace_all(time_string, " - ", ":")
  time_string <- str_extract(time_string, "\\d{1,2}:\\d{2}:\\d")
  vec <- as.integer(unlist(str_split(time_string, sep_symbol)))
  return (vec[1]*60+vec[2]+(4-vec[3])*15*60)
  }
#-------------------------------------------
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
    yards <- as.integer(str_sub(pom[2], start=1, end=pos))
  }
  
  minutes <- 0
  seconds <- 0
  away_score <- 0
  home_score <- 0
  
  
  #-----------------------------------------------
  return (list(result, num_plays, yards))
}