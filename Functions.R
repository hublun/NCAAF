#============functions==================

#----------- processing game clock ---------

convert_game_clock <- function(time_string, sep_symbol=":"){
  time_string <- str_replace_all(time_string, " - ", ":")
  time_string <- str_extract(time_string, "\\d{1,2}:\\d{2}:\\d")
  vec <- as.integer(unlist(str_split(time_string, sep_symbol)))
  return (vec[1]*60+vec[2]+(4-vec[3])*15*60)
  }
#-------------------------------------------

#======================== ================
