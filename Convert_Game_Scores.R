#=============================================================
convert_game_result <- function(gresult="Game Result"){
  
  ps <- 0
  pa <- 0
  if (is.na(gresult))
    return (c(0,0))
  if (str_detect(gresult,"\\d{1,2}-\\d{1,2}")==TRUE){
    
    scores <- as.integer(unlist(str_extract_all(gresult, "\\d{1,2}")))
    
    #print(scores)
    #print(gresult)
    if (str_detect(gresult,"W\\d") == TRUE){
      #print("W")
      ps <- scores[1]
      pa <- scores[2]
    } else {
      ps <- scores[2]
      pa <- scores[1]      
    }
  }
  
  return (c(ps,pa))
}
#=============================================================