#=============================================================
convert_game_result <- function(gresult="Game Result"){
  ps <- 0
  pa <- 0
  if (str_detect(gresult,"\\d{1,2}-\\d{1,2}")==TRUE){
    scores <- as.integer(unlist(str_extract_all(gresult, "\\d{1,2}")))
    print(scores)
  }
  return (list(ps,pa))
}
#=============================================================