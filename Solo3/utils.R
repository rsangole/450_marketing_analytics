tc <- function(x){
  print(x %>% tabyl())
  message(class(x))
  if(is.character(x))
    try(plot(table(x)))
  try((hist(x, breaks = 50)),silent = T)
  return(class(x))
}
purrtc <- function(DF, string){
  DF %>% 
    select(starts_with(string)) %>% 
    purrr::map(~tc(.x))
}
