tc <- function(x, trans='NULL'){
  print(x %>% tabyl())
  message(class(x))
  if(is.character(x))
    try(plot(table(x)))
  switch (trans,
    'NULL' = try((hist(x, breaks = 50)),silent = T),
    'log' = try((hist(log(x), breaks = 50)),silent = T),
    'inv' = try((hist(1/x, breaks = 50)),silent = T),
    'sqrt' = try((hist(x^.5, breaks = 50)),silent = T)

  )
  return(class(x))
}
purrtc <- function(DF, string,trans='NULL'){
  DF %>% 
    select(starts_with(string)) %>% 
    purrr::map(~tc(.x,trans))
}
