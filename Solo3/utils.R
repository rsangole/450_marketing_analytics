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
# -- functions ----
ROCplot <- function(x,t=0.5) {
  plot(
    x,
    print.thres = t,
    type = "S",
    print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
    print.thres.cex = 1,
    legacy.axes = TRUE
  )
}
calcAUC <- function(predcol, outcol){
  perf <- ROCR::performance(ROCR::prediction(predcol, outcol==pos),'auc')
  as.numeric(perf@y.values)
}
mkPredC <- function(outCol,varCol,appCol){
  ppos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pposWNA <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pposWV <- (vTab[pos,]+1e-3*ppos)/(colSums(vTab)+1e-3)
  pred <- pposWV[appCol]
  pred[is.na(appCol)] <- pposWNA
  pred[is.na(pred)] <- ppos
  tibble(X = pred)
}
mkPredN <- function(outcol,varcol,appcol){
  cuts <- unique(as.numeric(quantile(varcol,probs = seq(0,1,0.1),na.rm=T)))
  varc <- cut(varcol,cuts)
  appc <- cut(appcol,cuts)
  mkPredC(outcol,varc,appc)
}
