# efcode.attmat.f <- function(attmat){
#   # wrapper for efcode.att.f
#   # accepts a numerically coded task plan
#   # returns a effects coded version of it
#   # attmat - numeric task design matrix
#   # VALUE - effects coded design matrix
#   # REQUIRES: dummies package, efcode.att.f
#   # 2014 02 10 - LB
#   # NOT WARRANTED TO BE SUITABLE FOR ANY
#   # PARTICULAR PURPOSE. (User beware.)
#   #
#   require(dummies)
#   if(!exists("efcode.att.f")){
#     cat("Oops! Can't find efcode.att.f()\n")
#     return()
#   }
#   if(!is.matrix(attmat)){
#     cat("Oops! attmat input should be type matrix.\n")
#     return()
#   }
#   natts=ncol(attmat)   #no. of attributes
#   efmat=matrix(data=NA,ncol=1,nrow=nrow(attmat))  #placeholder
#   for (j in 1:natts){    # attribs loop
#     dummat=efcode.att.f(attmat[,j])
#     efmat=cbind(efmat,dummat)
#   }
#   efmat=efmat[,-1] #drop 1st col that has NA's
#   dimnames(efmat)=list(NULL,NULL)
#   return(efmat)
# }
#
# efcode.att.f <- function(xvec){
#   # code vector of levels into effects coded matrix
#   # xvec - numeric vec of levels
#   # REQUIRES: dummies package
#   # VALUE- effects coded matrix for xvec
#   # 2014 02 10 - LB
#   # NOT WARRANTED TO BE SUITABLE FOR ANY PARTICULAR
#   # PURPOSE. (User beware.)
#   #
#   require(dummies)    # throw exception if not available
#   att.mat=dummy(xvec)    # att.mat is dummy code mat
#   ref.ndx=att.mat[,1]    # 1st col used to locate ref level
#   att.mat=att.mat[,-1]   # drop 1st col
#   if(!is.matrix(att.mat)){
#     att.mat=matrix(att.mat,ncol=1)  # in case att is two level
#   }
#   att.mat[ref.ndx==1,]=-1  # set ref level to -1's.
#   return(att.mat)
# }

convert_to_effectcodes <- function(df){
  result_df <- list()
  for (i in 1:ncol(df)) {
    df_i <- dummy.data.frame(data = as.data.frame(df[,i]),names = names(df[,i]))
    df_i[df_i[,1]==1,] <- -1
    df_i[,1] <- NULL
    result_df[[i]] <- df_i
  }
  result_df %>% reduce(bind_cols)
}

plot_runid_bwplots <- function(HBMNL_obj, run_id){
  betas_from_draws <- as_tibble(HBMNL_obj$betadraw[,,run_id])
  names(betas_from_draws) <- names(xdf)
  betas_from_draws %>%
    tibble::rownames_to_column(var = 'id') %>%
    reshape2::melt(id = 'id') %>%
    lattice::bwplot(value~variable,.,panel=function(x,y,...){panel.bwplot(x,y,...);panel.abline(h = 0,col = 'darkgray',lty = 2)}, main = paste0('Beta Values for all respondents for simulation run #', run_id), scales = list(x=list(rot=45)))
}

plot_respondent_runcharts <- function(HBMNL_obj, respondent_id, plottype = 1:2,burnoff = NULL){
  respondent_id_level_beta_runchart <- as_tibble(t(HBMNL_obj$betadraw[respondent_id,,]))
  names(respondent_id_level_beta_runchart) <- names(xdf)
  respondent_id_level_beta_runchart %<>%
    tibble::rownames_to_column(var = 'id') %>%
    reshape2::melt(id = 'id') %>%
    mutate(id = as.numeric(id))
  if(1 %in% plottype){
    print(xyplot(value~id|variable,respondent_id_level_beta_runchart,type='l',
                 panel=function(...){panel.abline(h=0,col='gray',lty=1);panel.xyplot(...)},
                 main = paste0('Beta estimation run chart for Respondent ',respondent_id)))
  }
  if(2 %in% plottype){
    if(is.null(burnoff)){
      nrows <- max(respondent_id_level_beta_runchart$id)
      burnoff <- (0.8*nrows):nrows
    }
    densityplot(~value|variable,
                respondent_id_level_beta_runchart[respondent_id_level_beta_runchart$id %in% burnoff,],plot.points=F,
                panel=function(...){
                  panel.abline(v=0,col='gray',lty=1);
                  panel.densityplot(...)},
                main = paste0('Density Plot for Respondent ',respondent_id))
  }
}
