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
