cv_ng<- function(mod, dat, biom, seed=NULL, cluster=TRUE, min_clust_size){
  #mod is the linear mixed effects model of interest, fit by lme()
  #dat is the data frame containing the data which is used to fit the model
  #biom is a string which is the outcome variable of the mixed effects model
  #set seed with seed
  #leave cluster=TRUE
  #min_clust_size is the minimum number of observations in a cluster you wish to include.
  # If there are any subjects with one observation, min_clust_size must be >=2. Can modify
  # commented out code below to accommodate min_clust_size >2.
  
  #cv_ng() uses mean square error as a cross validation metric. Other metrics available in
  # the "cv" package can be substituted.
  
  library(nlme)
  library(cv)
  library(splines)
  lme_params<- lmeControl(maxIter=5000, msMaxIter=5000, msMaxEval=5000, opt="optim", optimMethod="L-BFGS-B", allow.n.lt.q = TRUE)
  if(!is.null(seed)){set.seed(seed)}
  
  dat<- dat %>% group_by(sid) %>% filter(n() >= min_clust_size)
  
  if(isTRUE(cluster)){
    dat$k_clust<- NA
    for(i in unique(dat$sid)){
      rand_int<- sample(c(1,2), size=1)
      rand_int_II<- sample(c(1,2), size=1)
      rand_int_III<- sample(1:3, size=1)
      rand_int_IV<- sample(1:4, size=1)
      sid_len<- length(which(dat$sid == i))
      if(sid_len == 2 & rand_int == 1){dat$k_clust[which(dat$sid == i)]<- c(1,2)}
      if(sid_len == 2 & rand_int == 2){dat$k_clust[which(dat$sid == i)]<- c(2,1)}
      if(sid_len == 3 & rand_int_II == 1){dat$k_clust[which(dat$sid == i)]<- sample(c(1,1,2), size=3)}
      if(sid_len == 3 & rand_int_II == 2){dat$k_clust[which(dat$sid == i)]<- sample(c(1,2,2), size=3)}
      if(sid_len == 4 & rand_int_II == 1){dat$k_clust[which(dat$sid == i)]<- sample(c(1,1,1,2), size=4)}
      if(sid_len == 4 & rand_int_II == 2){dat$k_clust[which(dat$sid == i)]<- sample(c(1,1,2,2), size=4)}
      if(sid_len == 4 & rand_int_II == 3){dat$k_clust[which(dat$sid == i)]<- sample(c(1,2,2,2), size=4)}
      if(sid_len == 5 & rand_int_IV == 1){dat$k_clust[which(dat$sid == i)]<- sample(c(1,1,1,1,2), size=5)}
      if(sid_len == 5 & rand_int_IV == 2){dat$k_clust[which(dat$sid == i)]<- sample(c(1,1,1,2,2), size=5)}
      if(sid_len == 5 & rand_int_IV == 3){dat$k_clust[which(dat$sid == i)]<- sample(c(1,1,2,2,2), size=5)}
      if(sid_len == 5 & rand_int_IV == 4){dat$k_clust[which(dat$sid == i)]<- sample(c(1,2,2,2,2), size=5)}
    }
  }
  
  if(isTRUE(cluster)==FALSE){dat$k_clust<- sample(1:5, size=nrow(dat), replace=TRUE)}
  
  dat_1_train<- dat %>% filter(k_clust == 2)
  dat_1_test<- dat %>% filter(k_clust == 1)
  mod1<- update(mod, data=dat_1_train)
  eval(parse(text=paste0("mse_train_1<- mse(dat_1_train$",biom, ", predict(mod1, dat_1_train))")))
  eval(parse(text=paste0("mse_test_1<- mse(dat_1_test$", biom, ", predict(mod1, dat_1_test))")))
  
  dat_2_train<- dat %>% filter(k_clust == 1)
  dat_2_test<- dat %>% filter(k_clust == 2)
  mod2<- update(mod, data=dat_2_train)
  eval(parse(text=paste0("mse_train_2<- mse(dat_2_train$", biom, ", predict(mod2, dat_2_train))")))
  eval(parse(text=paste0("mse_test_2<- mse(dat_2_test$", biom, ", predict(mod2, dat_2_test))")))
  
  #dat_3_train<- dat %>% filter(k_clust %in% c(1,2,4,5))
  #dat_3_test<- dat %>% filter(k_clust == 3)
  #mod3<- update(mod, data=dat_3_train)
  #eval(parse(text=paste0("mse_train_3<- mse(dat_3_train$", biom, ", predict(mod3, dat_3_train))")))
  #eval(parse(text=paste0("mse_test_3<- mse(dat_3_test$", biom, ", predict(mod3, dat_3_test))")))
  
  #dat_4_train<- dat %>% filter(k_clust %in% c(1,2,3,5))
  #dat_4_test<- dat %>% filter(k_clust == 4)
  #mod4<- update(mod, data=dat_4_train)
  #eval(parse(text=paste0("mse_train_4<- mse(dat_4_train$", biom, ", predict(mod4, dat_4_train))")))
  #eval(parse(text=paste0("mse_test_4<- mse(dat_4_test$", biom, ", predict(mod4, dat_4_test))")))
  
  #dat_5_train<- dat %>% filter(k_clust %in% c(1,2,3,4))
  #dat_5_test<- dat %>% filter(k_clust == 5)
  #mod5<- update(mod, data=dat_5_train)
  #eval(parse(text=paste0("mse_train_5<- mse(dat_5_train$", biom, ", predict(mod5, dat_5_train))")))
  #eval(parse(text=paste0("mse_test_5<- mse(dat_5_test$", biom, ", predict(mod5, dat_5_test))")))
  
  #mse_train<- c(mse_train_1, mse_train_2, mse_train_3, mse_train_4, mse_train_5)
  #mse_test<- c(mse_test_1, mse_test_2, mse_test_3, mse_test_4, mse_test_5)
  
  mse_train<- c(mse_train_1, mse_train_2)
  mse_test<- c(mse_test_1, mse_test_2)
  
  return(list("mse_train"=mse_train, "mse_test"=mse_test, 
              "mean_mse_train"=mean(mse_train), "mean_mse_test"=mean(mse_test),
              "n_cluster" = table(dat$k_clust),
              "n_sid" = table(table(dat$sid))
              #"pred_1" = predict(mod1, dat_1_test, level=1),
              #"pred_2" = predict(mod2, dat_2_test, level=1)
  ))
}