    library(ElemStatLearn)
    library(randomForest)

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(tidyverse)

    ## ── Attaching packages ──────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ stringr 1.4.0
    ## ✓ tidyr   1.0.0     ✓ forcats 0.4.0
    ## ✓ readr   1.3.1

    ## ── Conflicts ─────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::combine()  masks randomForest::combine()
    ## x dplyr::filter()   masks stats::filter()
    ## x dplyr::lag()      masks stats::lag()
    ## x ggplot2::margin() masks randomForest::margin()

    library(caret)

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

Convert the response variable in the “vowel.train” data frame to a
factor variable prior to training, so that “randomForest” does
classification rather than regression.

    vowel.train<-vowel.train %>% mutate(y=as.factor(y))

Fit the random forest model to the vowel data using all of the 11
features using the default values of the tuning parameters.

    set.seed(71)
    vowel.rf <- randomForest(y ~ ., data=vowel.train, importance=TRUE,
                            proximity=TRUE)

se 5-fold CV and tune the model by performing a grid search for the
following tuning parameters: 1) the number of variables randomly sampled
as candidates at each split; consider values 3, 4, and 5, and 2) the
minimum size of terminal nodes; consider a sequence (1, 5, 10, 20, 40,
and 80).

    set.seed(23)
    f<-createFolds(vowel.train$y, k=5)

    misclass_rate<-function(prediction_vactor,true_vector) mean(prediction_vactor!=true_vector)

    CV_error<-function(fold_list=f,m,nsize){
      test_errors<-rep(NA,length(fold_list))
      for (i in 1:length(fold_list)){
        valdat<-vowel.train[fold_list[[i]],]
      traindat<-vowel.train[-fold_list[[i]],]
      set.seed(1)
     vowel.rf<-randomForest(y ~ ., data=traindat, importance=TRUE,
                            proximity=TRUE,mtry=m,nodesize=nsize)
       # vowel.rf<-randomForest(y ~ ., data=traindat, mtry=m,nodesize=nsize)
     predicts<-predict(vowel.rf,valdat[,-1])
      test_errors[i]<-misclass_rate(predicts,valdat$y)
      }
      return(c(CV_estimated=mean(test_errors),std_error=sd(test_errors),mtry=m,nodesize=nsize))
    }



    tune_para<-expand.grid(m_split=3:5,min_nsize= c(1, 5, 10, 20, 40,80))

    tune_out<-CV_error(m=tune_para$m_split[1],nsize=tune_para$min_nsize[1])
    for (i in 2:nrow(tune_para)){
      out<-CV_error(m=tune_para$m_split[i],nsize=tune_para$min_nsize[i])
      tune_out<-rbind(tune_out,out)
    }

    tune_out<-tune_out %>% as.data.frame() 
    m_best<-(tune_out %>% filter(CV_estimated==min(CV_estimated)))$mtry
    n_best<-(tune_out %>% filter(CV_estimated==min(CV_estimated)))$nodesize

With the tuned model, make predictions using the majority vote method,
and compute the misclassification rate using the ‘vowel.test’ data.

    set.seed(1)
    vowel.rf_best<-randomForest(y ~ ., data=vowel.train, importance=TRUE,
                            proximity=TRUE,mtry=m_best,nodesize=n_best)

    misclass_rate(predict(vowel.rf_best,vowel.test[,-1]),vowel.test$y)

    ## [1] 0.3939394
