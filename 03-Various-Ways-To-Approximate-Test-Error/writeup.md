    library('MASS') ## for 'mcycle'
    library('manipulate') ## for 'manipulate'
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

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
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## x dplyr::select() masks MASS::select()

    library(ggplot2)


    y_ini <- mcycle$accel
    x_ini <- matrix(mcycle$times, length(mcycle$times), 1)

    plot(x_ini, y_ini, xlab="Time (ms)", ylab="Acceleration (g)")

![](writeup_files/figure-markdown_strict/overview%20of%20the%20data-1.png)

    set.seed(1)
    size<-nrow(mcycle)
    splitRatio<-0.25
    indeX<-sample(1:size)
    validata<-mcycle[indeX[1:round(size*splitRatio)],]
    traindata<-mcycle[indeX[(round(size*splitRatio)+1):size],]

    ## Epanechnikov kernel function
    ## x  - n x p matrix of training inputs
    ## x0 - 1 x p input where to make prediction
    ## lambda - bandwidth (neighborhood size)
    kernel_epanechnikov <- function(x, x0, lambda=1) {
      d <- function(t)
        ifelse(t <= 1, 3/4*(1-t^2), 0)
      z <- t(t(x) - x0)
      d(sqrt(rowSums(z*z))/lambda)
    }

    ## k-NN kernel function
    ## x  - n x p matrix of training inputs
    ## x0 - 1 x p input where to make prediction
    ## k  - number of nearest neighbors
    kernel_k_nearest_neighbors <- function(x, x0, k=1) {
      ## compute distance betwen each x and x0
      z <- t(t(x) - x0)
      d <- sqrt(rowSums(z*z))

      ## initialize kernel weights to zero
      w <- rep(0, length(d))
      
      ## set weight to 1 for k nearest neighbors
      w[order(d)[1:k]] <- 1
      
      return(w)
    }

    ## Make predictions using the NW method
    ## y  - n x 1 vector of training outputs
    ## x  - n x p matrix of training inputs
    ## x0 - m x p matrix where to make predictions
    ## kern  - kernel function to use
    ## ... - arguments to pass to kernel function
    nadaraya_watson <- function(y, x, x0, kern, ...) {
      smoother <- t(apply(x0, 1, function(x0_) {
        k_ <- kern(x, x0_, ...)
        k_/sum(k_)
      }))
      yhat <- drop(smoother %*% y)
      attr(yhat, 'k') <- smoother
      return(yhat)
    }

    ## Helper function to view kernel (smoother) matrix
    matrix_image <- function(x) {
      rot <- function(x) t(apply(x, 2, rev))
      cls <- rev(gray.colors(20, end=1))
      image(rot(x), col=cls, axes=FALSE)
      xlb <- pretty(1:ncol(x))
      xat <- (xlb-0.5)/ncol(x)
      ylb <- pretty(1:nrow(x))
      yat <- (ylb-0.5)/nrow(x)
      axis(3, at=xat, labels=xlb)
      axis(2, at=yat, labels=ylb)
      mtext('Rows', 2, 3)
      mtext('Columns', 3, 3)
    }

    ## Compute effective df using NW method
    ## y  - n x 1 vector of training outputs
    ## x  - n x p matrix of training inputs
    ## kern  - kernel function to use
    ## ... - arguments to pass to kernel function
    effective_df <- function(y, x, kern, ...) {
      y_hat <- nadaraya_watson(y, x, x,
        kern=kern, ...)
      sum(diag(attr(y_hat, 'k')))
    }

    ## loss function
    ## y    - train/test y
    ## yhat - predictions at train/test x
    loss_squared_error <- function(y, yhat)
      (y - yhat)^2

    ## test/train error
    ## y    - train/test y
    ## yhat - predictions at train/test x
    ## loss - loss function
    error <- function(y, yhat, loss=loss_squared_error)
      mean(loss(y, yhat))

    ## AIC
    ## y    - training y
    ## yhat - predictions at training x
    ## d    - effective degrees of freedom
    aic <- function(y, yhat, d)
      error(y, yhat) + 2/length(y)*d

    ## BIC
    ## y    - training y
    ## yhat - predictions at training x
    ## d    - effective degrees of freedom
    bic <- function(y, yhat, d)
      error(y, yhat) + log(length(y))/length(y)*d

    y <- traindata$accel
    x <- matrix(traindata$times, length(traindata$times), 1)
    x_plot <- matrix(seq(min(x),max(x),length.out=100),100,1)


    k_set<-seq(1,100,20)
    for (k_slider in k_set){
      y_hat <- nadaraya_watson(y, x, x,
         kern=kernel_k_nearest_neighbors, k=k_slider)
      y_hat_plot <- nadaraya_watson(y, x, x_plot,
        kern=kernel_k_nearest_neighbors, k=k_slider)
      edf <- effective_df(y, x, kern=kernel_k_nearest_neighbors, k=k_slider)
    aic_error<-aic(y, y_hat, edf)
    bic_error<-bic(y, y_hat, edf)
    train_error<-error(y,y_hat)
    y_val<-validata$accel
    x_val<-matrix(validata$times, length(validata$times), 1)
    y_hat_val<-nadaraya_watson(y, x, x_val,kern=kernel_k_nearest_neighbors, k=k_slider)
    vali_error<-error(y_val,y_hat_val)
      plot(x, y, xlab="Time (ms)", ylab="Acceleration (g)",main='How k changes the shape of k-NN model')
        legend('topright', legend = c(
        paste0('k = ', k_slider),
        paste0('training error = ', round(train_error,1)),
        paste0('aic = ', round(aic_error, 1)),
        paste0('bic = ', round(bic_error, 1)),
        paste0('validation error = ', round(vali_error, 1))),
        bty='n')
      lines(x_plot, y_hat_plot, col="#882255", lwd=2) 
    }

![](writeup_files/figure-markdown_strict/prediction%20by%20k-NN%20kernal%20with%20varying%20k-1.png)![](writeup_files/figure-markdown_strict/prediction%20by%20k-NN%20kernal%20with%20varying%20k-2.png)![](writeup_files/figure-markdown_strict/prediction%20by%20k-NN%20kernal%20with%20varying%20k-3.png)![](writeup_files/figure-markdown_strict/prediction%20by%20k-NN%20kernal%20with%20varying%20k-4.png)![](writeup_files/figure-markdown_strict/prediction%20by%20k-NN%20kernal%20with%20varying%20k-5.png)

    # manipulate({
    #   y_hat <- nadaraya_watson(y, x, x,
    #      kern=kernel_k_nearest_neighbors, k=k_slider)
    #   y_hat_plot <- nadaraya_watson(y, x, x_plot,
    #     kern=kernel_k_nearest_neighbors, k=k_slider)
    #   edf <- effective_df(y, x, kern=kernel_k_nearest_neighbors, k=k_slider)
    # aic_error<-aic(y, y_hat, edf)
    # bic_error<-bic(y, y_hat, edf)
    # train_error<-error(y,y_hat)
    # y_val<-validata$accel
    # x_val<-matrix(validata$times, length(validata$times), 1)
    # y_hat_val<-nadaraya_watson(y, x, x_val,kern=kernel_k_nearest_neighbors, k=k_slider)
    # vali_error<-error(y_val,y_hat_val)
    #   plot(x, y, xlab="Time (ms)", ylab="Acceleration (g)",main='How k changes the shape of k-NN model')
    #     legend('topright', legend = c(
    #     paste0('training error = ', round(train_error,1)),
    #     paste0('aic = ', round(aic_error, 1)),
    #     paste0('bic = ', round(bic_error, 1)),
    #     paste0('validation error = ', round(vali_error, 1))),
    #     bty='n')
    #   lines(x_plot, y_hat_plot, col="#882255", lwd=2) 
    #   
    # }, k_slider=slider(1, 100, initial=1, step=1))

We can see that as k-NN’s increase, the model becomes smoother and
simpler. So the less the k is, the model is tending to overfitting.

    error_when_tuning<-function(training_dat,val_dat,tune_para){
    ## make predictions using k-NN method at training inputs
    y <- training_dat$accel
    x <- matrix(training_dat$times, length(training_dat$times), 1)
    y_hat <- nadaraya_watson(y, x, x,
      kern=kernel_k_nearest_neighbors, k=tune_para)
    ## compute effective degrees of freedom
    edf <- effective_df(y, x, kern=kernel_k_nearest_neighbors, k=tune_para)
    aic_error<-aic(y, y_hat, edf)
    bic_error<-bic(y, y_hat, edf)
    train_error<-error(y,y_hat)
    ## make predictions using k-NN method at validation inputs
    y_val<-val_dat$accel
    x_val<-matrix(val_dat$times, length(val_dat$times), 1)
    y_hat_val<-nadaraya_watson(y, x, x_val,kern=kernel_k_nearest_neighbors, k=tune_para)
    vali_error<-error(y_val,y_hat_val)
    return (c('aic'=aic_error,'bic'=bic_error,'train_error'=train_error,'vali_error'=vali_error))
    }

    k_tune<-1:20
    for (i in seq_along(k_tune)){
      errors<-error_when_tuning(traindata,validata,i)
      if (i==1){
        tune_vector<-errors
      }else{
        tune_vector<-tune_vector %>% rbind(errors)
      }
    }
    tune_df<-tune_vector %>% as.data.frame()%>% mutate(k=k_tune) %>% pivot_longer(1:4,names_to = 'error_type')

    tune_df %>% ggplot(aes(x=k,y=value,group=error_type))+geom_line(aes(group=error_type,col=error_type))+labs(title = 'AIC,BIC,training error, validation error changes over k')

![](writeup_files/figure-markdown_strict/make%20graph%20of%20various%20error%20changing%20with%20varying%20k-1.png)

We can see that as k increases, the training error goes up because the
bias becomes bigger when more neighbors are taken into account. Test
error goes down as k increases because the model is less flexible when
more neighbors are taken into account.

    #randomly split data into 5 groups
    set.seed(390)
    folddata<-mcycle %>% mutate(group=sample(rep(1:5,round(133/5))[1:133],133))

    error_by_fold<-function(fdata,tune_para){
      test_errors<-rep(NA,5)
      for (i in 1:5){
        valdat<-fdata %>% filter(group==i)
      traindat<-fdata %>% filter(group!=i)
      y<- traindat$accel
      x<- matrix(traindat$times, length(traindat$times), 1)
      y_val <- valdat$accel
      x_val <- matrix(valdat$times, length(valdat$times), 1)
      y_hat_val<-nadaraya_watson(y, x, x_val,kern=kernel_k_nearest_neighbors, k=tune_para)
      test_errors[i]<-error(y_val,y_hat_val)
      }
      return(c(CV_estimated=mean(test_errors),std_error=sd(test_errors),k=tune_para))
    }

    for (k in 1:20){
      ferrors<-error_by_fold(folddata,k)
      if (k==1){
        ftune_vector<-ferrors
      }else{
        ftune_vector<-ftune_vector %>% rbind(ferrors)
      }
    }
    ftune_df<-ftune_vector %>% as.data.frame()%>% mutate(top=CV_estimated+std_error,bottom= CV_estimated-std_error)
    plot(data=ftune_df,CV_estimated~k,ylim=c(min(CV_estimated)-200,max(CV_estimated)+600),
         main='Test error estimated by 5-fold CV changes over k')
    segments(ftune_df$k,ftune_df$top,ftune_df$k,ftune_df$bottom)
    hline<-(ftune_df %>% arrange(CV_estimated) %>% head(1) )$top[[1]]
    abline(h=hline,lty=2)

![](writeup_files/figure-markdown_strict/5-fold%20cross%20validation-1.png)

According to the result, we can see when k=8, test error is the least.
And according to one standard error rule, we want a simplest model of
which the test error is no more than (the lest test error+one standard
error).And since this is k-NN method, the more neighbors we choose, the
simpler the model is. Hence, from the horizontal dashed line, we could
see the simplest model with test error under (the lest test error+one
standard error) is at k=18. Finally, we choose k=18 .
