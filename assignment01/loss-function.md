Introduction
============

In mathematical optimization and decision theory, a loss function or
cost function is a function that maps an event or values of one or more
variables onto a real number intuitively representing some “cost”
associated with the event. An optimization problem seeks to minimize a
loss function.

Frequently used loss functions are L1 loss and L2 loss function.L1 Loss
function stands for Least Absolute Deviations. Also known as LAD. L2
Loss function stands for Least Square Errors. Also known as LS.
Especially, we have titled loss function to adjust the extent of
penalizing.

In this blog, we try linear and non-linear model to fit the data, with
L1, L2 and tilted L1 loss function.

Linear Model
============

    library(ElemStatLearn)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## load prostate data
    data("prostate")
    ## subset to training examples
    prostate_train <- subset(prostate, train==TRUE)

    ## plot lcavol vs lpsa
    plot_psa_data <- function(dat=prostate_train) {
      plot(dat$lpsa, dat$lcavol,
           xlab="log Prostate Screening Antigen (psa)",
           ylab="log Cancer Volume (lcavol)")
    }

    #absolute error
    L1_loss<- function(y, yhat,tau=0.5){
      case_when(
        (y-yhat)>0~tau*(y-yhat),
        T~(tau-1)*(y-yhat)
      )
    }

    #squred error
    L2_loss <- function(y, yhat,tau=0.5){
      case_when(
        (y-yhat)>0~tau*(y-yhat)*(y-yhat),
        T~(1-tau)*(y-yhat)*(y-yhat)
      )
    }

    ## fit simple linear model using numerical optimization
    fit_lin <- function(y, x, loss=L2_loss, beta_init = c(-0.51, 0.75),tau=0.5) {#try different initial value and compare their accuracy 
      err <- function(beta,taue=tau)
        mean(loss(y,  beta[1] + beta[2]*x,taue))# training error
      beta <- optim(par = beta_init, fn = err)
      #A function to be minimized (or maximized), with first argument the vector of parameters over which minimization is to take place. It should return a scalar result.
      return(beta)
    }

    ## make predictions from linear model
    predict_lin <- function(x, beta) beta[1] + beta[2]*x

    ## compute predictions for a grid of inputs
    x_grid <- seq(min(prostate_train$lpsa),
                  max(prostate_train$lpsa),
                  length.out=100)

    # l2 loss
    lin_beta_2 <- fit_lin(y=prostate_train$lcavol,
                        x=prostate_train$lpsa,
                        loss=L2_loss)
    lin_pred_2 <- predict_lin(x=x_grid, beta=lin_beta_2$par)

    # L1 loss

    lin_beta_1 <- fit_lin(y=prostate_train$lcavol,
                        x=prostate_train$lpsa,
                        loss=L1_loss)
    lin_pred_1 <- predict_lin(x=x_grid, beta=lin_beta_1$par)

    # loss=l1,tau=0.25

    lin_beta_tilted_25 <- fit_lin(y=prostate_train$lcavol,
                        x=prostate_train$lpsa,
                        loss=L1_loss,
                        tau=0.25)
    lin_pred_tilted_25 <- predict_lin(x=x_grid, beta=lin_beta_tilted_25$par)

    #loss=l1,tau=0.75

    lin_beta_tilted_75 <- fit_lin(y=prostate_train$lcavol,
                        x=prostate_train$lpsa,
                        loss=L1_loss,
                        tau=0.75)
    lin_pred_tilted_75 <- predict_lin(x=x_grid, beta=lin_beta_tilted_75$par)


    ## plot data
    plot_psa_data()
    ## plot predictions
    lines(x=x_grid, y=lin_pred_1,col='red')
    lines(x=x_grid, y=lin_pred_2,col='orange')
    lines(x=x_grid, y=lin_pred_tilted_25,col='green')
    lines(x=x_grid, y=lin_pred_tilted_75,col='blue')
    legend(4,1.3,legend = c('L1 loss','L2 loss','tilted(tau=0.25)','tilted(tau=0.75)'),lty=1:1,col=c('red','orange','green','blue'),title='method',cex=0.8)
    title(main='linear model fitting')

![](loss-function_files/figure-markdown_strict/linear%20fit-1.png)

Non-linear Model
================

    ## fit simple non-linear model using numerical optimization
    fit_exp <- function(y, x, loss=L2_loss, beta_init = c(-1.0, 0.0, -0.3),tau=0.5) {
      err <- function(beta,taue=tau)
        mean(loss(y,  beta[1] + beta[2]*exp(-beta[3]*x),taue))# training error
      beta <- (optim(par = beta_init, fn = err))
      return(beta)
    }

    ## make predictions from non-linear model
    model_exp <- function(beta){
      beta<-beta[[1]]
       exp_model<-function(x){
        beta[1] + beta[2]*exp(-beta[3]*x)
      }
      return(exp_model)
    }

    # l2 loss
    exp_beta_2 <- fit_exp(y=prostate_train$lcavol,
                        x=prostate_train$lpsa,
                        loss=L2_loss)

    exp_beta_2 %>% model_exp->exp_model2

    #l1 loss
    exp_beta_1 <- fit_exp(y=prostate_train$lcavol,
                        x=prostate_train$lpsa,
                        loss=L1_loss)
    exp_beta_1 %>% model_exp->exp_model1

    #l1 loss,tau=0.25
    exp_beta_tilt_25 <- fit_exp(y=prostate_train$lcavol,
                        x=prostate_train$lpsa,
                        loss=L1_loss,
                        tau=0.25)
    exp_beta_tilt_25 %>% model_exp->exp_model_tilt25


    #l1 loss,tau=0.75
    exp_beta_tilt_75 <- fit_exp(y=prostate_train$lcavol,
                        x=prostate_train$lpsa,
                        loss=L1_loss,
                        tau=0.75)
    exp_beta_tilt_75 %>% model_exp->exp_model_tilt75

    ## plot data
    plot_psa_data()
    ## plot predictions
    curve(exp_model1,from=0,to=15,add=T,col='red')
    curve(exp_model2,from=0,to=15,add=T,col='orange')
    curve(exp_model_tilt25,from=0,to=15,add=T,col='green')
    curve(exp_model_tilt75,from=0,to=15,add=T,col='blue')
    legend(4,1.3,legend = c('L1 loss','L2 loss','tilted(tau=0.25)','tilted(tau=0.75)'),lty=1:1,col=c('red','orange','green','blue'),title='method',cex=0.8)
    title(main='non-linear model fitting')

![](loss-function_files/figure-markdown_strict/non-linear%20fit-1.png)

·
