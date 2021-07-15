fit_logistic_lasso <- function(x, y, lambda, beta0 = NULL, eps = 0.0001, max_iter = 100) {
  ## fit_logistic_lasso computes the logistic Lasso using coordinate descent on the penalised iteratively reweighted
  ## least squares algorithm on normalised data.
  ##
  ## Input:
  ## - x: matrix of predictors (not including the intercept) that has been standardised
  ## - y: vector of target data  
  ## - lambda: numeric penalization parameter
  ## - beta0: vector of initial values of beta (initial guess) or NULL as default
  ## - eps: numeric parameter for stopping critereon
  ## - max_iter: maximum number of iterations
  ##
  ## Output:
  ## - A large list of six elements: a boolean variable for if convergence was successful (TRUE or FALSE), the 
  ##   final iteration number, list of beta coefficient estimates, intercept coefficient estimate, the lambda value,
  ##   and the factor levels associated with the target vector data y.
  ##
  ## Example:
  ## library(tidyverse)
  ## library(tidymodels)
  ##
  ## n <- 1000
  ## x = seq(-3,3, length.out = n)
  ## w = 3*cos(3*seq(-pi,pi, length.out = n))
  ## y = rbinom(n,size = 1, prob = 1/(1 + exp(-w+2*x)) )%>% as.numeric %>% factor
  ## ret1 <- fit_logistic_lasso(x, y, lambda = 0.3, beta0 = NULL, esp = 0.0001, max_iter = 100)
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  
  n <- dim(x)[1]
  m <- dim(x)[2]
  
  # Register beta0:
  if (!is.null(beta0)) {
    beta <- beta0
  } 
  else{
    beta0 <- rep(0, m)
    beta <- beta0
  }
  
  ## Process the factor to be 0/1
  ## Make sure you save the names of the factor levels so we can
  ## use them in the predictions
  fct_levels <- levels(y)
  y <- as.numeric(y) - 1
  
  x_beta0 <- (x %*% beta) %>% as.numeric
  p <- 1/(1 + exp(-x_beta0))

  intercept0 <- 0 
  # Looping until max number of iterations
  for(iter in 1:max_iter) {
    w <- p * (1 - p)
    z <- x_beta0 + (y - p)/w #compute z
    r <- z - x %*% beta 
    intercept <- sum(w*r)/sum(w) #computing intercept
    
    # Looping through each coordinate
    for (j in 1:m) {
      rj <- z - intercept - x[,-j] %*% beta[-j] #computing rj for coordinate descent
      
      if (t(x[,j])%*%(rj*w) > 0){ #coordinate descent condition 1
        beta[j] <- max((abs(t(x[,j])%*%(rj*w)) - (n*lambda)), 0) / sum(w*(x[,j])^2)
      }
      else if (t(x[,j])%*%(rj*w) < 0){ #coordinate descent condition 2
        beta[j] <- - max((abs(t(x[,j])%*%(rj*w)) - (n*lambda)), 0) / sum(w*(x[,j])^2) 
      }
    }
    
    x_beta0 <- x %*% beta %>% as.numeric #update x_beta0
    p1 <- 1/(1 + exp(-x_beta0))  #update p1
    names(beta) <- colnames(x) #name the betas
    
    # Convergence check:
    if ((max(abs(intercept - intercept0), max(abs(beta - beta0)))) < eps) {
      return(list(intercept=intercept, beta = beta,
                  lambda = lambda,converged = TRUE,
                  fact_levels = fct_levels,
                  iter = iter))
    }
    beta0 <- beta # keep track of the old beta for convergence check
    intercept0 <- intercept #keep track of the old intercept for convergence check
  }
  return(list(intercept=intercept, beta = beta,
              lambda = lambda,converged = FALSE,
              fact_levels = fct_levels,
              iter = iter))
  
}


predict_logistic_lasso <- function(object, new_x) {
  ## predict_logistic_lasso predicts the fitted values of the logistic regression. The logistic regression is given
  ## by the following equation:
  ##
  ## $
  ## 
  ## Input:
  ## - object: the output of fit_logistic_lasso: a large list of six elements: a boolean variable for if convergence 
  ##           was successful (TRUE or FALSE), the final iteration number, list of beta coefficient estimates derived 
  ##           from a logistic lasso model using coordinate descent on the penalised iteratively reweighted least
  ##           squares algorithm, intercept coefficient estimate, the lambda value, and the factor levels associated
  ##           with the target vector of data y.
  ##
  ## - new_x: matrix of new values of x to make predictions at 
  ##
  ## Output:
  ## - A list containing the intercept and beta estimates.
  ##
  ## Example:
  ## library(tidyverse)
  ## library(tidymodels)
  ##
  ## ret2 <- predict_logistic_lasso(object = ret1, x_new)
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  # Add intercept to XBeta:
  numeric_pred <- (object$intercept + (new_x %*% object$beta) >= 0) %>% as.numeric
  
  return(object$fact_levels[numeric_pred + 1] %>% factor)
}

  

