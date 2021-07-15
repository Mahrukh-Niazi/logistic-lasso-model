library(tidyverse)
library(tidymodels)

logistic_lasso_IRLS <- function(mode = "classification", penalty) {
  ## logistic_lasso_IRLS function is used to register the model that will be added to the tidymodels universe by 
  ## specifying the mode the model is working in (classification) and the penalty. These arguments are needed to make 
  ## the algorithm to work.
  ##
  ## Input:
  ## - mode: specify "classification" mode
  ## - penalty: lambda penalty term 
  ##
  ## Output:
  ## - No output.
  ## 
  ## Example:
  ## library(tidyverse)
  ## library(tidymodels)
  ##
  ## IRLS <- logistic_lasso_IRLS(mode = "classification", penalty = 0.1)
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  args <- list(penalty = rlang::enquo(penalty)) 
  new_model_spec("logistic_lasso_IRLS", 
                 args = args,
                 mode = mode,
                 eng_args = NULL,
                 method = NULL,
                 engine = NULL)
  }

set_new_model("logistic_lasso_IRLS")
set_model_mode(model = "logistic_lasso_IRLS", 
               mode = "classification")
set_model_engine("logistic_lasso_IRLS", 
                 mode = "classification",
                 eng = "fit_logistic_lasso")

set_dependency("logistic_lasso_IRLS", 
               eng = "fit_logistic_lasso", 
               pkg = "base")

set_model_arg( 
  model = "logistic_lasso_IRLS",
  eng = "fit_logistic_lasso",
  parsnip = "penalty", 
  original = "lambda", 
  func = list(pkg = "dials", fun = "penalty"), 
  has_submodel = FALSE 
  )

set_encoding(
  model = "logistic_lasso_IRLS",
  eng = "fit_logistic_lasso",
  mode = "classification",
  options = list(predictor_indicators = "traditional", 
                 compute_intercept = TRUE, 
                 remove_intercept = TRUE, 
                 allow_sparse_x = FALSE)
  )


set_fit(
  model = "logistic_lasso_IRLS",
  eng = "fit_logistic_lasso",
  mode = "classification",
  value = list(interface = "matrix",
               protect = c("x", "y"),
               func = c(fun = "fit_logistic_lasso"),
               defaults = list()
               )
  )

set_pred(
  model = "logistic_lasso_IRLS",
  eng = "fit_logistic_lasso",
  mode = "classification",
  type = "class",
  value = list(pre = NULL,post = NULL,func = c(fun = "predict_logistic_lasso"),
               args = list(object = expr(object$fit),
                           new_x = expr(as.matrix(new_data[, names(object$fit$beta)])))
               )
  )

# update function. It basically creates a new spec that has the final parameter:
update.logistic_lasso_IRLS <- function(object, penalty = NULL, ...) {
  ## update.logistic_lasso_IRLS creates a new model specification with the finalised parameter.
  ##
  ## Inputs:
  ## - object: outcome of fitting function stored as a list
  ## - penalty: lambda penalty parameter
  ##
  ## Output:
  ## - No output.
  ##
  ## Example:
  ## library(tidyverse)
  ## library(tidymodels)
  ##
  ## object <- logistic_lasso_IRLS(mode = "classification", penalty = 0.1)
  ## updateIRLS <- update.logistic_lasso_IRLS(object$fit, penalty = 0.1)
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  if(! is.null(penalty)) {
    object$args <- list(penalty = enquo(penalty))
  }
  new_model_spec("logistic_lasso_IRLS", 
                 args = object$args, 
                 eng_args = NULL,
                 mode = "classification", 
                 method = NULL, 
                 engine = object$engine
                 )
}

