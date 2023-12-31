#' List GLM models
#'
#' Function used to create list of glm objects of class 'listGLM'
#'
#' @param ... one or more fitted `glm` model objects of family `binomial`.
#' @keywords list glm
#' @examples
#' fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
#' fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
#' x <- listGLM(fit_glm1, fit_glm2)
#'
#'
#' @export
listGLM <- function(...){
  fittedModels <- list(...)

  # Check each model is a glm object of family binomial
  for(m in fittedModels) {
    if(!("glm" %in% class(m))){ stop(paste("Check that all objects in input are glm objects.")) }
    if(!("binomial" %in% m$family)) { stop(paste("Check that all glm models in input have family = binomial."))  }
  }

  class(fittedModels) <- 'listGLM'
  return(fittedModels)
}



#' Logistic Regression Coefficients and Confidence Intervals
#'
#' S3 method for class 'listGLM' used to present the model coefficient estimates, standard errors and confidence intervals of one or more `glm` objects in a gt table or data.frame.
#'
#' @param x An object of class 'listGLM' i.e. a list of fitted `glm` model object(s) of family `binomial`.
#' @param exp `logical`. If `TRUE` then exponentiate the coefficients and confidence intervals; if `FALSE` do not exponentiate (default).
#' @param raw `logical`. If `TRUE` then return the values in a `list` of `data.frame` objects, if `FALSE` return the values in a `gt` group object (default).
#' @param ci The confidence interval level required. Default is 0.95.
#' @param ci_normal `logical`. If `TRUE` then calculate the confidence interval based on asymptotic normality; if `FALSE` calculate the profile likelihood confidence interval using `confint` (default).
#' @param sigfig The number of significant figures to round the results to. Default is 6.
#' @param expand `logical`. If `TRUE` then additionally return the z values and p-values from the glm summary object.
#' @keywords coefficients, confidence interval, forest plot
#' @examples
#' fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
#' fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
#' modelList <- listGLM(fit_glm1, fit_glm2)
#' coef(modelList, exp=TRUE)
#'
#' @importFrom gt gt opt_stylize gt_group
#'
#' @export
coef.listGLM <- function(x, exp=F, raw=F, ci=0.95, ci_normal=F, sigfig=6, expand=F){

  # Check inputs
  if(class(x) != "listGLM") {stop(paste("The input is not a listGLM object"))}
  if(!is.logical(exp)){stop(paste("Argument 'exp' must be logical"))}
  if(!is.logical(raw)){stop(paste("Argument 'raw' must be logical"))}
  if(!is.logical(ci_normal)){stop(paste("Argument 'ci_normal' must be logical"))}
  if(!is.logical(expand)){stop(paste("Argument 'expand' must be logical"))}
  if(!is.numeric(ci)){stop(paste("Argument 'ci' must be a numeric value between 0 and 1"))}
  if(ci>=1 || ci < 0){stop(paste("Argument 'ci' must be a numeric value between 0 and 1"))}
  if(!is.numeric(sigfig)){stop(paste("Argument 'sigfig' must be an integer greater than 0"))}
  if(sigfig%%1!=0 || sigfig < 0){stop(paste("Argument 'sigfig' must be an integer greater than 0"))}


  # Get the required coefficient list (data and formulae)
  coef_list <- getCoef(x, exp, ci, ci_normal, sigfig)

  # Return coefficients and CIs only
  if(!expand) {
    coef_list$data <- coef_list$data[,1:6]
  } else {
    coef_list$data <- coef_list$data[,1:8]
  }


  if(!raw){
    # Convert data.frames to ("gt_tbl" "list") objects
    gt_data <- coef_list$data %>%
      gt::gt() %>%
      gt::opt_stylize(style = 1)

    gt_formula <- coef_list$formula %>%
      gt::gt() %>%
      gt::opt_stylize(style = 1)
    # Return gt group object
    return(gt::gt_group(gt_data, gt_formula))
  }

  # Return data.frame object
  return(coef_list)
}



#' Forest Plot of Logistic Regression Coefficient Estimates
#'
#' S3 method for class 'listGLM' used to plot coefficient estimates and confidence intervals in a Forest Plot.
#'
#' @param x An object of class 'listGLM' i.e. a list of fitted `glm` model object(s) of family `binomial`.
#' @param exp `logical`. If `TRUE` then the coefficients are exponentiated, so the x marker is 0; if `FALSE`, x marker is 1.
#' @param ci The confidence interval level required. Default is 0.95.
#' @param ci_normal `logical`. If `TRUE` then calculate the confidence interval based on asymptotic normality; if `FALSE` calculate the profile likelihood confidence interval using `confint` (default).
#' @param sigfig The number of significant figures to round the results to. Default is 6.
#' @keywords forest plot, confidence interval
#' @examples
#' fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
#' fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
#' modelList <- listGLM(fit_glm1, fit_glm2)
#' plot(modelList)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh position_dodge geom_vline labs theme_minimal scale_x_continuous
#'
#' @export
plot.listGLM <- function(x, exp=F, ci=0.95, ci_normal=F, sigfig=6){

  # Check inputs
  if(class(x) != "listGLM") {stop(paste("The input is not a listGLM object"))}
  if(!is.logical(exp)){stop(paste("Argument 'exp' must be logical"))}
  if(!is.logical(ci_normal)){stop(paste("Argument 'ci_normal' must be logical"))}
  if(!is.numeric(ci)){stop(paste("Argument 'ci' must be a numeric value between 0 and 1"))}
  if(ci>=1 || ci < 0){stop(paste("Argument 'ci' must be a numeric value between 0 and 1"))}
  if(!is.numeric(sigfig)){stop(paste("Argument 'sigfig' must be an integer greater than 0"))}
  if(sigfig%%1!=0 || sigfig < 0){stop(paste("Argument 'sigfig' must be an integer greater than 0"))}


  # Get the required coefficient list (data and formulae)
  coef_list <- getCoef(x, exp, ci, ci_normal, sigfig)
  coef_data <- coef_list$data

  # Show vertical line at 0 for log-odds ratio
  v<-0
  xlab <- "Coefficient Estimate"
  # Show vertical line at 1 for odds ratio
  if(exp) {
    v <- 1
    xlab <- "Exponentiated Coefficient Estimate"
  }

  # Create a forest plot with stacked points for each Model
  ggplot2::ggplot(coef_data, aes(y = Variable, x=Estimate, color=Model)) +
    geom_point(position = position_dodge(width=0.4), size=2) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), position = position_dodge(width=0.4),
                   height = 0.2, linewidth=1, alpha=0.75) +
    geom_vline(xintercept = v, linetype = "dashed") +
    labs(title = " Model Coefficients", x =  xlab) +
    theme_minimal() +
    scale_x_continuous(n.breaks=10)

}


#' Get coefficients from GLM models
#'
#' S3 method for class 'listGLM. Get coefficients, std errors, z and p values for each model in 'listGLM' object.
#' Used internally by coef and plot methods.
#'
#' @param fittedModels An object of class 'listGLM' i.e. a list of fitted `glm` model object(s) of family `binomial`.
#' @param exp `logical`. If `TRUE` then exponentiate the coefficients and confidence intervals; if `FALSE` do not exponentiate (default).
#' @param ci The confidence interval level required. Default is 0.95.
#' @param ci_normal `logical`. If `TRUE` then calculate the confidence interval based on asymptotic normality; if `FALSE` calculate the profile likelihood confidence interval using `confint` (default).
#' @param sigfig The number of significant figures to round the results to. Default is 6.
#' @keywords coefficients, confidence interval, glm
#'
#' @importFrom magrittr %>%
#' @importFrom stats confint confint.default
#' @importFrom dplyr relocate
#'
#' @export
getCoef <- function(fittedModels, exp=F, ci=0.95, ci_normal=F, sigfig=6) UseMethod('getCoef')

#' @export
getCoef.default <- function(fittedModels, exp=F, ci=0.95, ci_normal=F, sigfig=6) print("Function is only available to listGLM objects.")

#' @export
getCoef.listGLM <- function(fittedModels, exp=F, ci=0.95, ci_normal=F, sigfig=6){

  # Function should only be called internally, adding checks just in case
  # Check inputs
  if(class(fittedModels) != "listGLM") {stop(paste("getCoef: The input is not a listGLM object"))}
  if(!is.logical(exp)){stop(paste("getCoef: Argument 'exp' must be logical"))}
  if(!is.logical(ci_normal)){stop(paste("getCoef: Argument 'ci_normal' must be logical"))}
  if(!is.numeric(ci)){stop(paste("getCoef: Argument 'ci' must be a numeric value between 0 and 1"))}
  if(ci>=1 || ci < 0){stop(paste("getCoef: Argument 'ci' must be a numeric value between 0 and 1"))}
  if(!is.numeric(sigfig)){stop(paste("getCoef: Argument 'sigfig' must be an integer greater than 0"))}
  if(sigfig%%1!=0 || sigfig < 0){stop(paste("getCoef: Argument 'sigfig' must be an integer greater than 0"))}


  # Get required data from each model
  vals <- NULL
  model_formulae <- NULL
  model_num <- 1

  for(m in fittedModels){
    summ <- summary(m)
    c1_est <- m$coefficients
    if(ci_normal){ c2_ci<- suppressMessages(stats::confint.default(m, level = ci)) }
    else { c2_ci<- suppressMessages(stats::confint(m, level = ci)) }
    c3_se<- summ$coefficients[,"Std. Error"]
    c4_z<-summ$coefficients[,"z value"]
    c5_p<-summ$coefficients[,"Pr(>|z|)"]

    # Exponentiate estimates and CIs
    if(exp){
      c1_est <- exp(c1_est)
      c2_ci <- exp(c2_ci)
    }

    # append data from each model to the existing vals object
    if(length(c2_ci) == 2) {
      # Intercept only model
      newvals <- signif(cbind(c1_est, c2_ci[[1]], c2_ci[[2]], c3_se, c4_z, c5_p), sigfig)
    } else {
      newvals <- signif(cbind(c1_est, c2_ci, c3_se, c4_z, c5_p), sigfig)
    }

    newvals <- cbind(newvals,paste0("M",model_num))
    vals <- rbind(vals, newvals)

    # store model formula separately
    model_formula <- paste(format(m$formula), collapse=" ")
    model_formula <- cbind(paste0("M",model_num), model_formula)
    model_formulae <- rbind(model_formulae, model_formula)

    model_num <- model_num+1
  }

  # Reformat the data
  # Set row name (variable) as first column
  coef_data<- cbind(rownames(vals), data.frame(vals, row.names=NULL))
  colnames(coef_data) <- c("Variable", "Estimate", "Lower", "Upper", "SE", "z value", "p-value","Model")
  coef_data <- coef_data %>% dplyr::relocate(Model)

  coef_data$Estimate <- as.numeric(coef_data$Estimate)
  coef_data$Lower <- as.numeric(coef_data$Lower)
  coef_data$Upper <- as.numeric(coef_data$Upper)
  coef_data$SE <- as.numeric(coef_data$SE)
  coef_data$'z value' <- as.numeric(coef_data$'z value' )
  coef_data$'p-value'  <- as.numeric(coef_data$'p-value' )

  # Re-order rows based on variable groupings
  coef_data <- coef_data[order(coef_data$Variable,coef_data$Model ),]


  # Reformat formulae
  model_formulae <- as.data.frame(model_formulae)
  colnames(model_formulae) <- c("Model", "Formula")

  # Return list
  return_data<- list()
  return_data$data <- coef_data
  return_data$formula <- model_formulae
  return(return_data)

}

