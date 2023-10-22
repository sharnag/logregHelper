#' List GLM models
#'
#' Function used create list of glm objects of class 'listGLM'
#'
#' @param ... one or more fitted `glm` model objects of family `binomial`.
#' @keywords list glm
#' @examples
#' x <- listGLM(fittedModel1, fittedModel2, fittedModel3)
#'
#'
#' @export
listGLM <- function(...){
  fittedModels <- list(...)

  # Check each model is a glm object of family binomial
  for(m in fittedModels) {
    if(!("glm" %in% class(m))){ stop(paste("Not all objects passed are glm objects.")) }
    if(!("binomial" %in% m$family)) { stop(paste("Not all models passed have family = binomial."))  }
  }

  class(fittedModels) <- 'listGLM'
  return(fittedModels)
}



#' Logistic Regression Coefficients and Confidence Intervals
#'
#' S3 method for class 'listGLM' used to present the model coefficient estimates and confidence intervals of one or more `glm` objects in a gt table or data.frame.
#'
#' @param x An object of class 'listGLM' i.e. a list of fitted `glm` model object(s) of family `binomial`.
#' @param exp `logical`. If `TRUE` then exponentiate the coefficients and confidence intervals; if `FALSE` do not exponentiate (default).
#' @param raw `logical`. If `TRUE` then return the values in a `data.frame` object, if `FALSE` return the values in a `gt` table object (default).
#' @param ci The confidence interval level required.
#' @param ci_normal `logical`. If `TRUE` then calculate the confidence interval based on asymptotic normality; if `FALSE` calculate the profile likelihood confidence interval using `confint` (default).
#' @param sigfig The number of significant figures to round the results to.
#' @param expand `logical`. If `TRUE` then additionally return the standard errors, z values and p-values from the glm summary object.
#' @keywords coefficients, confidence interval, forest plot
#' @examples
#' modelList <- listGLM(fittedModel1, fittedModel2)
#' coef(modelList, exp=T)
#'
#' @importFrom gt gt opt_stylize
#'
#' @export
coef.listGLM <- function(x, exp=F, raw=F, ci=0.95, ci_normal=F, sigfig=6, expand=F){

  # Get the required coefficient list (data and formulae)
  coef_list <- getCoef(x, exp, ci, ci_normal, sigfig)

  # Return coefficients and CIs only
  if(!expand) {
    coef_list$data <- coef_list$data[,1:5]
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
    return(gt_group(gt_data, gt_formula))
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
#' @keywords forest plot, confidence interval
#' @examples
#' modelList <- listGLM(fittedModel1, fittedModel2)
#' plot(modelList)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh position_dodge geom_vline labs theme_minimal scale_x_continuous
#'
#' @export
plot.listGLM <- function(x, exp=F, ci=0.95, ci_normal=F, sigfig=6){

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
                   height = 0.2, size=1, alpha=0.75) +
    geom_vline(xintercept = v, linetype = "dashed") +
    labs(title = " Model Coefficients", x =  xlab) +
    theme_minimal() +
    scale_x_continuous(n.breaks=10)

}


#' Get coefficients from GLM models
#'
#' S3 method for class 'listGLM. Get coefficients, std errors, z and p values for each model in 'listGLM' object.
#'
#' @param fittedModels An object of class 'listGLM' i.e. a list of fitted `glm` model object(s) of family `binomial`.
#' @param exp `logical`. If `TRUE` then exponentiate the coefficients and confidence intervals; if `FALSE` do not exponentiate (default).
#' @param ci The confidence interval level required.
#' @param ci_normal `logical`. If `TRUE` then calculate the confidence interval based on asymptotic normality; if `FALSE` calculate the profile likelihood confidence interval using `confint` (default).
#' @param sigfig The number of significant figures to round the results to.
#' @keywords coefficients, confidence interval, glm
#' @examples
#' coef <- getCoef(listGLM(fittedModel1, fittedModel2, fittedModel3))
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

  # TODO check fittedModels is of class listGLM
  #TODO check ci range is valid
  # check everything is valid


  # Get required data from each model
  vals <- NULL
  model_formulae <- NULL
  model_num <- 1

  for(m in fittedModels){
    summ <- summary(m)
    c1_est <- m$coefficients
    if(ci_normal){ c2_ci<- stats::confint.default(m, level = ci) }
    else { c2_ci<- stats::confint(m, level = ci) }
    c3_se<- summ$coefficients[,"Std. Error"]
    c4_z<-summ$coefficients[,"z value"]
    c5_p<-summ$coefficients[,"Pr(>|z|)"]

    # Exponentiate estimates and CIs
    if(exp){
      c1_est <- exp(c1_est)
      c2_ci <- exp(c2_ci)
    }

    # append data from each model to the existing vals object
    newvals <- signif(cbind(c1_est, c2_ci, c3_se, c4_z, c5_p), sigfig)
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

