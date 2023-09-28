#' Logistic Regression Coefficients and Confidence Intervals
#'
#' Function used to present the model coefficient estimates and confidence intervals of one or more `glm` objects in a gt table, data.frame or Forest Plot.
#'
#' @param fittedModels A single or list of fitted `glm` model object of family `binomial`.
#' @param exp `logical`. If `TRUE` then exponentiate the coefficients and confidence intervals; if `FALSE` do not exponentiate (default).
#' @param plot `logical`. If `TRUE` then return a Forest Plot of the estimates and confidence intervals; if `FALSE` return a table (default).
#' @param raw `logical`. If `TRUE` then return the values in a `data.frame` object, if `FALSE` return the values in a `gt` table object (default).
#' @param ci The confidence interval level required.
#' @param ci_normal `logical`. If `TRUE` then calculate the confidence interval based on asymptotic normality; if `FALSE` calculate the profile likelihood confidence interval using `confint` (default).
#' @param sigfig The number of significant figures to round the results to.
#' @param expand `logical`. If `TRUE` then additionally return the standard errors, z values and p-values from the glm summary object.
#' @keywords coefficients, confidence interval, forest plot
#' @examples
#' viewCoef(fittedModel1, exp=T)
#' modelList <- list(fittedModel1, fittedModel2)
#' viewCoef(modelList, plot=T)
#'
#' @importFrom magrittr %>%
#' @importFrom stats confint confint.default
#' @importFrom dplyr relocate
#' @importFrom gt gt opt_stylize
#'
#' @export
viewCoef <- function(fittedModels, exp=F, plot=F, raw=F, ci=0.95, ci_normal=F, sigfig=6, expand=F){

  # Check if fittedModels is a glm object or a list
  if(!("glm" %in% class(fittedModels)) & !("list" %in% class(fittedModels))){
    stop(paste("Object must be a glm object or list of glm objects."))
  }

  # If single glm model passed, check that it has family=binomial, then add it to a list
  if("glm" %in% class(fittedModels)) {
    if(!("binomial" %in% fittedModels$family)) { stop(paste("The glm object must have family = binomial."))}
    fittedModels <- list(fittedModels)
  }

  # Check each model is
  for(m in fittedModels) {
    # Check that fittedModel is a glm object of family binomial
    if(!("glm" %in% class(m))){ stop(paste("Not all models in list are glm objects.")) }
    if(!("binomial" %in% m$family)) { stop(paste("Not all models in list have family = binomial."))  }
  }

  # Mutiple Models
  vals <- NULL
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
    newvals <- signif(cbind(c1_est, c2_ci, c3_se, c4_z, c5_p), sigfig)
    newvals <- cbind(newvals, paste0("M",model_num))

    # append data from each model to the existing vals object
    vals <- rbind(vals, newvals)
    model_num <- model_num+1
  }

  # Reformat the data
  coef_data<- cbind(rownames(vals), data.frame(vals, row.names=NULL))
  colnames(coef_data) <- c("Variable", "Estimate", "Lower", "Upper", "SE", "z value", "p-value", "Model")
  coef_data <- coef_data %>% dplyr::relocate(Model)

  coef_data$Estimate <- as.numeric(coef_data$Estimate)
  coef_data$Lower <- as.numeric(coef_data$Lower)
  coef_data$Upper <- as.numeric(coef_data$Upper)
  coef_data$SE <- as.numeric(coef_data$SE)
  coef_data$'z value' <- as.numeric(coef_data$'z value' )
  coef_data$'p-value'  <- as.numeric(coef_data$'p-value' )

  # Re-order rows based on variable groupings
  coef_data <- coef_data[order(coef_data$Variable,coef_data$Model ),]



  if(plot){
    # Return ggplot object
    return(plotCoef(coef_data, exp=exp))

  } else{
    # Return coefficients and CIs only
    if(!expand) {
      coef_data <- coef_data[,1:5]
    }

    if(!raw){
      # Convert data.frame to ("gt_tbl" "list")
      coef_data<- coef_data %>%
        gt::gt() %>%
        gt::opt_stylize(style = 1)

    }
    # Return data.frame object of gt table
    return(coef_data)
  }


}

