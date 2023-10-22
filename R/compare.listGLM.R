#' Compare GLM models using various criteria
#'
#' S3 method for class 'listGLM'. Get AIC, BIC and Pseudo-R-squared values for each model in 'listGLM' object.
#'
#' @param fittedModels An object of class 'listGLM' i.e. a list of fitted `glm` model object(s) of family `binomial`.
#' @param raw `logical`. If `TRUE` then return the values in a `data.frame` object, if `FALSE` return the values in a `gt` table object (default).
#' @param sigfig The number of significant figures to round the results to.
#' @param expand `logical`. If  `FALSE` (default), return the AIC, BIC and McFadden's Pseudo R2-values. If TRUE` then return additional Pseudo-R2 values.
#' @keywords aic, bic, compare, R-squared
#' @examples
#' compare(listGLM(fittedModel1, fittedModel2, fittedModel3),expand=T)
#'
#' @importFrom stats AIC BIC
#' @importFrom DescTools PseudoR2
#'
#' @export
compare <- function(fittedModels, raw=F, sigfig=4, expand=F) UseMethod('compare')

#' @export
compare.default <- function(fittedModels, raw=F, sigfig=4, expand=F) print("Function is only available to listGLM objects.")

#' @export
compare.listGLM <- function(fittedModels, raw=F, sigfig=4, expand=F){
  # Get criterion for each model
  vals <- NULL
  for (m in listGLM_obj){
    formula <-paste(format(m$formula), collapse=" ")
    AIC <- stats::AIC(m) # round
    BIC <- stats::BIC(m)
    McFadden <- DescTools::PseudoR2(m, "McFadden")[[1]]
    McFaddenAdj <-  DescTools::PseudoR2(m, "McFaddenAdj")[[1]]
    CoxSnell <-  DescTools::PseudoR2(m, "CoxSnell")[[1]]
    Nagelkerke <-  DescTools::PseudoR2(m, "Nagelkerke")[[1]]
    AldrichNelson <-  DescTools::PseudoR2(m, "AldrichNelson")[[1]]
    VeallZimmermann <-  DescTools::PseudoR2(m, "VeallZimmermann")[[1]]
    McKelveyZavoina <-  DescTools::PseudoR2(m, "McKelveyZavoina")[[1]]
    Efron <-  DescTools::PseudoR2(m, "Efron")[[1]]
    Tjur <-  DescTools::PseudoR2(m, "Tjur")[[1]]

    # append data from each model to the existing vals object
    newvals <- cbind(formula, signif(cbind(AIC, BIC, McFadden,McFaddenAdj,CoxSnell,Nagelkerke,AldrichNelson,VeallZimmermann,McKelveyZavoina,Efron,Tjur), sigfig))
    vals <- rbind(vals, newvals)
  }

  # Return AIC, BIC plus all Pseudo-R2 values OR just AIC, BIC and McFadden's R2 values.
  if (expand){
    df_data<- data.frame(vals)
  } else {
    df_data <- data.frame(vals)[,1:5]
  }


  if(!raw){
    # Convert data.frames to ("gt_tbl" "list") objects
    gt_data <- df_data %>%
      gt::gt() %>%
      gt::opt_stylize(style = 1)

    # Return gt  object
    return(gt_data)
  }

  # Return data.frame object
  return(df_data)
}


