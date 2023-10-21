#' Compare GLM models using various criteria
#'
#' S3 method for class 'listGLM'. Get AIC, BIC and Pseudo-R-squared values for each model in 'listGLM' object.
#'
#' @param fittedModels An object of class 'listGLM' i.e. a list of fitted `glm` model object(s) of family `binomial`.
#' @param expand `logical`. If  `FALSE` (default), return the AIC, BIC and McFadden's Pseudo R2-values. If TRUE` then return additional Pseudo-R2 values.
#' @param sigfig The number of significant figures to round the results to.
#' @keywords aic, bic, compare, R-squared
#' @examples
#' compare(listGLM(fittedModel1, fittedModel2, fittedModel3),expand=T)
#'
#' @importFrom stats AIC BIC
#' @importFrom DescTools PseudoR2
#'
#' @export
compare <- function(fittedModels, expand=F, sigfig=4) UseMethod('compare')

#' @export
compare.default <- function(fittedModels, expand=F, sigfig=4) print("Function is only available to listGLM objects.")

#' @export
compare.listGLM <- function(fittedModels, expand=F, sigfig=4){
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

  if (expand){
    return(data.frame(vals))
  } else {
    return(data.frame(vals)[,1:5])
  }

}
