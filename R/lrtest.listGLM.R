#' Likelihood Ratio Test for Multiple Models
#'
#' S3 method for class 'listGLM'. Performs a likelihood ratio test between pairs of models in a given listGLM object.
#'
#' @param x An object of class 'listGLM' i.e. a list of fitted `glm` model object(s) of family `binomial`.
#' @param alpha The significance level of the LR test. Default is 0.05.
#' @param raw `logical`. If `TRUE` then return the values in a `data.frame` object, if `FALSE` return the values in a `gt` table object (default).
#' @param override `logical`. If `TRUE` then perform the LR test for all pairs in the input list; if `FALSE` then only perform the LR test for nested model pairs.
#' @param sigfig The number of significant figures to round the results to. Default is 4.
#' @keywords likelihood ratio test
#' @examples
#' fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
#' fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
#' modelList <- listGLM(fit_glm1, fit_glm2)
#' lrtest(modelList)
#'
#' @importFrom insight is_nested_models
#' @importFrom magrittr %>%
#' @importFrom gt gt opt_stylize
#' @importFrom lmtest lrtest
#'
#' @export
lrtest.listGLM <- function(x, alpha=0.05, raw=F, override=F, sigfig=4){

  # Check inputs
  if(class(x) != "listGLM") {stop(paste("The input is not a listGLM object"))}
  if(!is.logical(raw)){stop(paste("Argument 'raw' must be logical"))}
  if(!is.logical(override)){stop(paste("Argument 'override' must be logical"))}
  if(!is.numeric(alpha)){stop(paste("Argument 'alpha' must be a numeric value between 0 and 1"))}
  if(alpha > 1 || alpha < 0){stop(paste("Argument 'alpha' must be a numeric value between 0 and 1"))}
  if(!is.numeric(sigfig)){stop(paste("Argument 'sigfig' must be an integer greater than 0"))}
  if(sigfig%%1!=0 || sigfig < 0){stop(paste("Argument 'sigfig' must be an integer greater than 0"))}

  # Check if only one model exists in list
  n <- length(x)
  if(n<2) {stop(paste("The listGLM object must contain more than one model"))  }


  # Create a comparison matrix indicating which models are nested
  comparison_matrix <- matrix(0,nrow = n, ncol = n)

  # Loop through each pair of elements once, excluding pairs with itself
  for (i in 1:(n-1)) {
    for (j in (i + 1):n) {
      if (override){
        # All pairs will be compared
        comparison_matrix[i, j] <- 1
      } else{
        # Only nested models will be compared (1 if nested, otherwise 0)
        comparison_matrix[i, j] <- insight::is_nested_models(x[[i]],x[[j]])[1]
        # Message from insight: Some of the nested models seem to be identical
      }
    }
  }

  # If there are no pairs to compare, stop here
  if(sum(comparison_matrix) == 0){stop(paste("The listGLM object contains no nested models"))  }

  # Store formula, deviances and residual dfs in separate vectors
  m_formula <- rep(NA, n)
  m_dev <- rep(NA, n)
  m_df <- rep(NA, n)

  for (i in 1:n){
    m_formula[i] <- paste(format(x[[i]]$formula), collapse=" ")
    m_dev[i] <- x[[i]]$deviance
    m_df[i] <- x[[i]]$df.residual
  }

  # Call Rcpp function to calculate p-value of LR test
 list_LR <-rcpp_multipleLRTest(m_dev, m_df, comparison_matrix)

 # Create results dataframe
 rejectH0 <- ifelse(list_LR$pvals<alpha, 'Reject', 'Do Not Reject')
 rejectH0 <- ifelse((list_LR$df==0), 'Check Models', rejectH0)

 results <- data.frame(rejectH0,
                       pvals=signif(list_LR$pvals,sigfig),
                       test_stat=signif(list_LR$test_stat,sigfig),
                       df=list_LR$df,
                       m1=m_formula[list_LR$m1_index],
                       m2=m_formula[list_LR$m2_index])

 test_level <- paste0("H0 (alpha=", alpha, ")")
 colnames(results) <- c(test_level, "p-value", "Test Stat", "DF", "M1", "M2")

 if(!raw){
   # Convert data.frame to ("gt_tbl" "list")
   results <- results %>%
     gt::gt() %>%
     gt::opt_stylize(style = 5)

 }

 return(results)
}


