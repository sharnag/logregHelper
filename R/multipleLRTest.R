#' Likelihood Ratio Test
#'
#' Performs a likelihood ratio test between pairs of models in a given list.
#'
#' @param model_list A list of fitted `glm` models object of family `binomial`.
#' @param alpha The significance level of the LR test. Default is 0.05.
#' @param override `logical`. If `TRUE` then perform the LR test for all pairs in the input list; if `FALSE` then only perform the LR test for nested model pairs.
#' @keywords fitted
#' @examples
#' new_data <- addCols(fittedModel)
#'
#' @importFrom insight is_nested_models
#'
#' @export
multipleLRTest <- function(model_list, alpha=0.05,override=F){

  # todo input checks

  n <- length(model_list)
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
        comparison_matrix[i, j] <- insight::is_nested_models(model_list[[i]],model_list[[j]])[1]
      }
    }
  }

  # TODO: if no "TRUE", then stop the function here. suggest override.

  # Store formula, deviances and residual dfs in separate vectors
  m_formula <- rep(NA, n)
  m_dev <- rep(NA, n)
  m_df <- rep(NA, n)

  for (i in 1:n){
    m_formula[i] <- format(model_list[[i]]$formula)
    m_dev[i] <- model_list[[i]]$deviance
    m_df[i] <- model_list[[i]]$df.residual
  }

  # Call Rcpp function to calculate p-value of LR test
 list_LR <-rcpp_multipleLRTest(m_dev, m_df, comparison_matrix)

 # Create results dataframe
 rejectH0 <- ifelse(list_LR$pvals<alpha, 'Yes', 'No')
 rejectH0 <- ifelse((list_LR$df==0 & list_LR$pvals==1), 'Invalid', rejectH0)

 results <- data.frame(rejectH0,
                       pvals=signif(list_LR$pvals,4),
                       test_stat=signif(list_LR$test_stat,5),
                       df=list_LR$df,
                       m1=m_formula[list_LR$m1_index],
                       m2=m_formula[list_LR$m2_index])

 return(results)
}




