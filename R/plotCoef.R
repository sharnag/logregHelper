#' Forest Plot of Logistic Regression Coefficient Estimates
#'
#' Internal Method used to plot coefficient estimates and confidence intervals in a Forest Plot. Used by `viewCoef`.
#'
#' @param coef_data A `data.frame` containing the coefficient estimates and confidence intervals of one or more `glm` models.
#' @param exp `logical`. If `TRUE` then the coefficients are exponentiated, so the x marker=0; if `FALSE`, x marker=1.
#' @keywords forest plot, confidence interval
#' @examples
#' plotCoef(fittedModel)
#'
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh position_dodge geom_vline labs theme_minimal scale_x_continuous
#'
#' @export
plotCoef <- function(coef_data, exp=F){
  # Move the x marker depending on whether coefficients are exponentiated or not
  v<-1
  if(exp) v <- 0
  # Create a forest plot with stacked points
  ggplot2::ggplot(coef_data, aes(y = Variable, x=Estimate, color=Model)) +
    geom_point(position = position_dodge(width=0.4), size=2) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), position = position_dodge(width=0.4),
                   height = 0.2, size=1, alpha=0.75) +
    geom_vline(xintercept = v, linetype = "dashed") +
    labs(title = " Model Coefficients", x = "Coefficient Estimate") +
    theme_minimal() +
    scale_x_continuous(n.breaks=10)

}
