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
