#' Add Columns to glm Data
#'
#' Internal Function used copy the model dataframe and add additional columns such as the predicted probability and logit of the response. Used by `plotPredictors` during model diagnostics
#'
#' @param fittedModel A fitted `glm` model object of family `binomial`.
#' @keywords fitted
#' @examples
#' new_data <- addCols(fittedModel)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate n
#'
#' @export
addCols <- function(fittedModel) UseMethod('addCols')

#'
#' @export
addCols.glm <-function(fittedModel){
  data_copy <- fittedModel$data

  # Bind logit(.fitted) etc to the dataframe
  data_copy <- broom::augment(fittedModel) %>%
    dplyr::mutate(index = 1:dplyr::n())

  # Add the predicted probabilities
  data_copy$predprob <- fittedModel$fitted.values

  return(data_copy)
}

