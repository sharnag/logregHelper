#' Internal Function used copy the model dataframe and add additional columns
#'
#' @param fittedModel is a logistic regression model fitted using the glm function
#' @keywords fitted
#' @export
#' @examples
#' new_data <- addCols(fittedModel)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate n
addCols <-function(fittedModel){
  data_copy <- fittedModel$data

  # Bind logit(.fitted) etc to the dataframe
  data_copy <- broom::augment(fittedModel) %>%
    dplyr::mutate(index = 1:dplyr::n())

  # Add the predicted probabilities
  data_copy$predprob <- fittedModel$fitted.values

  return(data_copy)
}

