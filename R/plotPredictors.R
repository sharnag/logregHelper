#' Plot Logit of Response against Numerical Predictors
#'
#' S3 Method for glm object used to return plots of the logit of the response vs numerical predictors. Used during model diagnostics.
#'
#' @param fittedModel A fitted `glm` model object of family `binomial`.
#' @param interactive `logical`. If `TRUE` then return an interactive plot; if `FALSE`, return ggplot objects (default).
#' @keywords diagnostics, assumptions, linearity, logit, predictor plots
#' @examples
#' my_plots <- plotPredictors(fittedModel)
#' my_plots[[1]]
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal geom_smooth
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly add_markers layout
#' @importFrom rlang sym
#'
#' @export
plotPredictors <- function(fittedModel, smooth=T, interactive=F) UseMethod('plotPredictors')

#' @export
plotPredictors.default <- function(fittedModel, smooth=T, interactive=F) print("Function is only available to glm objects.")

#' @export
plotPredictors.glm <- function(fittedModel, smooth=T, interactive=F){

  # Check inputs
  if(!is.logical(smooth)){stop(paste("Argument 'smooth' must be logical"))}
  if(!is.logical(interactive)){stop(paste("Argument 'interactive' must be logical"))}

  # Check that fittedModel is a glm object of family binomial
  if(!("glm" %in% class(fittedModel))){ stop(paste("The model must be a glm object.")) }
  if(!("binomial" %in% fittedModel$family)) { stop(paste("The glm object must have family = binomial."))  }



  # TODO, need to decide how to handle polynomial terms "poly(age, 2)" and models with interaction
  # get the numerical terms used in the model
  numerical_cols <- colnames(fittedModel$data[,sapply(fittedModel$data,is.numeric)])
  model_terms <- attr(fittedModel$terms, "term.labels")
  numerical_terms <- intersect(numerical_cols, model_terms)

  # if no numerical terms in the model then return an error message
  if (length(numerical_terms)==0) {stop(paste("The model has no numerical predictors."))}

  # Calculate the logit of the predicted probabilities
  data_copy <- fittedModel$data
  data_copy$logitResp <- log(fittedModel$fitted.values / (1-fittedModel$fitted.values))


  if(interactive){

    # Use Plotly to create interactive plot
    fig <- plotly::plot_ly(data_copy, y = ~logitResp)

    # Add markers for each numerical predictor
    # Initially we show only the first predictor and hide the remaining plots
    vis_flag <- TRUE
    for (i in numerical_terms) {
      fig <- fig %>% plotly::add_markers(x=data_copy[[i]], name=i,  visible=vis_flag, color=I("steelblue"))
      if(vis_flag) vis_flag <- FALSE
    }

    # Create a list of buttons dynamically based on numerical_terms
    # This creates a vector of booleans to apply to the "visible" argument
    # e.g. for the second element in numerical_terms, this would be list(FALSE, TRUE, FALSE, ...)
    buttons_list <- lapply(numerical_terms, function(term) {
      list(
        method = "restyle",
        args = list("visible", lapply(numerical_terms, function(t) t == term)),
        label = term
      )
    })

    # Define the layout and update action when selecting predictors
    fig <- fig %>% plotly::layout(
      showlegend = F,
      title = "Logit of Response vs Numerical Predictors",
      xaxis = list(title="Predictor"),
      yaxis = list(title = "Logit"),
      updatemenus = list(
        list(x = 1.4, y= 0.7,buttons = buttons_list)
      )
    )
    # return plotly figure
    return(fig)
  }
  else{
    # Plot non-interactive plots using ggplot2
    diagnostic_plots <- list()
    for (i in numerical_terms) {
      var <- rlang::sym(i)
      temp_plot <- ggplot2::ggplot(data_copy,aes(x=!!var, y=logitResp))+
        geom_point(color='steelblue') +
        labs(y="Logit") +
        theme_minimal()
      if(smooth) {temp_plot <- temp_plot + ggplot2::geom_smooth(method='loess', color='orange', fill='orange')}
      diagnostic_plots[[i]] <- temp_plot

    }
    # Return list object of ggplots
    return(diagnostic_plots)
  }


}

