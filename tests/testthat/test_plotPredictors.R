# Unit tests for plotPredictors()

# Objects used for testing
fit_glm0 <- glm(am ~ 1, data=mtcars, family = binomial(link = "logit"))
fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
fit_glm3 <- glm(am ~ cyl + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm4 <- suppressWarnings(glm(am ~ cyl + hp * wt, data=mtcars, family = binomial(link = "logit")))
fit_glm5 <- suppressWarnings(glm(am ~ cyl + hp + poly(wt, 2), data=mtcars, family = binomial(link = "logit")))

fit_mix <- glm(am ~ factor(cyl) + hp + wt, data=mtcars, family = binomial(link = "logit"))
fit_mix2 <- glm(am ~ factor(cyl) + wt, data=mtcars, family = binomial(link = "logit"))
fit_glmg <- glm(am ~ cyl + hp + wt, data=mtcars, family = gaussian)
fit_lm <- lm(am ~ cyl + hp + wt, data=mtcars)
fit_glm_no_numeric <- glm(am ~ factor(cyl) + factor(gear), data=mtcars, family = binomial)

result <- suppressMessages(plotPredictors(fit_mix))
result2 <- suppressMessages(plotPredictors(fit_mix, interactive = T))

#function(fittedModel, smooth=T, interactive=F)

## Invalid inputs testing
test_that("plotPredictors gives helpful errors for invalid inputs", {
  expect_output(plotPredictors(fit_lm), "Function is only available to glm objects.")
  expect_error(logregHelper:::plotPredictors.glm("123"), "The model must be a glm object.")
  expect_error(plotPredictors(fit_glmg), "The glm object must have family = binomial.")
  expect_error(plotPredictors(fit_glm_no_numeric), "The model has no numerical predictors.")
  expect_error(plotPredictors(fit_glm1, smooth=6), "Argument 'smooth' must be logical")
  expect_error(plotPredictors(fit_glm1, interactive=6), "Argument 'interactive' must be logical")

})


## Valid inputs testing
test_that("plotPredictors works with valid input arguments", {
  expect_silent(plotPredictors(fit_mix2))
  expect_silent(plotPredictors(fit_mix2, smooth=F))
  expect_silent(plotPredictors(fit_mix2, interactive=T))

  # plotPredictors should return an informational message for discrete predictors
  expect_message(plotPredictors(fit_mix), "The predictor 'hp' is discrete.")

  # plotPredictors should return a warning when numerical predictors in the model contains transformations


})

## Functionality testing - check objects created are the correct class
test_that("plotPredictors creates valid output objects", {
  expect_type(result, "list")
  expect_s3_class(result[[1]], c("gg","ggplot"))
  expect_s3_class(result2, c("plotly","htmlwidget"))

  # Check that data used by plot function has not changed
  expect_snapshot(result[[1]]$data)

})
