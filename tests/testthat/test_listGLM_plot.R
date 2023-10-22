# Unit tests for plot.listGLM

# Objects used for testing
fit_glm0 <- glm(am ~ 1, data=mtcars, family = binomial(link = "logit"))
fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
fit_glm3 <- glm(am ~ cyl + wt, data=mtcars, family = binomial(link = "logit"))


listGLM_obj <- listGLM(fit_glm0,fit_glm2)
listGLM_obj_w <- listGLM(fit_glm0,fit_glm1) # warning from confint

result_ggplot <- plot(listGLM_obj)
result_ggplot2 <- plot(listGLM_obj, exp=T, ci=0.8, ci_normal=T, sigfig=4)

## Invalid inputs testing
test_that("plot gives helpful errors for invalid inputs", {
  expect_error(logregHelper:::plot.listGLM("123"), "The input is not a listGLM object")
  expect_error(plot(listGLM_obj, exp=6), "Argument 'exp' must be logical")
  expect_error(plot(listGLM_obj, ci_normal=6), "Argument 'ci_normal' must be logical")
  expect_error(plot(listGLM_obj, ci="string"), "Argument 'ci' must be a numeric value between 0 and 1")
  expect_error(plot(listGLM_obj, ci=6), "Argument 'ci' must be a numeric value between 0 and 1")
  expect_error(plot(listGLM_obj, sigfig="string"), "Argument 'sigfig' must be an integer greater than 0")
  expect_error(plot(listGLM_obj, sigfig=-1), "Argument 'sigfig' must be an integer greater than 0")
})


## Valid inputs testing
test_that("plot works with valid input arguments", {
  expect_silent(plot(listGLM_obj))
  expect_silent(plot(listGLM_obj,exp=T))
  expect_silent(plot(listGLM_obj,ci=0.8))
  expect_silent(plot(listGLM_obj,ci_normal=T))
  expect_silent(plot(listGLM_obj,sigfig=4))
})

## Functionality testing
test_that("plot output is valid", {
  expect_s3_class(result_ggplot, c("gg","ggplot"))
  # Check that data used by plot function has not changed
  expect_snapshot(result_ggplot$data)

})
