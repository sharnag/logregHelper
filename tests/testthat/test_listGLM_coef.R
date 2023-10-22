# Unit tests for coef.listGLM()

# Objects used for testing
fit_glm0 <- glm(am ~ 1, data=mtcars, family = binomial(link = "logit"))
fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
fit_glm3 <- glm(am ~ cyl + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm4 <- suppressWarnings(glm(am ~ cyl + hp * wt, data=mtcars, family = binomial(link = "logit")))
fit_glm5 <- suppressWarnings(glm(am ~ cyl + hp + poly(wt, 2), data=mtcars, family = binomial(link = "logit")))

listGLM_obj <- listGLM(fit_glm0,fit_glm2)
listGLM_obj_w <- listGLM(fit_glm0,fit_glm1) # warning from confint

result_gt <- coef(listGLM_obj)
result_df1 <- coef(listGLM_obj, raw=T, exp=T, sigfig=8)
result_df2 <- coef(listGLM_obj, raw=T, ci=0.8, ci_normal=T)


## Invalid inputs testing
test_that("coef.listGLM gives helpful errors for invalid inputs", {
  expect_error(logregHelper:::coef.listGLM("123"), "The input is not a listGLM object")
  expect_error(coef(listGLM_obj, exp=6), "Argument 'exp' must be logical")
  expect_error(coef(listGLM_obj, raw=6), "Argument 'raw' must be logical")
  expect_error(coef(listGLM_obj, ci_normal=6), "Argument 'ci_normal' must be logical")
  expect_error(coef(listGLM_obj, expand=6), "Argument 'expand' must be logical")
  expect_error(coef(listGLM_obj, ci="string"), "Argument 'ci' must be a numeric value between 0 and 1")
  expect_error(coef(listGLM_obj, ci=6), "Argument 'ci' must be a numeric value between 0 and 1")
  expect_error(coef(listGLM_obj, sigfig="string"), "Argument 'sigfig' must be an integer greater than 0")
  expect_error(coef(listGLM_obj, sigfig=-1), "Argument 'sigfig' must be an integer greater than 0")
})


# (fittedModels, exp=F, ci=0.95, ci_normal=F, sigfig=6, expand=F)


## Valid inputs testing
test_that("coef.listGLM works with valid input arguments", {
  # This should run with no errors or warnings
  expect_silent(coef(listGLM_obj))
  expect_silent(coef(listGLM_obj,exp=T))
  expect_silent(coef(listGLM_obj,ci=0.8))
  expect_silent(coef(listGLM_obj,ci_normal=T))
  expect_silent(coef(listGLM_obj,sigfig=4))
  expect_silent(coef(listGLM_obj,expand=F))

  # The confint function results in this warning 22 times: "glm.fit: fitted probabilities numerically 0 or 1 occurred"
  expect_snapshot(coef(listGLM_obj_w))
})


## Functionality testing
test_that("coef.listGLM produces the correct output and datatypes", {
  # Check datatypes
  expect_s3_class(result_gt, "gt_group")
  expect_type(result_df1, "list")
  expect_s3_class(result_df1$data, "data.frame")
  # Check output
  expect_true(round(result_df1$data[2,"Estimate"],2)==round(exp(fit_glm2$coefficients["(Intercept)"]),2))
  suppressMessages(expect_true(round(result_df1$data[2,"Lower"],2)==round(exp(confint(fit_glm2)[1,1]),2)))
  suppressMessages(expect_true(round(result_df1$data[2,"Upper"],2)==round(exp(confint(fit_glm2)[1,2]),2)))
  suppressMessages(expect_true(round(result_df2$data[2,"Lower"],2)==round(confint.default(fit_glm2, level=0.8)[1,1],2)))
  suppressMessages(expect_true(round(result_df2$data[2,"Upper"],2)==round(confint.default(fit_glm2, level=0.8)[1,2],2)))
})
