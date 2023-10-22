# Unit tests for compare.listGLM

# Objects used for testing
fit_glm0 <- glm(am ~ 1, data=mtcars, family = binomial(link = "logit"))
fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
fit_glm3 <- glm(am ~ cyl + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm4 <- suppressWarnings(glm(am ~ cyl + hp * wt, data=mtcars, family = binomial(link = "logit")))
fit_glm5 <- suppressWarnings(glm(am ~ cyl + hp + poly(wt, 2), data=mtcars, family = binomial(link = "logit")))

listGLM_obj <- listGLM(fit_glm0, fit_glm1, fit_glm2,fit_glm3,fit_glm4,fit_glm5)
listGLM_single <- listGLM(fit_glm1)



result1 <-compare(listGLM_obj)
result2 <-compare(listGLM_obj, raw=T, sigfig=6, expand=T)
result3 <- compare(listGLM_single, raw=T)

#function(fittedModels, raw=F, sigfig=4, expand=F)

## Invalid inputs testing
test_that("compare.listGLM gives helpful errors for invalid inputs", {
  expect_error(logregHelper:::compare.listGLM("123"), "The input is not a listGLM object")
  expect_error(compare(listGLM_obj, raw=6), "Argument 'raw' must be logical")
  expect_error(compare(listGLM_obj, expand=6), "Argument 'expand' must be logical")
  expect_error(compare(listGLM_obj, sigfig="string"), "Argument 'sigfig' must be an integer greater than 0")
  expect_error(compare(listGLM_obj, sigfig=-1), "Argument 'sigfig' must be an integer greater than 0")

})

## Valid inputs testing
test_that("compare.listGLM works with valid input arguments", {
  expect_silent(compare(listGLM_obj))
  expect_silent(compare(listGLM_single))
  expect_silent(compare(listGLM_obj, raw=T))
  expect_silent(compare(listGLM_obj, expand=T))
  expect_silent(compare(listGLM_obj,sigfig=3))
})

## Functionality testing
test_that("compare.listGLM output is valid", {
  # Check datatypes
  expect_s3_class(result1, c("gt_tbl", "list"))
  expect_s3_class(result2, "data.frame")

  # Check result of compare on simple model
  expect_true(signif(as.numeric(result3$AIC),4)==signif(AIC(fit_glm1),4))
  expect_true(signif(as.numeric(result3$BIC),4)==signif(BIC(fit_glm1),4))
  expect_true(signif(as.numeric(result3$McFadden),4)==signif(PseudoR2(fit_glm1)[[1]],4))

  # Check results don't change
  expect_snapshot(result2)

})
