# Unit tests for lrtest.listGLM

# Objects used for testing
fit_glm0 <- glm(am ~ 1, data=mtcars, family = binomial(link = "logit"))
fit_glm1 <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))
fit_glm3 <- glm(am ~ cyl + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm4 <- suppressWarnings(glm(am ~ cyl + hp * wt, data=mtcars, family = binomial(link = "logit")))
fit_glm5 <- suppressWarnings(glm(am ~ cyl + hp + poly(wt, 2), data=mtcars, family = binomial(link = "logit")))

listGLM_obj <- listGLM(fit_glm0, fit_glm1, fit_glm2,fit_glm3,fit_glm4,fit_glm5)
listGLM_single <- listGLM(fit_glm1)
listGLM_same <- listGLM(fit_glm1, fit_glm1)
listGLM_nonest <- listGLM(fit_glm2, fit_glm3)


result1 <-lrtest(listGLM_obj)
result2 <-lrtest(listGLM_obj, alpha=0.01, raw=T, override=T)
result3 <- lrtest(listGLM(fit_glm1, fit_glm2), raw=T)

#function(x, alpha=0.05, raw=F, override=F, sigfig=4){

## Invalid inputs testing
test_that("lrtest.listGLM gives helpful errors for invalid inputs", {
  expect_error(logregHelper:::lrtest.listGLM("123"), "The input is not a listGLM object")
  expect_error(lrtest(listGLM_obj, raw=6), "Argument 'raw' must be logical")
  expect_error(lrtest(listGLM_obj, override=6), "Argument 'override' must be logical")
  expect_error(lrtest(listGLM_obj, alpha="string"), "Argument 'alpha' must be a numeric value between 0 and 1")
  expect_error(lrtest(listGLM_obj, alpha=6), "Argument 'alpha' must be a numeric value between 0 and 1")
  expect_error(lrtest(listGLM_obj, sigfig="string"), "Argument 'sigfig' must be an integer greater than 0")
  expect_error(lrtest(listGLM_obj, sigfig=-1), "Argument 'sigfig' must be an integer greater than 0")

  expect_error(lrtest(listGLM_single), "The listGLM object must contain more than one model")
  expect_error(lrtest(listGLM_nonest), "The listGLM object contains no nested models")
  expect_message(lrtest(listGLM_same), "Some of the nested models seem to be identical")



})

## Valid inputs testing
test_that("lrtest.listGLM works with valid input arguments", {
  expect_silent(lrtest(listGLM_obj))
  expect_silent(lrtest(listGLM_obj, raw=T))
  expect_silent(lrtest(listGLM_obj, override=T))
  expect_silent(lrtest(listGLM_obj,alpha=0.01))
  expect_silent(lrtest(listGLM_obj,sigfig=3))
})

## Functionality testing
test_that("lrtest.listGLM output is valid", {
  # Check datatypes
  expect_s3_class(result1, c("gt_tbl", "list"))
  expect_s3_class(result2, "data.frame")

  # Check result of LRTest on simple model
  expect_true(signif(result3$`p-value`,4)==signif(lrtest(fit_glm1,fit_glm2)$`Pr(>Chisq)`[[2]],4))

  # Check results don't change
  expect_snapshot(result2)

})
