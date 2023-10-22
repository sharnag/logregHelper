# Unit tests for listGLM()
## Invalid inputs testing

fit_glm <- glm(am ~ cyl + hp + wt, data=mtcars, family = binomial(link = "logit"))
fit_glm2 <- glm(am ~ cyl + hp, data=mtcars, family = binomial(link = "logit"))

fit_glmg <- glm(am ~ cyl + hp + wt, data=mtcars, family = gaussian)
fit_lm <- lm(am ~ cyl + hp + wt, data=mtcars)

test_that("listGLM gives helpful errors for invalid inputs", {
  expect_error(listGLM("123"), "Check that all objects in input are glm objects.")
  expect_error(listGLM(fit_glm,fit_lm), "Check that all objects in input are glm objects.")
  expect_error(listGLM(fit_glm,fit_glmg), "Check that all glm models in input have family = binomial.")
})


## Valid inputs testing
test_that("listGLM works with valid input arguments", {
  expect_silent(listGLM(fit_glm))
  expect_silent(listGLM(fit_glm,fit_glm2))
})

## Functionality testing - check objects created are the correct class
test_that("listGLM creates an object of class listGLM", {
  result <- listGLM(fit_glm,fit_glm2)
  expect_s3_class(result, "listGLM")
  expect_s3_class(result[[1]], c("glm", "lm"))

})
