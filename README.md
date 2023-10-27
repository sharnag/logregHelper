# logregHelper
Created as part of DATA501, Semester 2, 2023.

The purpose of this package is to simplify and streamline the model selection process by providing insightful summaries, tables and visualisations that allow users to review and compare logistic regression models. This package focuses on two areas: Model Diagnostics and Model Selection.

The package can be installed from Github using:

```{r, eval=FALSE}
install.packages("devtools")
devtools::install_github("sharnag/logregHelper")
```

The main functions are:

**Model Diagnostics**

- `plotPredictors` plots the logit of the predicted probability against each numerical predictor
 
**Model Selection**

- `listGLM` creates an object of a new class `listGLM` which is essentially a list of fitted `glm` models.

It implements the following methods:

- `coef` returns the estimated coefficients and confidence intervals
- `plot` plot the estimated coefficients and confidence intervals in a Forest Plot
- `compare` returns the AIC, BIC and Pseudo-R-squared values for each model
- `lrtest` performs a Likelihood Ratio test between each pair of nested models

The logregHelper vignette provides examples of how to use these functions.
