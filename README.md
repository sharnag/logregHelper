# logregHelper
Created as part of DATA501, Semester 2, 2023.

The purpose of this package is to simplify and streamline the model selection process by providing insightful summaries, tables and visualisations that allow users to review and compare logistic regression models. This package focuses on two areas: Model Diagnostics and Model Selection.

The package can be installed from Github using:

```{r, eval=FALSE}
install.packages("devtools")
devtools::install_github("sharnag/logregHelper")
library(logregHelper)
```

There are currently two main functions in the package:

**Model Diagnostics**

- `plotPredictors` plots the logit of the response variable against the numerical predictors

**Model Selection**

- `viewCoef` view the estimated coefficients and confidence intervals

The logregHelper vignette provides examples of how to use them.
