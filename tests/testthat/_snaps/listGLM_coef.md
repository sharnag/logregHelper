# coef.listGLM works with valid input arguments

    Code
      coef(listGLM_obj_w, raw = T)
    Warning <simpleWarning>
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      glm.fit: fitted probabilities numerically 0 or 1 occurred
    Output
      $data
        Model    Variable   Estimate        Lower      Upper        SE
      1    M1 (Intercept) -0.3794900  -1.10815000  0.3176620 0.3599370
      2    M2 (Intercept) 19.7029000   8.55536000 44.2516000 8.1163700
      3    M2         cyl  0.4875980  -1.53300000  3.1204700 1.0716200
      4    M2          hp  0.0325917   0.00332091  0.0883896 0.0188611
      5    M2          wt -9.1494700 -21.36320000 -3.4815100 4.1533200
      
      $formula
        Model            Formula
      1    M1             am ~ 1
      2    M2 am ~ cyl + hp + wt
      

