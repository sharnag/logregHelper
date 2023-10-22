# lrtest.listGLM output is valid

    Code
      result2
    Output
         H0 (alpha=0.01)   p-value Test Stat DF                 M1
      1           Reject 2.667e-07   33.3900  3             am ~ 1
      2           Reject 6.741e-04   14.6000  2             am ~ 1
      3           Reject 6.479e-07   28.5000  2             am ~ 1
      4           Reject 7.283e-07   34.0500  4             am ~ 1
      5           Reject 4.789e-07   34.9400  4             am ~ 1
      6           Reject 1.464e-05   18.7800  1 am ~ cyl + hp + wt
      7    Do Not Reject 2.703e-02    4.8890  1 am ~ cyl + hp + wt
      8    Do Not Reject 4.165e-01    0.6602  1 am ~ cyl + hp + wt
      9    Do Not Reject 2.135e-01    1.5480  1 am ~ cyl + hp + wt
      10    Check Models 0.000e+00   13.8900  0      am ~ cyl + hp
      11          Reject 5.995e-05   19.4400  2      am ~ cyl + hp
      12          Reject 3.847e-05   20.3300  2      am ~ cyl + hp
      13   Do Not Reject 6.237e-02    5.5490  2      am ~ cyl + wt
      14   Do Not Reject 4.002e-02    6.4370  2      am ~ cyl + wt
      15    Check Models 0.000e+00    0.8873  0 am ~ cyl + hp * wt
                                  M2
      1           am ~ cyl + hp + wt
      2                am ~ cyl + hp
      3                am ~ cyl + wt
      4           am ~ cyl + hp * wt
      5  am ~ cyl + hp + poly(wt, 2)
      6                am ~ cyl + hp
      7                am ~ cyl + wt
      8           am ~ cyl + hp * wt
      9  am ~ cyl + hp + poly(wt, 2)
      10               am ~ cyl + wt
      11          am ~ cyl + hp * wt
      12 am ~ cyl + hp + poly(wt, 2)
      13          am ~ cyl + hp * wt
      14 am ~ cyl + hp + poly(wt, 2)
      15 am ~ cyl + hp + poly(wt, 2)

