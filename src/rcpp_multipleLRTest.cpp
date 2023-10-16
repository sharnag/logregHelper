// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/math/distributions/chi_squared.hpp> //included in BH

using namespace Rcpp;


//' Internal rcpp function - Likelihood Ratio Test
//' @name Internal rcpp function - Likelihood Ratio Test
//'
//' @details Internal rcpp function that performs a likelihood ratio test between pairs of models. Returns the p-values and model indicies.
//'
//' @export
//' @examples
//' list_LR <-multipleLRTest(m_dev, m_df, comparison_matrix)
//' @export
// [[Rcpp::export]]
List rcpp_multipleLRTest(NumericVector dev_vec, IntegerVector df_vec, NumericMatrix comparison_matrix) {

  int n = dev_vec.length();
  double test_dev;
  int test_df;
  int total_vals = sum(comparison_matrix);
  NumericVector p (total_vals);
  NumericVector m1_index (total_vals);
  NumericVector m2_index (total_vals);

  int counter = 0;
  for (int i = 0; i < (n-1); ++i) {
    for (int j = (i+1); j < n; ++j) {
      if(comparison_matrix(i,j)) { //if models are nested

        // calculate p-value of LR test
        test_dev = abs(dev_vec[i] - dev_vec[j]);
        test_df = abs(df_vec[i] - df_vec[j]);
        boost::math::chi_squared mydist(test_df);
        p[counter] = 1-boost::math::cdf(mydist,test_dev);

        //store model indexes for R (add 1)
        m1_index[counter] = i+1;
        m2_index[counter] = j+1;
        ++counter;
      }
    }
  }
  // return p-values and model pair indexes
  List ret;
  ret["m1_index"] = m1_index;
  ret["m2_index"] = m2_index;
  ret["pvals"] = p;
  return(ret);
}


