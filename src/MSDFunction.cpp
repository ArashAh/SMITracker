#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector localMSDcomplete(NumericVector positions, int windowSize) {

  int n = positions.size();

  if (windowSize >= n) {
    windowSize = n - 1;
  }

  // container for results
  NumericMatrix mat(n-1, windowSize-1);

  // container for mean square displacements
  NumericVector msds(n);
  // calculate square displacement at each distance
  for(int j = 1; j < windowSize; j++) {

    // container for square displacements internally
    // NumericVector msds_internal(n-j);

    for(int i = 0; i < n-j; i++) {

      double value = 0;
      int kk = 0;

      // max aggregation windowlength
      int aggWindowLength = windowSize-1;
      if (n-i-j < windowSize-1){
        aggWindowLength = n-i-j;
      }


      // aggregate square displacements
      for(int k = 0; k<aggWindowLength; k++) { //windowSize-1
        // local difference
        double tmp = positions(i+k) - positions(i+j+k);
        // aggregated square values
        value = value + tmp * tmp;

        kk = k;
      }

      // mean of values
      value = value/(kk+1);


      mat(i,j-1) = value / j;

      // msds_internal(i) = value;
    }
    // msds(j-1) = mean(msds_internal) ;
  }

  for (int j = 0; j <= n - windowSize; j++) {
    msds(j) = mean(mat(j,_ ));
  }

  // NumericVector output(1);

  // output(0) = mean(msds);


  return(msds);
}

