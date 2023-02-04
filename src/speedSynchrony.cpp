#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericVector row_medians(NumericMatrix toSort) {
  int n = toSort.rows();
  int medN  = toSort.cols();
  NumericVector meds = NumericVector(n);
  for (int i = 0; i < n; i++) {
    NumericVector curRow = toSort.row(i);
    std::nth_element(curRow.begin(), curRow.begin() + curRow.size()/2 - 1, curRow.end());
    double med1 = curRow[curRow.size()/2 - 1];
    if (medN % 2 == 0) {
      std::nth_element(curRow.begin(), curRow.begin() + curRow.size()/2, curRow.end());
      double med2 = curRow[curRow.size()/2];
      meds[i] = (med1 + med2)/2.0;
    } else {
      meds[i] = med1;
    }
  }

  return meds;
}

//' @export
// [[Rcpp::export]]
NumericVector row_kth(NumericMatrix toSort, int k) {
  int n = toSort.rows();
  NumericVector meds = NumericVector(n);
  for (int i = 0; i < n; i++) {
    NumericVector curRow = toSort.row(i);
    std::nth_element(curRow.begin(), curRow.begin() + k, curRow.end());
    meds[i] = curRow[k];
  }

  return meds;
}

//' @export
// [[Rcpp::export]]
NumericVector kemp_ind(IntegerVector byDay, IntegerVector starts,
                       IntegerVector ends, IntegerVector durs,
                       bool compSelf = false) {
  int n = starts.size();
  NumericVector indSync = NumericVector(n);
  double iTotal = 0;

  for (int i = 0; i < n; i++) {
    iTotal = 0;
    for (int d = starts[i]-1; d < ends[i]; d++) {
      iTotal += byDay[d];
    }
    double divideBy = n;
    if (!compSelf) {
      iTotal -= durs[i];
      divideBy -= 1;
    }
    indSync[i] = iTotal/(durs[i]*divideBy);
  }

  return indSync;
}
