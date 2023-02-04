#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
IntegerMatrix daysSync_noself(IntegerVector starts, IntegerVector ends, int n) {
  IntegerMatrix syncMatrix(n, n-1);
  for (int i = 0; i < n; i++) {
    for (int j = i+1; j < n; j++) {
      int begin = std::max(starts[i], starts[j]);
      int finish = std::min(ends[i], ends[j]);

      int days = finish - begin;
      if (days >= 0) days += 1;
      if (days < 0) days = 0;

      syncMatrix(i, j-1) = days;
      syncMatrix(j, i) = days;
    }
  }
  return syncMatrix;
}

//' @export
// [[Rcpp::export]]
IntegerMatrix daysSync_self(IntegerVector starts, IntegerVector ends, int n) {
  IntegerMatrix syncMatrix(n, n);
  for (int i = 0; i < n; i++) {
    for (int j = i; j < n; j++) {
      int begin = std::max(starts[i], starts[j]);
      int finish = std::min(ends[i], ends[j]);

      int days = finish - begin;
      if (days >= 0) days += 1;
      if (days < 0) days = 0;

      syncMatrix(i, j) = days;
      syncMatrix(j, i) = days;
    }
  }
  return syncMatrix;
}

//' @export
// [[Rcpp::export]]
IntegerMatrix daysEither_noself(IntegerVector starts, IntegerVector ends, int n) {
  IntegerMatrix eitherMatrix(n, n-1);
  for (int i = 0; i < n; i++) {
    for (int j = i+1; j < n; j++) {
      if (starts[i] > ends[j] || starts[j] > ends[i]) {
        eitherMatrix(i, j-1) = ends[i] - starts[i] + ends[j] - starts[j] + 2;
        eitherMatrix(j, i) = ends[i] - starts[i] + ends[j] - starts[j] + 2;
        continue;
      }

      int begin = std::min(starts[i], starts[j]);
      int finish = std::max(ends[i], ends[j]);

      int days = finish - begin;
      if (days >= 0) days += 1;
      if (days < 0) days = 0;

      eitherMatrix(i, j-1) = days;
      eitherMatrix(j, i) = days;
    }
  }
  return eitherMatrix;
}

//' @export
// [[Rcpp::export]]
IntegerMatrix daysEither_self(IntegerVector starts, IntegerVector ends, int n) {
  IntegerMatrix eitherMatrix(n, n);
  for (int i = 0; i < n; i++) {
    for (int j = i; j < n; j++) {
      if (starts[i] > ends[j] || starts[j] > ends[i]) {
        eitherMatrix(i, j) = ends[i] - starts[i] + ends[j] - starts[j] + 2;
        eitherMatrix(j, i) = ends[i] - starts[i] + ends[j] - starts[j] + 2;
        continue;
      }

      int begin = std::min(starts[i], starts[j]);
      int finish = std::max(ends[i], ends[j]);

      int days = finish - begin;
      if (days >= 0) days += 1;
      if (days < 0) days = 0;

      eitherMatrix(i, j) = days;
      eitherMatrix(j, i) = days;
    }
  }
  return eitherMatrix;
}
