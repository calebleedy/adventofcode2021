#include <RcppArmadillo.h>
#include <algorithm>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
mat get_path_weight (mat weight_mat, mat ret) {

  ret(0, 0) = 0;

  int y = 0;

  for (int i = 0; i < weight_mat.n_rows + weight_mat.n_cols - 2; i++) {
    for (int x = std::max(0, i - ((int) weight_mat.n_cols) + 1);
        x <= std::min(i, ((int) weight_mat.n_rows) - 1); x++) {
      y = i - x;

      int x_vals[] = {std::max(x - 1, 0), x, x, std::min(x + 1, (int) weight_mat.n_rows - 1)};
      int y_vals[] = {y, std::max(y - 1, 0), std::min(y + 1, (int) weight_mat.n_cols - 1), y};

      for (int j = 0; j < 4; j++) {
        ret(x_vals[j], y_vals[j]) = 
          std::min(ret(x_vals[j], y_vals[j]), ret(x, y) + weight_mat(x_vals[j], y_vals[j]));
      }

    }
  }

  return(ret);
}


