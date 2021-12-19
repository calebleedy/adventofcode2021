#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
std::string find_val(
    std::vector<std::string> from, std::vector<std::string> to, std::string val) {

  int ind = 0;

  for (int j = 0; j < from.size(); j++) {
    if (from[j].compare(val) == 0) {
      ind = j;
      break;
    }
  }
  std::string ret = to[ind];
  return(ret);
}
