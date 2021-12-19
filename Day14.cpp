#include <Rcpp.h>
#include <string>
#include <vector>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
std::string find_val(
    std::vector<std::string> from, std::vector<std::string> to,
    char val1, char val2) {

  std::string val (1, val1);
  val.append(1, val2);
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

// [[Rcpp::export]]
void grow_polymer (std::list<std::string>& poly,
    std::vector<std::string> from, std::vector<std::string> to) {

  std::string to_add;
  std::list<std::string>::iterator cur = poly.begin(); 

  while ((++cur) != poly.end()) {
    --cur;

    std::string cur_str = *cur;
    std::string next_str = *(++cur);

    // to_add = find_val(from, to, cur_str, next_str);

    poly.insert(cur, to_add);
  }

}

 // [[Rcpp::export]]
std::string grow_poly(std::string& poly,
    std::vector<std::string> from, std::vector<std::string> to) {

  std::string to_add;

  for (int i = 0; i < poly.size() - 1; i++) {
    to_add = find_val(from, to, poly[i], poly[i+1]);
    poly.insert(i+1, 1, *(to_add.c_str()));
    i++;
  }

  return(poly);
}

// [[Rcpp::export]]
std::string grow_polymer_days(
    std::string poly,
    int days,
    std::vector<std::string> from,
    std::vector<std::string> to) {

  for (int i = 0; i < days; i++) {
    std::cout << "Day " << i + 1 << " of " << days << std::endl;
    grow_poly(poly, from, to);
  }

  return(poly);
}
