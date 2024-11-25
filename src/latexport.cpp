#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
RObject do_match_after(
  IntegerVector lin,
  IntegerVector ltarg,
  IntegerVector nmatch // pre-allocated on R side
){
  int nin = lin. size();
  int ntarg = ltarg. size();
  for( int i_in=0; i_in<nin; i_in++) {
    int old_nmatch = nmatch[ i_in];
    nmatch[ i_in] = 0;
    int this_lin = lin[ i_in];
    bool breako = false;
    for( int j_targ = old_nmatch; (!breako) && (j_targ <= ntarg); j_targ++) {
      breako = this_lin == ltarg[ j_targ-1];
      if( breako) {
        nmatch[ i_in] = j_targ;
      };
    }; // for j_targ
  }; // for i_in

return R_NilValue;
}
