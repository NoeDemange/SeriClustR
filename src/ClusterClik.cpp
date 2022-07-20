#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector ClusterClikcpp(NumericMatrix Rmat, double R){
  int ncol = Rmat.ncol();
  IntegerVector VphiClikClus;
  VphiClikClus.push_back(1);
  int m = 1;
  int i = 0;
  
  for(int j = i+1; j<ncol; j++){
    for (int k = i; k<=j-1; k++){
      if(Rmat(k,j)<R){
      i = j;
      m++;
      break;
      }
    }
    VphiClikClus.push_back(m);
  }

 return VphiClikClus;
}

