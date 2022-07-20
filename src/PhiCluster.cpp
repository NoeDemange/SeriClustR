#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List PhiClustercpp(NumericMatrix mat, double R, int k) {
  int nrow = mat.nrow();
  Environment base = Environment::namespace_env("base");
  Function table = base["table"];

  NumericVector Vclus;
  for(int i=1; i<nrow-1; i++){
    NumericVector Vmati = mat(i,_);
    NumericVector Vmat1i = mat(i-1,_);
    NumericVector Vmati1 = mat(i+1,_);
    
    NumericVector T1 = table(Vmat1i, Vmati);
    NumericVector Tab1 = T1/sum(T1);
    double a1 = Tab1[0];
    double b1 = Tab1[1];
    double c1 = Tab1[2];
    double d1 = Tab1[3];
    double Phi1 = (a1 - (a1 + b1) * (a1 + c1))/sqrt((a1 + b1) * (c1 + d1) * (a1 + c1) * (b1 + d1));

    if(Phi1<R){k=k+1;}
    Vclus.push_back(k);
  }
  List L = List::create(Named("Vclus") = Vclus, _["k"] = k);
  return L;
}

