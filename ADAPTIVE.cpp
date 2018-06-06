#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector caviar_ADAPTIVE(double K, double THETA, NumericVector BETA, NumericVector y, double empiricalQuantile, NumericVector VaR, int RowsOfy, int varPredict)
{
  int i;
  
  /* Initialize output variables */
  VaR[0] = empiricalQuantile;
  
  /* Start the loop */
  for(i = 1; i < RowsOfy; i++)
  {
    // Adaptive
    VaR[i] = VaR[i-1] + BETA[0] * (1/(1 + exp(K*(y[i-1]+ VaR[i-1]))) - THETA);		
    
  }
  
  if (varPredict == 1){
    VaR[RowsOfy] = VaR[RowsOfy-1] + BETA[0] * (1/(1 + exp(K*(y[RowsOfy-1]+ VaR[RowsOfy-1]))) - THETA);		
  } 
  return VaR;
}
