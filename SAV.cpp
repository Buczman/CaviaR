#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector caviar_SAV(NumericVector BETA, NumericVector y, double empiricalQuantile, NumericVector VaR, int RowsOfy, int varPredict)
{
  int i;
  
  /* Initialize output variables */
  VaR[0] = empiricalQuantile;
  
  /* Start the loop */
  for(i = 1; i < RowsOfy; i++)
  {
    // Symmetric Absolute Value
    VaR[i] = BETA[0] + BETA[1] * VaR[i-1] + BETA[2] * (y[i-1]*(y[i-1]>0) - y[i-1]*(y[i-1]<0));
    
  }
  
  if (varPredict == 1){
    VaR[RowsOfy] = BETA[0] + BETA[1] * VaR[RowsOfy-1] + BETA[2] * (y[RowsOfy-1]*(y[RowsOfy-1]>0) - y[RowsOfy-1]*(y[RowsOfy-1]<0));
  } 
  return VaR;
}




