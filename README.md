# RCaviar

## R code for estimation of CAViaR models.

  This function was created on the basis of Robert F. Engle & Simone Manganelli,
  2004. "CAViaR: Conditional Autoregressive Value at Risk by Regression Quantiles,"
  Journal of Business & Economic Statistics, American Statistical Association,
  vol. 22, pages 367-381, October.

  Its aim is to calculate CAViaR values, based on provided data, selected model and
  value at risk probability level. To know more about CAViaR please refer to the cited
  article or an article of my own: Buczynski M., Chlebus M., "Is CAViaR model really
  so good in Value at Risk forecasting? Evidence from evaluation of a quality
  of Value-at-Risk forecasts obtained based on the: GARCH(1,1), GARCH-t(1,1),
  GARCH-st(1,1), QML-GARCH(1,1), CAViaR and the historical simulation models
  depending on the stability of financial markets" Working Papers 2017-29,
  Faculty of Economic Sciences, University of Warsaw.


  ### **Input**:

  - data         - numeric         - data for the CAViaR model to calculate risk
                   (convertible to numerical)
  - model        - numeric         - model type (1 - SAV, 2 - AS, 3 - GARCH, 4 - ADAPTIVE) (defaults to 1)
  - pval         - numeric         - level of value at risk probability (defaults to 0.01)
  - REP          - numeric         - number of hessian optimization procedure repeats
  - MAXITER      - numeric         - maximum number of optimization procedure iterations
  - predict      - logical         - whether to return only predicted value for next period or whole list
  - k            - numeric         - optional parameter for ADAPTIVE model

  ### **Output**:
  - outL        - list:
    - bestVals     - numeric         - best cost function values
    - bestPar      - numeric         - best parameters
    - VaR          - numeric         - calculated Value at Risk
    - bestRQ       - numeric         - best cost function among all
    - VarPredict   - numeric         - predicted value at risk for the next period (tail(VaR,1))
