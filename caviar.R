require(Rcpp)
require(magrittr)

fSourceLocal <- function(){
  
  # Function to return sourced file location inside to 
  # be used inside caviarOptim, where sourceCpp works
  
  # I do know that it is not well written and precise,
  # but for the sake that the function is only in one file,
  # and will be used within other scripts, I decided the issue
  # will be tackled with that way
  # 
  # Moreover this is in any way a package or a class, hence
  # among tools that I know this was the only one
  
  for (i in -(1:sys.nframe())) {
    if (identical(sys.function(i), base::source)){
      path <- normalizePath(sys.frame(i)$ofile)
    }
  }
  
  # if path found return it, else NULL
  if (exists("path")){
    return(dirname(path))
  } else {
    return(NULL)  
  }
  
}

# unfortunately this variable must stay in the scope
# for the purposes of caviarOptim
caviarOptim.currDirr <- fSourceLocal()

caviarOptim <- function(data, 
                        model = 1, 
                        pval = 0.01,
                        k = 5,
                        REP = 5, 
                        MAXITER = 500, 
                        predict = F){

  # This function was created on the basis of Robert F. Engle & Simone Manganelli,
  # 2004. "CAViaR: Conditional Autoregressive Value at Risk by Regression Quantiles,"
  # Journal of Business & Economic Statistics, American Statistical Association,
  # vol. 22, pages 367-381, October.
  # 
  # Its aim is to calculate CAViaR values, based on provided data, selected model and
  # value at risk probability level. To know more about CAViaR please refer to the cited
  # article or an article of my own: Buczynski M., Chlebus M., "Is CAViaR model really
  # so good in Value at Risk forecasting? Evidence from evaluation of a quality
  # of Value-at-Risk forecasts obtained based on the: GARCH(1,1), GARCH-t(1,1),
  # GARCH-st(1,1), QML-GARCH(1,1), CAViaR and the historical simulation models
  # depending on the stability of financial markets" Working Papers 2017-29,
  # Faculty of Economic Sciences, University of Warsaw.
  # 
  # 
  # Input:
  # 
  # - data         - numeric         - data for the CAViaR model to calculate risk
  #                  (convertible to numerical)
  # - model        - numeric         - model type (1 - SAV, 2 - AS, 3 - GARCH, 4 - ADAPTIVE) (defaults to 1)
  # - pval         - numeric         - level of value at risk probability (defaults to 0.01)
  # - REP          - numeric         - number of hessian optimization procedure repeats
  # - MAXITER      - numeric         - maximum number of optimization procedure iterations
  # - predict      - logical         - whether to return only predicted value for next period or whole list
  # - k            - numeric         - optional parameter for ADAPTIVE model
  # 
  # Output:
  # - outL        - list:
  #   - bestVals     - numeric         - best cost function values
  #   - bestPar      - numeric         - best parameters
  #   - VaR          - numeric         - calculated Value at Risk
  #   - bestRQ       - numeric         - best cost function among all
  #   - VarPredict   - numeric         - predicted value at risk for the next period (tail(VaR,1))

  if (length(data) < 2) {
    stop("data should be series of at least 300 length")
  } else if (length(data) < 300) {
    warning(paste0("for the best empirical quantile estimation, it is suggested to provide data of at least 300 length. Only ", 
                   length(data), " samples provided"))
  }
  
  tryCatch(data <- as.numeric(data), 
           error = function(e){
             print("Can't convert to numeric! Provide data convertible to numerical!")
             stop()
  })
  
  models <- c('SAV','AS','GARCH','ADAPTIVE')

  if (!model %in% c(1,2,3,4)) {
    stop('Wrong MODEL selected')
  } else if (model == 1 || model == 3) {
    initialTargetVectors <- matrix(runif(10000*3),ncol=3)
    nInitialCond = 10
  } else if (model == 2) {
    initialTargetVectors <- matrix(runif(10000*4),ncol=4)
    nInitialCond = 15
  } else {
    initialTargetVectors <- matrix(runif(10000),ncol=1)
    nInitialCond = 5
  }

  sourceCpp(paste0(caviarOptim.currDirr, "/", models[model], '.cpp'))
  obs <- length(data)

  VaR <- rep(0, obs)
  Hit <- VaR
  
  emp_qnt <- if (obs < 300){
    quantile(data[1:obs], pval)  
  } else {
    quantile(data[1:300], pval)
  }

  RQfval <- apply(initialTargetVectors, 1, RQObjectiveFunction, 1, model, data, obs, pval, emp_qnt, k)
  
  BestInitialCond <- if (model == 4) {
    matrix(initialTargetVectors[order(RQfval), ][1:nInitialCond], ncol=1)
  } else {
    initialTargetVectors[order(RQfval), ][1:nInitialCond, ]
  }
  
  RQoptim <- cbind(NA, BestInitialCond, NA)
  
  met <- 'Nelder-Mead'
  low <- -Inf
  up <- Inf
  met_hes <- 'BFGS'
  con <- list(maxit = MAXITER)

  # @TODO change optimization procedure to the best automatically
  if (model == 4) {
    met <- "Brent"
    low <- -10
    up <- 10
  }
  if (model == 3) {
    met_hes <- 'SANN'
  }
  
  for (i in 1:nrow(BestInitialCond)) {
    # initial optimization procedure
    vOptim <- optim(BestInitialCond[i,], 
                    RQObjectiveFunction, 
                    out = 1, 
                    model = model, 
                    data = data, 
                    obs = obs, 
                    pval = pval, 
                    emp_qnt = emp_qnt,
                    k = k,
                    method = met, 
                    lower = low, 
                    upper = up, 
                    control = con)
      
    RQoptim[i, 1] <- vOptim$value
    RQoptim[i, 2:(ncol(initialTargetVectors)+1)] <- vOptim$par
    
    # repeating optim on hessian optimiziation to get more accurate results
    for(j in 1:REP){
      # once optimizing previous best param (using BFGS)
      vOptim <- optim(RQoptim[i, 2:(ncol(initialTargetVectors)+1)],
                      RQObjectiveFunction, 
                      out = 1,
                      model = model,
                      data = data,
                      obs = obs,
                      pval = pval,
                      emp_qnt = emp_qnt,
                      k = k,
                      method = met_hes,
                      control = con)
      # secondly optimizing previous best param with afore method
      vOptim <- optim(vOptim$par,
                      RQObjectiveFunction,
                      out = 1,
                      model = model,
                      data = data,
                      obs = obs,
                      pval = pval,
                      emp_qnt = emp_qnt,
                      k = k,
                      method = met,
                      lower = low,
                      upper = up,
                      control = con)
      
      #checking whether hessian optimization converged
      if(abs(RQoptim[i, 1] - vOptim$value)>10000000000){ 
        RQoptim[i, 1] <- vOptim$value
        RQoptim[i, (ncol(initialTargetVectors)+1)] <- vOptim$par      
      } else {
        RQoptim[i,ncol(RQoptim)] <- j
        break
      }
    }
  }
  
  bestPar <- RQoptim[order(RQoptim[,1]), ][1, 2:(ncol(initialTargetVectors)+1)]
  VaR <- RQObjectiveFunction(bestPar, 2, model, data, obs, pval, emp_qnt, k, T)
  VarPredict <- VaR %>% tail(1)
  VaR <- VaR[-length(VaR)]

  if (predict) 
    return(VarPredict)
  else {
    outL <- list(bestVals   = RQoptim,
                 bestPar    = bestPar,
                 VaR        = VaR,
                 bestRQ     = RQoptim[order(RQoptim[,1]), ][1, 1],
                 VarPredict = VarPredict
                 )
    return(outL)
  }
  
}

RQObjectiveFunction <- function(beta,
                                out,
                                model,
                                data,
                                obs,
                                pval,
                                emp_qnt,
                                k = 5,
                                varPredict = F){
  
  #This is optimization function, that uses underlying cpp files to calculate VaR.
  #It comes in two main modes:
  # - out = 1, where it returns RQ (cost function) value
  # - out = 2, where it returns calculated VaR and Hit values
  
  if (varPredict) {
    VaR = rep(0, obs + 1)
  } else {
    VaR = rep(0, obs)
    Hit = VaR
  }
  
  VaR[1] <- -1*emp_qnt

  VaR <- if (model == 1) {
    caviar_SAV(beta, data, VaR[1], VaR, obs, varPredict)
  } else if (model == 2) {
    caviar_AS(beta, data, VaR[1], VaR, obs, varPredict)
  } else if (model == 3) {
    caviar_GARCH(beta, data, VaR[1], VaR, obs, varPredict)
  } else if (model == 4) {
    caviar_ADAPTIVE(k, pval, beta, data, VaR[1], VaR, obs, varPredict)
  }
  
  if (!varPredict){
    Hit <- (data < -VaR) - pval
    if (out == 1) {
      RQ = -1*t(Hit) %*% (data + VaR)
      if (is.infinite(RQ))
        RQ = 1e+100
      return(RQ)
    } else if (out == 2) {
      return(cbind(VaR, Hit))
    }
  } else if (varPredict) {
    return(VaR)
  }
  
}

