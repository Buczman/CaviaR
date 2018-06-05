require(Rcpp)

caviarOptimisation <- function(data, 
                               model = 1, 
                               pval = 0.01, 
                               REP = 5, 
                               MAXITER = 500, 
                               predict = 0){
  # script.dir <- dirname(parent.frame(2)$ofile)
  print(parent.frame(2)$ofile)
  
  
  if (length(data) < 1) {
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
  con <- list(maxit = MAXITER)
  
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

  sourceCpp(paste0(models[model],'.cpp'))
  obs <- length(data)

  VaR <- rep(0, obs)
  Hit <- VaR
  
  emp_qnt <- if (obs < 300){
    quantile(data[1:obs], pval)  
  } else {
    quantile(data[1:300], pval)
  }

  print('STARTING VALUES SIMULATION')  
  RQfval <- apply(initialTargetVectors, 1, RQObjectiveFunction, 1, model, data, obs, pval, emp_qnt)
  
  BestInitialCond <- if (model == 4) {
    matrix(initialTargetVectors[order(RQfval), ][1:nInitialCond],ncol=1)
  } else {
    initialTargetVectors[order(RQfval), ][1:nInitialCond, ]
  }
  
  RQoptim <- cbind(NA, BestInitialCond, NA)
  
  met <- 'Nelder-Mead'
  low <- -Inf
  up <- Inf
  met_hes <- 'BFGS'
  
  if(model == 4){
    met <- "Brent"
    low <- -10
    up <- 10
  }else if(model == 3){
    met_hes <- 'SANN'
  }
  print('STARTING OPTIMIZATION PROCEDURE')  
  for (i in 1:nrow(BestInitialCond)){
    opt <- optim(BestInitialCond[i,], RQObjectiveFunction, 
                 out=1, model=model, data=data, obs=obs, pval=pval, emp_qnt=emp_qnt, 
                 method=met, lower=low, upper=up, control = con)
    RQoptim[i, 1] <- opt$value
    RQoptim[i, 2:(ncol(initialTargetVectors)+1)] <- opt$par
    
    for(j in 1:REP){
      opt <- optim(RQoptim[i, 2:(ncol(initialTargetVectors)+1)],
                   RQObjectiveFunction, out=1, model=model, data=data, obs=obs, pval=pval, emp_qnt=emp_qnt,
                   method=met_hes, control = con)
      opt <- optim(opt$par, RQObjectiveFunction,
                   out=1, model=model, data=data, obs=obs, pval=pval, emp_qnt=emp_qnt,
                   method=met, lower=low, upper=up, control = con)

      if(abs(RQoptim[i, 1] - opt$value)>10000000000){ #Convergence test
        RQoptim[i, 1] <- opt$value
        RQoptim[i, (ncol(initialTargetVectors)+1)] <- opt$par      
      }else{
        RQoptim[i,ncol(RQoptim)] <- j
        break
      }
    }
  }
  
  bestPar <- RQoptim[order(RQoptim[,1]), ][1, 2:(ncol(initialTargetVectors)+1)]
  VaR <- RQObjectiveFunction(bestPar, 2, model, data, obs, pval, emp_qnt, 1)
  VarPredict <- VaR %>% tail(1)
  VaR <- VaR[-length(VaR)]

  if (predict) return(VarPredict)
  else {
    outL <- list(	bestVals =   RQoptim,
                  bestPar =    bestPar,
                  VaR =        VaR,
                  bestRQ =     RQoptim[order(RQoptim[,1]), ][1, 1],
                  VarPredict = VarPredict)
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
                                varPredict = 0){
  if (varPredict) {
    VaR = rep(0,obs+1)
  } else {
    VaR = rep(0,obs)
    Hit = VaR
  }
  
  VaR[1] <- -1*emp_qnt

  
  
  if (model == 1) {

    VaR <- caviar_SAV(beta, data, VaR[1], VaR, obs, varPredict)
    if (!varPredict) Hit <- (data < -VaR) - pval

  } else if (model == 2) {
    
    VaR <- caviar_AS(beta, data, VaR[1], VaR, obs, varPredict)
    if (!varPredict) Hit <- (data < -VaR) - pval
    
  } else if (model == 3) {
    
    VaR <- caviar_GARCH(beta, data, VaR[1], VaR, obs, varPredict)
    if (!varPredict) Hit <- (data < -VaR) - pval
    
  } else if (model == 4) {
    
    VaR <- caviar_ADAPTIVE(pval, k, beta, data, VaR[1], VaR, obs, varPredict)
    if (!varPredict) Hit <- (data < -VaR) - pval
    
  }
  
  if (varPredict) {
    return(VaR)
  } else if (out == 1) {
    RQ  =-1*t(Hit)%*%(data + VaR)
    if (is.infinite(RQ))  RQ = 1e+100;
    return(RQ)
  } else if (out == 2) {
    return(cbind(VaR, Hit))
  }
  
}
