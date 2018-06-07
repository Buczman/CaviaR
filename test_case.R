library(data.table)
source('caviar.R')
library(microbenchmark)
data <- fread('test_case.csv')
data[, logret := log(indeks/shift(indeks, 1, type='lag'))]

caviar <- caviarOptim(data[5000:.N,logret])

caviar$bestVals
#  [,1]       [,2]       [,3]     [,4] [,5]
#  [1,] 0.3549710 0.02106695 -0.1490612 1.240709    1
#  [2,] 0.3549990 0.02111399 -0.1507999 1.237332    1
#  [3,] 0.3550274 0.02091154 -0.1436567 1.245788    1
#  [4,] 0.3549660 0.02110489 -0.1503366 1.240317    1
#  [5,] 0.3549659 0.02110502 -0.1503449 1.240319    1
#  [6,] 0.3549821 0.02102770 -0.1459618 1.239477    1
#  [7,] 0.3549659 0.02110503 -0.1503451 1.240319    1
#  [8,] 0.3550288 0.02063165 -0.1343349 1.245138    1
#  [9,] 0.3549660 0.02110508 -0.1503471 1.240319    1
# [10,] 0.3549693 0.02110265 -0.1502495 1.240635    1

caviar$bestPar
# [1]  0.02110502 -0.15034490  1.24031898

tail(caviar$VaR)
# [1] 0.04133456 0.01886556 0.02421253 0.03195978 0.03104959 0.02367271

# length of VaR is equal to the length of data
length(caviar$VaR)
# [1] 1043
length(data[5000:.N,logret])
# [1] 1043

# whereas varPredict is VaR for the next period
caviar$VarPredict
# [1] 0.01805231

microbenchmark(caviarOptim(data[5000:.N,logret],1),
               caviarOptim(data[5000:.N,logret],2),
               caviarOptim(data[5000:.N,logret],3),
               caviarOptim(data[5000:.N,logret],4),
               times = 5
               )
# With source cpps loaded
#             Unit: milliseconds
#                                              expr       min        lq      mean    median        uq       max neval
#   SAV       caviarOptim(data[5000:.N, logret], 1)  700.1505  750.6235  779.4809  794.0617  818.6963  833.8726     5
#   AS        caviarOptim(data[5000:.N, logret], 2) 1037.1660 1042.7767 1069.4377 1050.0679 1107.0141 1110.1639     5
#   GARCH     caviarOptim(data[5000:.N, logret], 3) 1141.1666 1147.9098 1209.1223 1218.8887 1242.9981 1294.6482     5
#   ADAPTIVE  caviarOptim(data[5000:.N, logret], 4) 1094.4244 1144.2377 1160.9489 1176.5494 1184.6566 1204.8765     5

# Without source cpps loaded +- 3 s slower per each first calculation
#             Unit: milliseconds
#                                              expr       min        lq     mean    median        uq      max neval
#   SAV       caviarOptim(data[5000:.N, logret], 1)  738.8247  761.1415 1363.354  788.7325  812.1219 3715.948     5
#   AS        caviarOptim(data[5000:.N, logret], 2) 1025.2370 1081.2675 1756.967 1184.6326 1215.5393 4278.160     5
#   GARCH     caviarOptim(data[5000:.N, logret], 3) 1182.6648 1200.8437 1909.535 1278.3613 1288.6174 4597.186     5
#   ADAPTIVE  caviarOptim(data[5000:.N, logret], 4) 1132.9430 1133.2657 1748.118 1143.4592 1147.0464 4183.877     5

#ADVANCED
library(xts)
library(ggplot2)
varpredict <- rollapplyr(data[5000:.N,logret], length(data[5000:.N,logret]) - 250, caviarOptim, 1, 0.01, predict = 1) %>% lag

ggplot() + 
  geom_line(aes(x = 1:250, y = tail(data[5000:.N,logret],250), col = "Index log return")) + 
  geom_line(aes(x = 1:251, y = -1*varpredict, col = "CAVIAR VaR"))+
  scale_color_manual(values=c("#ff0000", "#0000ff")) + 
  ylab("Values")+
  xlab("Dates")


