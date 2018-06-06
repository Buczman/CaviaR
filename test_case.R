library(data.table)
source('caviar.R')
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


