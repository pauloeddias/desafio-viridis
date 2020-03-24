library(lubridate)
library(bsts)
library(dplyr)
library(ggplot2)
library(forecast)

### Load the data
ctrain <- read.csv("ctrain.csv")
ctest <- read.csv("ctest.csv") 
c_all <- rbind(ctrain,ctest)
### Fit the ARIMA model
arima <- arima(ctrain$data_mean_global, 
               order=c(1, 2, 1), 
               seasonal=list(order=c(1,0,1), period=12))

### Actual versus predicted

d1 <- data.frame(c(fitted(arima), # fitted and predicted
                   predict(arima, n.ahead = 420)$pred),
                 as.numeric(c_all$data_mean_global), #actual values
                 c_all$datetime)

names(d1) <- c("Fitted", "Actual", "Date")

write.csv(d1,"carbon_arima.csv")

### MAPE (mean absolute percentage error)
MAPE <- filter(d1, year(Date)>1980) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))

MAPE*100

### Plot actual versus predicted
#ggplot(data=d1, aes(x=Date)) +
#  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
#  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
#  theme_bw() + theme(legend.title = element_blank()) + 
#  ylab("") + xlab("") +
#  geom_vline(xintercept=as.numeric(as.Date("1750-01-01")), linetype=2) +
#  ggtitle(paste0("ARIMA -- Holdout MAPE = ", round(100*MAPE,2), "%")) + 
#  theme(axis.text.x=element_text(angle = -90, hjust = 0))
