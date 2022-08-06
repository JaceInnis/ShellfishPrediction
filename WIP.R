library(lubridate)

data = read.csv("final.csv")

data$date = dmy(data$date)

start = ymd("2012-01-06")

end = ymd("2022-03-31")

ndays = (((year(end)-2012)*365)+yday(end))


for (i in 1:ndays) {

  if(i == 1){
    dfram = data.frame(date = ymd(start), site = NA)
  }

x = start + (i-1)
  
dfram[i,1] = x

 for (j in 1:(nrow(data)-1)) {
   if(
     x >= data$date[j] & x < data$date[j+1]
   ){
     w = data$C[j]
   }
 }

dfram[i,2] = w   
}


dfram$week = NA
dfram$year = NA


for (i in 1:nrow(dfram)) {
  dfram$week[i] = week(dfram$date[i])
  dfram$year[i] = year(dfram$date[i])
}


dfram


plot( dfram$site ~ dfram$date, type="l" )


dfram$siteq = (dfram$site/14)

fit = glm( siteq ~ year + week , data=dfram , family = "quasibinomial")
res = predict.glm(fit, dfram[,c(4,3)])
plot( exp(res) ~ dfram$date, type="l" , col = "red")
lines( dfram$siteq ~ dfram$date, type="l"  , col = "blue")


dfram$catweek = as.factor(dfram$week)


fit = glm( siteq ~ year + catweek , data=dfram , family = "quasibinomial")
res = predict.glm(fit, dfram[,c(4,6)])
plot( exp(res) ~ dfram$date, type="l" , col = "red")
lines( dfram$siteq ~ dfram$date, type="l"  , col = "blue")


rep  = coef(fit)

for (i in 3:length(rep)) {
  if(i == 3){
    res = data.frame(week = (i-1), coef = rep[[i]])
  }
  res[i,] = c((i-1),rep[[i]])
}

plot(res$coef~res$week)



5744.17+16180.98+1803-7+9570+745


data[260:330,]
