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


dfram$week = week(dfram$date)
dfram$year = year(dfram$date)



#---------------------------------------- create weekdata frame ------------------------------





for (i in 1:(as.integer(nrow(dfram)/7)-1)) {
  
  if(i == 1 ){
    weekdata = data.frame(
      date =  as.character(dfram$date[((i+1)*7)]), 
      site = mean(dfram$site[((i*7)+1):((i+1)*7)]) ,
      weekn = i
    )
  }
  else{
    weekdata[i,] = c(as.character(dfram$date[((i+1)*7)]),mean(dfram$site[((i*7)+1):((i+1)*7)]),i)
  }

  print(i)
  
}


weekdata$date = ymd(weekdata$date)

weekdata$week = week(weekdata$date)
weekdata$year = year(weekdata$date)
weekdata



# --------------------------Collect the gee data for the above table---------------

library(RGEEtools)

start()


surigaostrait = ROI("[124.83301362364946, 9.855460853817496],
          [124.83301362364946, 9.563079212595822],
          [125.37134370177446, 9.563079212595822],
          [125.37134370177446, 9.855460853817496]")


weekdata$temp0 = NA
weekdata$temp1 = NA
weekdata$temp2 = NA
weekdata$temp3 = NA
weekdata$temp4 = NA
weekdata$temp5 = NA
weekdata$temp6 = NA
weekdata$temp7 = NA
weekdata$temp8 = NA


for (i in 1:nrow(weekdata)) {
  
  weekdata$date[i]
  
  
  if(weekdata$date[i] > ymd("20130327") & weekdata$date[i] < ymd("20130403")){
    temp = wttolist(depth = 10, date = weekdata$date[i], interval = 10, ROI = surigaostrait, scale = 10000)
  }
  else{
    temp = wttolist(depth = 10, date = weekdata$date[i], interval = 7, ROI = surigaostrait, scale = 10000)
  }
  
  temp = mean((geetodf(temp)$water_temp_10*0.001)+20)
  temp = mod[yday(weekdata$date[i])] - temp
  
  

  weekdata$temp0[i] = temp
  weekdata$temp1[i+1] = temp
  weekdata$temp2[i+2] = temp
  weekdata$temp3[i+3] = temp
  weekdata$temp4[i+4] = temp
  weekdata$temp5[i+5] = temp
  weekdata$temp6[i+6] = temp
  weekdata$temp7[i+7] = temp
  weekdata$temp8[i+8] = temp
  
    
  print(weekdata$date[i])
}

tail(weekdata)

weekdata = weekdata[!is.na(weekdata$temp0),]
weekdata = weekdata[!is.na(weekdata$temp8),]

# -------------------------- GML -------------------------







plot( weekdata$site ~ weekdata$date, type="l" )

weekdata = weekdata[!weekdata$week==53,]

weekdata$site = as.numeric(weekdata$site)

weekdata$siteq = (weekdata$site/14)

weekdata$week = as.factor(weekdata$week)

weekdata$month = as.factor(month(weekdata$date))

month


#--- week ---


fit = glm( siteq ~ year + week , data=weekdata , family = "quasibinomial")
res = predict.glm(fit, weekdata)
plot( exp(res) ~ weekdata$date, type="l" , col = "red")
lines( weekdata$siteq ~ weekdata$date, type="l"  , col = "blue")


summary(fit)


#--- month ---

fit = glm( siteq ~ year + month + temp5 , data=weekdata , family = "quasibinomial")
res = predict.glm(fit, weekdata)
plot( exp(res) ~ weekdata$date, type="l" , col = "red")
lines( weekdata$siteq ~ weekdata$date, type="l"  , col = "blue")

summary(fit)

fit$

glm


# ------------------------------------------ EDA on Coefficents -------------------------------------------

rep  = coef(fit)

for (i in 3:length(rep)) {
  if(i == 3){
    res = data.frame(week = (i-1), coef = rep[[i]])
  }
  res[i,] = c((i-1),rep[[i]])
}

plot(res$coef~res$week)


