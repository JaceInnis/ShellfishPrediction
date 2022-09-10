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



# ------------------------------------- data dataframe -----------------------------

 




write.csv(weekdata, "weekdata.csv")





