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


tail(dfram)

ymd(dfr)

data[1,2]

i1 = start
i2 = start+7



((year(data$date[100])-2012)*365)+yday(data$date[100])








