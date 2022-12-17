library(lubridate)
library(tidyr)
library(dplyr)



#--------------------------------------------
# hyper parameters





#--------------------------------------------





date = read.csv("final.csv")
date = dmy(date[,1])

loc = read.csv("ShellfishBulletinDigital.csv")
loc = loc[,1]

wip = read.csv("ReportWip.csv")
wip = wip[-1,]
wip$report = wip$report-1


wip$report = date[wip$report]
wip$code = loc[wip$code - 1]

re = levels(factor(wip$code))

for (i in re) {
  print(
    wip[wip$code == i,]
  )
}

fil = levels(factor(filter(wip, wip$report < dmy("01012014"))$code))

sites = NA
for (i in fil) {
  if(6 < sum(wip[wip$code==i,][,3])){
    print(i)
    print(sum(wip[wip$code==i,][,3])) 
    sites = c(sites, i)
  }
}

sites = sites[-1]






wip = wip %>% 
  filter(code %in% sites)

data = pivot_wider(wip, names_from = code, values_from = IND)

for (i in 2:ncol(data)) {
  pop = 0
  for (t in 1:nrow(data)) {
    if(is.na(data[t,i])){
      data[t,i] = pop
    }
    else{
      pop = data[t,i]
    }
  }
}

data = add_row(data, data[nrow(data),])

data[nrow(data),1] = ymd("20221201")



#--------------------------------------------
# the above script takes the three .csv and output's a df of the specified sites with important dates
#--------------------------------------------


# checks for leap days

startdate = ymd(20120101)

startdates = startdate + (0:569 * 7)

predates = startdate + (-1 * 7 * 39:1)

wdata = data.frame(date = (startdates[1]), data[1,-1])

for (t in 1:length(startdates)) {
  check = TRUE
  i = 2
    while (check) {
    if(startdates[t] < data[i,1]){
      wdata = add_row(wdata, data.frame(date = startdates[t], data[i,-1]))
      check = FALSE
    }
    i = i + 1
  }
}


pred = data.frame(predates, rep(NA, length(predates)), rep(NA, length(predates))
             , rep(NA, length(predates)), rep(NA, length(predates)), rep(NA, length(predates))
             , rep(NA, length(predates)), rep(NA, length(predates)))

colnames(pred) = colnames(wdata)

wdata = rbind(pred, wdata)

write.csv(wdata, "D7InputData.csv")

#generate every week, add to dataframe. go through data$week until generated week is greater
#than the checked row in data, take previouse' row's values. 




colnames(wdata)

for (i in 2:8) {
plot(wdata[,i] ~ wdata$date )
}


data = read.csv("D7InputData.csv")[,-1]

colnames(data)

sum(data[,4] == data[,3], na.rm = TRUE)
