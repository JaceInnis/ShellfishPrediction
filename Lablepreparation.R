library(lubridate)
library(tidyr)
library(dplyr)
int = 7




data = read.csv("lableprep.csv")
labels = read.csv("SiteNames.csv")
data[,1] = c(mdy(data[1:219,1]),dmy(data[220:nrow(data),1]))


sap = data.frame(date = data$date)



startdate = dmy("01012004")

startdate = startdate-(40*int)

saf = data.frame(date = startdate + (int*(0:(992+40))))





for (c in labels$CODE) {
  vec = rep(NA, nrow(data))
  stat = FALSE
  for (z in 1:nrow(data)) {
    for (i in 2:length(data)) {
      el = abs(data[z,i]) == c
      if(is.na(el)){el = FALSE}
      if(el){
        stat = !stat
      }
    }
    vec[z] = stat
  }
  sap = cbind(sap, vec)
}
colnames(sap) = c("date", labels$CODE)

# final data frame

for (c in 2:length(sap)) {
  vec = rep(NA, nrow(saf))
  for (r in 41:nrow(saf)) {
    check = TRUE
    i = 1
    while (check) {
      if(saf$date[r]>=sap$date[i] & saf$date[r]<sap$date[i+1]){
        val = sap[i,c]
        check = FALSE
      }
    i = i+1
    }
    vec[r] = val
  }
  saf = cbind(saf, vec)
  
  print(c)
  print(Sys.time())
}

colnames(saf) = c("date", labels$CODE)


write.csv(saf,"7dayLabels.csv")




for (w in 2:length(saf)) {
  plot(saf[,w] ~ saf$date, main = labels$SITE[w-1], sub = labels$CODE[w-1])
}






