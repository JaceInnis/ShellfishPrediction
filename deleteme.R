library(lubridate)


date = read.csv("final.csv")
date = date[,1]
date = dmy(date)

loc = read.csv("ShellfishBulletinDigital.csv")
loc = loc[,1]

wip = read.csv("ReportWip.csv")
wip = wip[-1,]


wip$report = wip$report-1
#wip$code = wip$code - 1


wip$report = date[wip$report]
#wip$code = loc[wip$code]

re = factor(wip$code)

levels(re)


for (i in 1:41) {
  print(
  wip[wip$code==levels(re)[i],]
  )
}
