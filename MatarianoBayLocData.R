
library(RGEEtools)
start()


weekdata = read.csv("MatarianoBay.csv")
weekdata$date = mdy(weekdata$date)

plot(weekdata$site)

loc = list(
  clo = list(
    close = ROI("[125.64272812949912, 11.415548780100657],
           [125.67156724082724, 11.20143483948434],
           [125.77731064903037, 11.037038490337952],
           [125.9050267134835, 11.17179645797716],
           [125.69903306113974, 11.512453103212742]"),
    scale = 7000,
    name = "davosclose"),
  far = list(
    far = ROI("[125.73748520957724, 11.544747141750262],
          [125.9929173384835, 11.061299509812086],
          [126.32662705528037, 11.20682331018293],
          [126.03411606895224, 11.609324056071475]"), 
    scale = 10000, 
    name = "davosfar",
    depth = 2000),
  air = list(davosair = ROI("[125.45394917997407, 11.32337196988719],
          [125.45394917997407, 11.126707124629705],
          [125.73959371122407, 11.126707124629705],
          [125.73959371122407, 11.32337196988719]"), 
             scale = 8000,
             name = "davosair"))



df = data.frame(date = rep(NA, nrow(weekdata)), tox = rep(NA, nrow(weekdata)),
                cloud = rep(NA, nrow(weekdata)), rain = rep(NA, nrow(weekdata)), 
                tempc = rep(NA, nrow(weekdata)), salc = rep(NA, nrow(weekdata)),
                veluc = rep(NA, nrow(weekdata)), velvc = rep(NA, nrow(weekdata)), 
                elec = rep(NA, nrow(weekdata)),tempf = rep(NA, nrow(weekdata)), 
                salf = rep(NA, nrow(weekdata)),
                veluf = rep(NA, nrow(weekdata)), velvf = rep(NA, nrow(weekdata)), 
                elef = rep(NA, nrow(weekdata)),
                tempd = rep(NA, nrow(weekdata)), sald = rep(NA, nrow(weekdata)),
                velud = rep(NA, nrow(weekdata)), velvd = rep(NA, nrow(weekdata)))




hycom = function(date, obj, scale, int, depth){
  temp = wttolist(depth = depth, date = date, interval = int, ROI = obj, scale = scale)
  tem = mean((geetodf(temp)[,3]*0.001)+20)
  
  temp = wstolist(depth = depth, date = date, interval = int, ROI = obj, scale = scale)
  sal = mean((geetodf(temp)[,3]))
  
  temp = wvtolist(depth = depth, date = date, interval = int, ROI = obj, scale = scale)
  velu = mean((geetodf(temp)[,3]))
  velv = mean((geetodf(temp)[,4]))
  
  temp = ssetolist(depth = depth, date = date, interval = int, ROI = obj, scale = scale)
  sse = mean((geetodf(temp)[,3]))
  
  return(c(tem, sal, velu, velv, sse))  
}


air = function(date, obj, scale, int){
  
  fidate = as.character(date - int)
  fdate = as.character(date[591])
  
  idate = as.character(date - int)
  date = as.character(date)
  
  temp = ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$
    filterDate( fidate, fdate )$select("tcdc")$mean()$
    sample(region = obj, scale = scale , geometries = TRUE, seed = 10)$getInfo()
  clou = mean((geetodf(temp)[,3]))
  
  temp = ee$ImageCollection('NASA/GPM_L3/IMERG_V06')$
    filterDate( idate , date )$select("precipitationCal")$mean()$
    sample(region = obj, scale = scale , geometries = TRUE, seed = 10)$getInfo()
  rain = mean((geetodf(temp)[,3]))  
  
  return(c(clou, rain))
}


for (i in 591:nrow(weekdata)) {
  
  print(paste(as.character(weekdata[i,1]), i, i/nrow(weekdata)))
  
  df[i,1] = as.character(weekdata[i,1])
  
  df[i,2] = weekdata[i,2]
  
  df[i,3:4] = air(weekdata[i,1], loc[[3]][[1]], loc[[3]][[2]], 7)
  
  # maybe air needs a longer interval
  
  df[i,5:9] = hycom(weekdata[i,1], loc[[1]][[1]], loc[[1]][[2]], 7, 0)
  
  df[i,10:14] = hycom(weekdata[i,1], loc[[2]][[1]], loc[[2]][[2]], 7, 0)
  
  df[i,15:18] = hycom(weekdata[i,1], loc[[2]][[1]], loc[[2]][[2]], 7, loc[[2]][[4]])
  
}


df[90:110,]

tail(df)


write.csv(df, "MatarianoBayMaster.csv")

df = read.csv("MatarianoBayMaster.csv")


for (q in 5:length(df)) {
  me = mean(df[,q])
  sd = sd(df[,q])
  
  for (i in 1:nrow(df)) {
    if(abs(df[i,q]-me)/sd > 6){
      df[i,q] = me
      print(q)
    }
  }
}


rep = colnames(df[,5:18])
for (i in 5:length(df)) {
  plot(df[,i], main = rep[i], xlab = i, type = "l")
}



for (q in c(14,13,10,16, 9)) {
  me = mean(df[,q])
  sd = sd(df[,q])
  
  for (i in 1:nrow(df)) {
    if(abs(df[i,q]-me)/sd > 4.5){
     df[i,q] = me
      print(q)
    }
  }
}


write.csv(df, "davosMasterdata.csv")



# ---------------------------------- ANNO FOR TEMP LOOP ------------------------------

data = read.csv("MatarianoBayMaster.csv")

data$day = yday(mdy(data$date))

data$day[data$day==366] = 365

# =====

annolm <- lm( data[,7]  ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
             +sin((4*pi/365)*day)+cos((4*pi/365)*day),data=data)

mod = predict(annolm, data.frame(day = 1:365))

mod = as.data.frame(mod)$mod

data$tempcano = data[,7] - mod[data$day]

# =====

annolm <- lm( data[,11]  ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
              +sin((4*pi/365)*day)+cos((4*pi/365)*day),data=data)

mod = predict(annolm, data.frame(day = 1:365))

mod = as.data.frame(mod)$mod

data$tempfano = data[,11] - mod[data$day]

# =====

plot(data$rain, type = "l")
plot(data$rain ~ data$day)
points(data$tempfano, col = "red")

lines(1:365,mod,col=2)

mod = as.data.frame(mod)$mod

lines$cloud = mod



plot(data$tempc ~ data$tempcano)



points(data$tempc>28.5 & data$tempcano > 0.3)
plot(data$sitebi, col = "red")


# = temp loop ot get sse data 


elev = data.frame(close = rep(NA, nrow(data)), far = rep(NA, nrow(data)))

data$date = mdy(data$date)



for (i in 100:nrow(data)) {
  
temp = ssetolist(date = data$date[i], interval = 7, ROI = loc[[1]][[1]], scale = loc[[1]][[2]])
elev[i,1] = mean((geetodf(temp)[,3]))


temp = ssetolist(date = data$date[i], interval = 7, ROI = loc[[2]][[1]], scale = loc[[2]][[2]])
elev[i,2] = mean((geetodf(temp)[,3]))

print(i)

}



plot(elev[,1] ~ data$day)
plot(data$site ~ data$day)
plot(elev[,2] )

ssetolist()


colnames(data)
data = cbind(data, elev)



data$far[is.na(data$far)] = mean(data$far, na.rm = TRUE) 

data$close[is.na(data$close)] = mean(data$close, na.rm = TRUE) 


write.csv(data, "MatarianoBayMaster.csv")





