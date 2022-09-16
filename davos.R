library(RGEEtools)
start()


weekdata = read.csv("Davos.csv")
weekdata$date = mdy(weekdata$date)

plot(weekdata$site)

loc = list(
  clo = list(
close = ROI("[126.28030573889231, 6.814652675116069],
    [126.28030573889231, 6.696005840484114],
    [126.37094294592356, 6.696005840484114],
    [126.37094294592356, 6.814652675116069]"),
scale = 7000,
name = "davosclose"),
far = list(
far = ROI("[126.47852052151643, 6.817379845324491],
    [126.47852052151643, 6.38902801502897],
    [126.72571290432893, 6.38902801502897],
    [126.72571290432893, 6.817379845324491]"), 
scale = 7000, 
name = "davosfar",
depth = 2000),
air = list(davosair = ROI("  [125.97375843364307, 7.077609560455569],
    [125.97375843364307, 6.641303079622775],
    [126.54779407817432, 6.641303079622775],
    [126.54779407817432, 7.077609560455569]"), 
scale = 13000,
name = "davosair"))



df = data.frame(date = rep(NA, nrow(weekdata)), tox = rep(NA, nrow(weekdata)),
                cloud = rep(NA, nrow(weekdata)), rain = rep(NA, nrow(weekdata)), 
                tempc = rep(NA, nrow(weekdata)), salc = rep(NA, nrow(weekdata)),
                veluc = rep(NA, nrow(weekdata)), velvc = rep(NA, nrow(weekdata)), 
                tempf = rep(NA, nrow(weekdata)), salf = rep(NA, nrow(weekdata)),
                veluf = rep(NA, nrow(weekdata)), velvf = rep(NA, nrow(weekdata)), 
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

  return(c(tem, sal, velu, velv))  
}

air = function(date, obj, scale, int){
  idate = as.character(date - int)
  date = as.character(date)
  
  temp = ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$
    filterDate( idate, date )$select("tcdc")$mean()$
    sample(region = obj, scale = scale , geometries = TRUE, seed = 10)$getInfo()
  clou = mean((geetodf(temp)[,3]))
  
  temp = ee$ImageCollection('NASA/GPM_L3/IMERG_V06')$
    filterDate( idate , date )$select("precipitationCal")$mean()$
    sample(region = obj, scale = scale , geometries = TRUE, seed = 10)$getInfo()
  rain = mean((geetodf(temp)[,3]))  
    
  return(c(clou, rain))
}



for (i in 83:84) {
  
  print(paste(as.character(weekdata[i,1]), i, i/nrow(weekdata)))
  
  df[i,1] = as.character(weekdata[i,1])

  df[i,2] = weekdata[i,2]
  
  df[i,3:4] = air(weekdata[i,1], loc[[3]][[1]], loc[[3]][[2]], 7)
  
  df[i,5:8] = hycom(weekdata[i,1], loc[[1]][[1]], loc[[1]][[2]], 7, 0)

  df[i,9:12] = hycom(weekdata[i,1], loc[[2]][[1]], loc[[2]][[2]], 7, 0)

  df[i,13:16] = hycom(weekdata[i,1], loc[[2]][[1]], loc[[2]][[2]], 7, loc[[2]][[4]])
  
}




df = read.csv("davosMasterdata.csv")



for (q in 3:length(df)) {
  me = mean(df[,q])
  sd = sd(df[,q])
  
  for (i in 1:nrow(df)) {
    if(abs(df[i,q]-me)/sd > 6){
      df[i,q] = me
      print(q)
    }
  }
}


for (q in 11:12) {
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


