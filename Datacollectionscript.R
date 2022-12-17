



# function test

library(RGEEtools)


start()

obj = ROI("[120.02539327697872, 14.878475482547286],
          [120.02539327697872, 14.205856570093683],
          [120.98669698791622, 14.205856570093683],
          [120.98669698791622, 14.878475482547286]")

bay = ROI("[120.58844259338497, 14.724459912541365],
          [120.58844259338497, 14.482595767804597],
          [120.93725851135372, 14.482595767804597],
          [120.93725851135372, 14.724459912541365]")

geex = function(band, startdate, enddate, roi){
  
  startdate = as.character(startdate)
  enddate = as.character(enddate)
  
  
  temp = ee$ImageCollection(band)$
    filterDate( startdate, enddate)$mean()$
    sample(region = roi, scale = 120000 , geometries = TRUE)$getInfo()

  return(geetodf(temp))
}


band = "NOAA/CDR/AVHRR/AOT/V3"
startdate = '2018-02-01'
enddate = '2018-03-01'

keep = rep(NA, length(data[,1]))


for (i in 1:length(data[,1])) {
  sday = ymd(data[,1][i])
  eday = sday + 7
  keet =  geex("NOAA/CDR/AVHRR/AOT/V3", sday, eday, obj)
  if(isFALSE(nrow(keet) == 0))
  print(nrow(keet))
  keep[i] = mean(keet[,3])
  
}


sday = as.character(sday)
eday = as.character(eday)

temp = ee$ImageCollection(band)$
  filterDate( sday, eday)$mean()$
  sample(region = obj, scale = 120000 , geometries = TRUE)$getInfo()

return(geetodf(temp))



te =  geex("NOAA/CDR/AVHRR/AOT/V3", sday, eday, obj)


fax = geex("NOAA/CDR/AVHRR/AOT/V3", '2018-02-01', '2018-03-01', obj )

  
  
  
  
  
  
  
  
  
  
  
  

  temp = ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$
    filterDate( '2018-02-01', '2018-03-01' )$select("tcdc")$mean()$
    sample(region = obj, scale = 120000 , geometries = TRUE, seed = 10)$getInfo()
    
  clou = mean((geetodf(temp)[,3]))
  
temp = ee$ImageCollection('NASA/GPM_L3/IMERG_V06')$
  filterDate( idate , date )$select("precipitationCal")$mean()$
  sample(region = obj, scale = scale , geometries = TRUE, seed = 10)$getInfo()
rain = mean((geetodf(temp)[,3]))  

return(c(clou, rain))





rob = function(x){
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  tem = mean((geetodf(temp)[,3]*0.001)+20)
  tem = tem - x$var$temp[[yday(list)]]
  
  temp = wstolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  sal = mean((geetodf(temp)[,3]))
  sal = sal - x$var$Sal[[yday(list)]]
  
  temp = wvtolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  velu = mean((geetodf(temp)[,3]))
  velv = mean((geetodf(temp)[,4]))
  velu = velu - x$var$Velu[[yday(list)]]
  velv = velv - x$var$Velv[[yday(list)]]
  
  if(x[[2]] == 1){
    
    temp = wttolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    temd = mean((geetodf(temp)[,3]*0.001)+20)
    
    temp = wstolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    sald = mean((geetodf(temp)[,3]))










