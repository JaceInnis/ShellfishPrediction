


atmodata = data.frame(cloraw = rep(NA, 533), cloanno = rep(NA, 533), rainraw = rep(NA, 533), rainanno = rep(NA, 533))


for (i in 1:533) {
  list = weekdata$date[i]
  list = lubridate::ymd(list)
  i_date = as.character(list-7)
  f_date = as.character((list))
  
  data = ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$
    filterDate( i_date , f_date )$select("tcdc")$mean()$
    sample(region = atmos, scale = 160000 , geometries = TRUE, seed = 10)$getInfo()
  
  atmodata$cloraw[i] = mean(geetodf(data)$tcdc)
  atmodata$cloanno[i] = mean(geetodf(data)$tcdc) -  MasterLoc$atmos$var$cloud[yday(list)]     # anno minus

    
  data = ee$ImageCollection('NASA/GPM_L3/IMERG_V06')$
    filterDate( i_date , f_date )$select("precipitationCal")$mean()$
    sample(region = atmos, scale = 160000 , geometries = TRUE, seed = 10)$getInfo()

    
  atmodata$rainraw[i] = mean(geetodf(data)$precipitationCal)
  atmodata$rainanno[i] = mean(geetodf(data)$precipitationCal) -  MasterLoc$atmos$var$rain[yday(list)]     # anno minus
  
  
  print(i)
}












