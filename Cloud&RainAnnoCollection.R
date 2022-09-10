

mega = data.frame(date = "NA", Location = "NA", Variable = "NA", Value = 1.1)

mega = mega[,1:4]


date = ymd(20120106)


i = nrow(mega)

while (TRUE) {
  list = date + runif(1, 1, 3574)
  print(as.character(list))
  
  list = lubridate::ymd(list)
  i_date = as.character(list-7)
  f_date = as.character((list))

  data = ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$
    filterDate( i_date , f_date )$select("tcdc")$mean()$
    sample(region = atmos, scale = 160000 , geometries = TRUE, seed = 10)$getInfo()
  
  mega[i,] = c(as.character(list), "atmo", "cloud" , mean(geetodf(data)$tcdc))
  
  i = i + 1
  
  data = ee$ImageCollection('NASA/GPM_L3/IMERG_V06')$
    filterDate( i_date , f_date )$select("precipitationCal")$mean()$
    sample(region = atmos, scale = 160000 , geometries = TRUE, seed = 10)$getInfo()
  
  mega[i,] = c(as.character(list), "atmo", "rain" , mean(geetodf(data)$precipitationCal))
  
  i = i + 1

}

mega

write.csv(mega, "finalAnnoatmo.CSV")

mega$day = yday(mega$date)





mega %>% 
  filter(Variable == "cloud") -> gra

plot(Value~day, data = gra, xlab = "cloud")

annolm <- lm(Value  ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
             +sin((4*pi/365)*day)+cos((4*pi/365)*day),data=gra)

mod = predict(annolm, data.frame(day = 1:365))

lines(1:365,mod,col=2)

mod = as.data.frame(mod)$mod

mod


lines$cloud = mod

 

mega %>% 
  filter(Variable == "rain") -> gra

plot(Value~day, data = gra, xlab = "cloud")

annolm <- lm(Value  ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
             +sin((4*pi/365)*day)+cos((4*pi/365)*day),data=gra)

mod = predict(annolm, data.frame(day = 1:365))

lines(1:365,mod,col=2)

mod = as.data.frame(mod)$mod


lines$rain = mod


