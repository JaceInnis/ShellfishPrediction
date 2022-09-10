library(RGEEtools)

start()


# This script collects a lot of HCOM data at the specified locations in order to fit the yearly sin models to later use for
#  the annomoly data collection. In short, this scrip makes mega. a df with a lot of points from random days. below is the 
# code for non hycom data

# -------------------------- collection function -------------------------






rob = function(x){
  print("a")
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  temp = mean((geetodf(temp)[,3]*0.001)+20)
  addrowm = data.frame(date = as.character(list), Location = x[[5]], Layer = 0, Variable = "Temp", Value = temp)
  

  temp = wstolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  temp = mean((geetodf(temp)[,3]))
  addrowm[2,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 0, Variable = "Sal", Value = temp)
  
  temp = wvtolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  addrowm[3,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 0, Variable = "Velu", Value = mean(geetodf(temp)[,3]))
  addrowm[4,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 0, Variable = "Velv", Value = mean(geetodf(temp)[,4]))
  

  if(x[[2]] == 1){
    print("b")
    
    temp = wttolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    temp = mean((geetodf(temp)[,3]*0.001)+20)
    addrowm[5,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 1, Variable = "Temp", Value = temp)
    
    
    temp = wstolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    temp = mean((geetodf(temp)[,3]))
    addrowm[6,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 1, Variable = "Sal", Value = temp)
    
    temp = wvtolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    addrowm[7,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 1, Variable = "Velu", Value = mean(geetodf(temp)[,3]))
    addrowm[8,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 1, Variable = "Velv", Value = mean(geetodf(temp)[,4]))
    
  }
  return(addrowm)
  }
  



# ----------------------- Colloction loop ---------------------


mega = data.frame(date = "as.ch", Location = NA, Layer = NA, Variable = NA, Value = NA)

date = ymd(20120106)



while (TRUE) {
  list = date + runif(1, 1, 3574)
  print(as.character(list))

  
  for (i in 1:6) {
    print(i)
    rex = rob(MasterLoc[[i]])
    mega = rbind(mega, rex)
  }
  
  
  print((nrow(mega)/44))
}

mega

write.csv(mega, "finalAnno.CSV")



# this is the script to write the atmo anno data


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




