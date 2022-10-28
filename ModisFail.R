
library(RGEEtools)
start()




mydate = '2016-12-31'


anofunction = function (date){
  doi = ee$Date(date)
  
  t1 = doi$advance(-15,'day')
  
  t2 = doi$advance(-75,'day')
  
  clim1 = ee$ImageCollection('NASA/OCEANDATA/MODIS-Terra/L3SMI')$filterDate(t2,t1)$select('chlor_a')

  clim2 = ee$ImageCollection('NASA/OCEANDATA/MODIS-Aqua/L3SMI')$filterDate(t2,t1)$select('chlor_a')

  
  clim3 = ee$ImageCollection(clim1$merge(clim2))
  
  clim = ee$ImageCollection(clim3)$median()
  
  
  chl1 = ee$ImageCollection('NASA/OCEANDATA/MODIS-Terra/L3SMI')$filterDate(t1,doi)$select('chlor_a')
  
  chl2 = ee$ImageCollection('NASA/OCEANDATA/MODIS-Aqua/L3SMI')$filterDate(t1,doi)$select('chlor_a')

  
  chl3 = ee$ImageCollection(chl1$merge(chl2))
  
  chl = ee$ImageCollection(chl3)$median()
  
  
  anomo = chl$subtract(clim)
  
  return(anomo)
}



function(date){
  
}



data$date

tim = rep(NA, length(data$date))

for (i in 534:length(tim)) {
  
print(i)
print(length(tim)/i)
  
set = anofunction(as.character(data$date[i]))

sempi = set$sample(region = loc$far$far, scale = 4000 , geometries = TRUE, seed = 10)$getInfo()


tim[i] = mean(geetodf(sempi)$chlor_a)

}




plot(tim, type = "l")
points(data$site[!is.na(tim)])
points(data$site[min:max]~data$date[min:max])


var myseq = ee.List.sequence(-315,0,5);

var mydates = myseq.map(function(number){return ee.Date(mydate).advance(number,'day')});

var visto2 = mydates.map(anofunction);

print(visto2);

var visto3 = ee.ImageCollection(mydates.map(anofunction));












