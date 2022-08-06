library(RGEEtools)

start()



ROI = ROI()

temp = wttolist(depth = 0, date = "20100101", interval = 7, ROI = ROI, scale = 100000)
temp = geetodf(temp)
temp$water_temp_0 = (temp$water_temp_0*0.001)+20



geometry = ee$Geometry$Point(c(120.91157001734109, 8.723755466136668))


data = rgee::ee$ImageCollection('HYCOM/sea_temp_salinity')$
  filterDate(i_date, f_date)$select("water_temp_0")$mean()$
  sample(region = geometry, scale = 100 , geometries = TRUE, seed = 10)$getInfo()


