library(RGEEtools)

start()

tabla = ROI("[121.61231859512498, 12.067362107514418],
          [121.61231859512498, 11.77713025527187],
          [121.84577806778123, 11.77713025527187],
          [121.84577806778123, 12.067362107514418]")


# ------------------the chunk below should be ran in the first instance ---------------------

temp = wstolist(depth = 10, date = "2010-01-01", interval = 1, ROI = tabla , scale = 10000)
temp = mean((geetodf(temp)$xx))
temp = data.frame(date = "2010-01-01", temp = temp)
red = temp
red = red[,1:2]

while (TRUE) {
  
  
  list = date + runif(1, 1, 3574)
  temp = wstolist(depth = 10, date = list, interval = 1, ROI = tabla, scale = 10000)
  temp = mean((geetodf(temp)$salinity_10))
  red[(nrow(red)+1),] = c(as.character(list), temp)
  print(nrow(red))
}
