
date = ymd(20120106)

list = date + runif(600, 1, 3574)


# ------------------the chunk below should be ran in the first instance ---------------------

# temp = wstolist(depth = 10, date = "20100101", interval = 7, ROI = surigaostrait, scale = 10000)
# temp = mean((geetodf(temp)$salinity_10*0.001)+20)
# temp = data.frame(date = "2010-01-01", temp = temp)
# red = temp
# red = red[,1:2]

while (TRUE) {
  list = date + runif(1, 1, 3574)
  temp = wstolist(depth = 10, date = list, interval = 7, ROI = surigaostrait, scale = 10000)
  temp = mean((geetodf(temp)$salinity_10*0.001)+20)
  red[(nrow(red)+1),] = c(as.character(list), temp)
  print(nrow(red))
}

red$day = yday(red$date)

write.csv(red, "salANNOavg.csv")

#red = read.csv("tempANNOavg.csv")

red = red[!red$temp<33,]

SSTlm <- lm(temp ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
            +sin((4*pi/365)*day)+cos((4*pi/365)*day),data=red)

plot(temp~day,data=red)

mods = predict(SSTlm, data.frame(day = 1:365))

lines(1:365,mods,col=2)

mods = as.data.frame(mods)

mods = mods$mods


min(mod)

