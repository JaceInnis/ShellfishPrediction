surigaostrait = ROI("[124.83301362364946, 9.855460853817496],
          [124.83301362364946, 9.563079212595822],
          [125.37134370177446, 9.563079212595822],
          [125.37134370177446, 9.855460853817496]")

date = ymd(20120106)

list = date + runif(600, 1, 3574)


# ------------------the chunk below should be ran in the first instance ---------------------

# temp = wttolist(depth = 10, date = "20100101", interval = 7, ROI = surigaostrait, scale = 10000)
# temp = geetodf(temp)
# temp$water_temp_10 = (temp$water_temp_10*0.001)+20
# temp$date = ymd(20100101)
# red = temp

for (i in 1:length(list)) {
  temp = wttolist(depth = 10, date = list[i], interval = 7, ROI = surigaostrait, scale = 10000)
  temp = geetodf(temp)
  temp$water_temp_10 = (temp$water_temp_10*0.001)+20
  temp$date = ymd(list[i])
  red = rbind(red, temp)
  print(i)
  }

red$day = yday(red$date)

#write.csv(red, "tempANNO.csv")

SSTlm <- lm(water_temp_10 ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
            +sin((4*pi/365)*day)+cos((4*pi/365)*day),data=red)

plot(water_temp_10~day,data=red)

res = predict(SSTlm, data.frame(day = 1:365))

lines(1:365,res,col=2)





