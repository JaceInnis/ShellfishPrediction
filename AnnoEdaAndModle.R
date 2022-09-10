library(tidyverse)
library(lubridate)


mega = read.csv("finalAnno.CSV")

# this scrips firtly removes all the outliers from the mega df. then makes plots of every location, variable and depth. then,
# there is some code to manually make colums for the "lines" data frame. under that is the code used to make the atmo models


 mega = mega[! (mega$Location == "Mindoro" & mega$Variable == "Sal" & mega$Layer ==0 & mega$Value<13000),]
 mega = mega[! (mega$Location == "Mindoro" & mega$Variable == "Velu" & mega$Layer ==0 & mega$Value>1500),]
 mega = mega[! (mega$Location == "Mindoro" & mega$Variable == "Velv" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "sibuyan" & mega$Variable == "Sal" & mega$Layer ==0 & mega$Value<13000),]
 mega = mega[! (mega$Location == "sibuyan" & mega$Variable == "Velu" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "sibuyan" & mega$Variable == "Velv" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "southsibuyan" & mega$Variable == "Sal" & mega$Layer ==0 & mega$Value<12500),]
 mega = mega[! (mega$Location == "southsibuyan" & mega$Variable == "Velu" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "southsibuyan" & mega$Variable == "Velv" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "surigao" & mega$Variable == "Sal" & mega$Layer ==0 & mega$Value<12500),]
 mega = mega[! (mega$Location == "surigao" & mega$Variable == "Velu" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "surigao" & mega$Variable == "Velv" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "tablas" & mega$Variable == "Sal" & mega$Layer ==0 & mega$Value<12500),]
 mega = mega[! (mega$Location == "tablas" & mega$Variable == "Velu" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "tablas" & mega$Variable == "Velv" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "Visayan" & mega$Variable == "Sal" & mega$Layer ==0 & mega$Value<12500),]
 mega = mega[! (mega$Location == "Visayan" & mega$Variable == "Velu" & mega$Layer ==0 & mega$Value>1000),]
 mega = mega[! (mega$Location == "Visayan" & mega$Variable == "Velv" & mega$Layer ==0 & mega$Value>1000),]
 
 
 mega = mega[! (mega$Location == "Mindoro" & mega$Variable == "Sal" & mega$Layer ==1 & mega$Value<13000),]
 mega = mega[! (mega$Location == "Mindoro" & mega$Variable == "Temp" & mega$Layer ==1 & mega$Value>10),]
 mega = mega[! (mega$Location == "Mindoro" & mega$Variable == "Velu" & mega$Layer ==1 & mega$Value<(-1000)),]
 mega = mega[! (mega$Location == "Mindoro" & mega$Variable == "Velv" & mega$Layer ==1 & mega$Value>1000),]
 mega = mega[! (mega$Location == "sibuyan" & mega$Variable == "Sal" & mega$Layer ==1 & mega$Value<14000),]
 mega = mega[! (mega$Location == "sibuyan" & mega$Variable == "Velu" & mega$Layer ==1 & mega$Value<(-1000)),]
 mega = mega[! (mega$Location == "sibuyan" & mega$Variable == "Velv" & mega$Layer ==1 & mega$Value>1000),]
 mega = mega[! (mega$Location == "southsibuyan" & mega$Variable == "Sal" & mega$Layer ==1 & mega$Value<14000),]
 mega = mega[! (mega$Location == "southsibuyan" & mega$Variable == "Velu" & mega$Layer ==1 & mega$Value<(-1000)),]
 mega = mega[! (mega$Location == "southsibuyan" & mega$Variable == "Velv" & mega$Layer ==1 & mega$Value>1000),]
 mega = mega[! (mega$Location == "surigao" & mega$Variable == "Sal" & mega$Layer ==1 & mega$Value<12000),]
 mega = mega[! (mega$Location == "surigao" & mega$Variable == "Velu" & mega$Layer ==1 & mega$Value<(-2000)),]
 mega = mega[! (mega$Location == "surigao" & mega$Variable == "Velv" & mega$Layer ==1 & mega$Value>1000),]
 mega = mega[! (mega$Location == "tablas" & mega$Variable == "Sal" & mega$Layer ==1 & mega$Value<12500),]
 mega = mega[! (mega$Location == "tablas" & mega$Variable == "Velu" & mega$Layer ==1 & mega$Value<(-1000)),]
 mega = mega[! (mega$Location == "tablas" & mega$Variable == "Velv" & mega$Layer ==1 & mega$Value>1000),]

 







mega$date = mdy(mega$date)
mega$day = yday(mega$date)


locs = names(table(mega$Location))
vars = names(table(mega$Variable))



  for (w in 1:5) {
    for (x in 1:4) {
      mega %>% 
        filter(Location == locs[w], Variable == vars[x], Layer == 1) -> gra
      
      plot(Value~day,data=gra,xlab = vars[x], main = locs[w])
    }
  }

#  "Mindoro"      "sibuyan"      "southsibuyan" "surigao"      "tablas"       "Visayan"     
#   "Sal"         "Temp"          "Velu"        "Velv"


w = 1    # location
x = 4   # var



mega %>% 
  filter(Location == locs[w], Variable == vars[x], Layer == 0) -> gra

plot(Value~day, data = gra, xlab = vars[x], main = locs[w])


annolm <- lm(Value  ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
            +sin((4*pi/365)*day)+cos((4*pi/365)*day),data=gra)

annolm <- lm(Value  ~ sin((2*pi/365)*day)+cos((2*pi/365)*day),data=gra)



mod = predict(annolm, data.frame(day = 1:365))

lines(1:365,mod,col=2)

mod = as.data.frame(mod)$mod


lines = cbind(lines, mindorovelv = mod)


write.csv(lines, "modeledAnno.csv")



# starts the atmo code




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





