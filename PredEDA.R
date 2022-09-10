
load("masterdata.Rdata")

load("realmasterdata.Rdata")


# ---------------------- NA all the outliers ---------------------

weekdata$day = yday(weekdata$date)


plot(realmastdat$southsibuyan$top$velu$raw~weekdata$day)




for (q in 1:6) {
  for (w in 1:4) {
    for (s in 1:2) {
    i = 1
      me = mean(realmastdat[[q]][[1]][[w]][[s]], na.rm = TRUE)
      sd = sd(realmastdat[[q]][[1]][[w]][[s]], na.rm = TRUE)
      
      for (e in c(1:467,469:533)) {
        ra = realmastdat[[q]][[1]][[w]][[s]][e]
        if(abs((ra-me)/sd) > 4){
          realmastdat[[q]][[1]][[w]][[s]][e] = NA
          print(i)
          i = i +1
        }
      }
    }
  }
}


for (q in 2:6) {
  for (w in 1:4) {
    i = 1
    me = mean(realmastdat[[q]][[2]][[w]], na.rm = TRUE)
    sd = sd(realmastdat[[q]][[2]][[w]], na.rm = TRUE)
    
    for (e in c(1:467,469:533)) {
      ra = realmastdat[[q]][[2]][[w]][e]
      if(abs((ra-me)/sd) > 4){
        realmastdat[[q]][[2]][[w]][e] = NA
        print(i)
        i = i +1
      }
    }
  }
}


# this chunk just makes sure that the 
cut = realmastdat[[2]][[1]][[2]][[1]]
res = (cut - mean(cut, na.rm = TRUE))/ sd(cut, na.rm = TRUE)
hist(res)

# -----------------------------------------------------------------





wip = weekdata
wip$site = as.numeric(wip$site)
wip$site = wip$site/14
wip$month = month(wip$date, label = TRUE, abbr = FALSE)
wip$month = as.factor(wip$month)
wip$week = as.factor(wip$week)

lag = 75


msedata = data.frame(location = rep(NA, 44*lag), layer = rep(NA, 44*lag), variabel = rep(NA, 44*lag), lag = rep(NA, 44*lag), mse = rep(NA, 44*lag))
sigcode = data.frame(location = rep(NA,44), layer = rep(NA,44), variabel = rep(NA,44), lag = rep(NA,44), tvalue = rep(NA,44))


i = 1
d = 1

varnames = c("temp","sal","velu","velv")

for (q in 1:6) {

    for (w in 1:4) {
      for (e in 1:lag) {
        
        if(q == 1){
          
          if(e ==1){
            cambio = c(0,0)
          }
          
          test = cbind(wip[166:533,], var = masterdata[[q]][[1]][[w]][(167-e):(534-e)])
          fit = glm (site ~  month + var, data = test, family = "quasibinomial")
          der = c(summary(fit)$coefficients[13,3],e)
          if(abs(der[1])>abs(cambio[1])){
            cambio = der
          }
          fit = mean((fit$residuals^2))
          msedata[i,] = c(MasterLoc[[q]][[5]], 1 ,varnames[w], (e-1), fit)
          
          i = i+1
          
        }
        else{
          if(e ==1){
            cambio = c(0,0)
            casio = c(0,0)
          }
          test = cbind(wip[166:533,], var = masterdata[[q]][[1]][[w]][(167-e):(534-e)])
          fit = glm (site ~  month + var, data = test, family = "quasibinomial")
          der = c(summary(fit)$coefficients[13,3], e)
          if(abs(der[1])>abs(cambio[1])){
            cambio = der
          }
          fit = mean((fit$residuals^2))
          msedata[i,] = c(MasterLoc[[q]][[5]], 1 ,varnames[w], (e-1), fit)
          
          i = i+1
          
          
          test = cbind(wip[166:533,], var = masterdata[[q]][[2]][[w]][(167-e):(534-e)])
          fit = glm (site ~    month + var, data = test, family = "quasibinomial")
          dec = c(summary(fit)$coefficients[13,3], e)
          if(abs(dec[1])>abs(casio[1])){
            casio = dec
          }
          fit = mean((fit$residuals^2))
          msedata[i,] = c(MasterLoc[[q]][[5]], 2 ,varnames[w], (e-1), fit)
          
          i = i+1
          
        }
      }
    if(q ==1){
      sigcode[d,] = c(MasterLoc[[q]][[5]], 1 ,varnames[w], cambio[2], cambio[1])
      d = d + 1
    }
      else{
        sigcode[d,] = c(MasterLoc[[q]][[5]], 1 ,varnames[w], cambio[2], cambio[1])
        d = d + 1
        
        
        sigcode[d,] = c(MasterLoc[[q]][[5]], 2 ,varnames[w], casio[2], casio[1])
        d = d + 1
        
      }

  }
}



tail(msedata)

sigcode


# ----------------------------------- eda on results ------------------------------



for (q in 1:6) {
  for (w in 1:4) {
    
    subte = paste(
      sigcode$lag[sigcode$location == MasterLoc[[q]][[5]] & sigcode$layer == 2 & sigcode$variabel == varnames[w]],
      sigcode$tvalue[sigcode$location == MasterLoc[[q]][[5]] & sigcode$layer == 2 & sigcode$variabel == varnames[w]]
    )
    
    
    msedata %>% 
      filter(location == MasterLoc[[q]][[5]], layer == 1, variabel == varnames[w]) -> las
    plot(mse~ lag, data = las, ylab = varnames[w], main = MasterLoc[[q]][[5]], sub = subte)
    
  }
}
for (q in 2:6) {
  for (w in 1:4) {
    
     subte = paste(
      sigcode$lag[sigcode$location == MasterLoc[[q]][[5]] & sigcode$layer == 2 & sigcode$variabel == varnames[w]],
      sigcode$tvalue[sigcode$location == MasterLoc[[q]][[5]] & sigcode$layer == 2 & sigcode$variabel == varnames[w]]
    )

    msedata %>% 
      filter(location == MasterLoc[[q]][[5]], layer == 2, variabel == varnames[w]) -> las
    plot(mse~ lag, data = las, ylab = varnames[w], main = paste(MasterLoc[[q]][[5]], "bot"), sub = subte)
    
  }
}


msedata[msedata$mse == min(msedata$mse),]


msedata %>% 
  arrange(mse)



# ------------------------ muilti var -----------------------------

combo  = wip

lobo = cbind(wip[166:533,], var1 = masterdata[[5]][[1]][[1]][(167-12):(534-12)],
             var2 = masterdata[[1]][[1]][[1]][(167-20):(534-20)], 
             var3 = masterdata[[4]][[2]][[1]][(167-51):(534-51)], 
             var4 = masterdata[[1]][[1]][[1]][(167-17):(534-17)], 
             var5 = masterdata[[4]][[1]][[2]][(167-51):(534-51)])

lobo = lobo[,c(1,7,8,9 , 10, 11)]

combo = right_join(x = combo, y = lobo, by = "date")





fit = glm (site ~  month , data = combo, family = "quasibinomial")
mean((fit$residuals^2))
res = predict.glm(fit, combo)
plot( exp(res) ~ test$date, type="l" , col = "red")
lines( test$site ~ test$date, type="l"  , col = "blue")



summary(fit)




# ------------------------ eda on var w/out date ------------------------

e = 40 # lag
q = 6 # location
w = 1 # var

test = cbind(wip[166:533,], var = masterdata[[q]][[1]][[w]][(167-e):(534-e)])
fit = glm (site ~  month + var, data = test, family = "quasibinomial")
mean((fit$residuals^2))
res = predict.glm(fit, test)
plot( test$site ~ test$date, type="l" , col = "red")
lines( exp(res) ~ test$date, type="l"  , col = "blue")

summary(fit)$coefficients[14,3]

fit = mean((fit$residuals^2))
msedata[i,] = c(MasterLoc[[q]][[5]], 1 ,varnames[w], (e-1), fit)



fit = glm( siteq ~ year + week , data=weekdata , family = "quasibinomial")
res = predict.glm(fit, weekdata)
plot( exp(res) ~ weekdata$date, type="l" , col = "red")
lines( weekdata$siteq ~ weekdata$date, type="l"  , col = "blue")


summary(fit)




