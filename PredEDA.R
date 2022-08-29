# ---------------------- NA all the outliers ---------------------

masterdata = recov



masterdata[[q]][[1]][[w]][468]

for (q in 1:6) {
  for (w in 1:4) {
    i = 1
      me = mean(masterdata[[q]][[1]][[w]], na.rm = TRUE)
      sd = sd(masterdata[[q]][[1]][[w]], na.rm = TRUE)
      
      for (e in c(1:467,469:533)) {
        ra = masterdata[[q]][[1]][[w]][e]
        if(abs((ra-me)/sd) > 4){
          masterdata[[q]][[1]][[w]][e] = NA
          print(i)
          i = i +1
        }
      }
  }
}


for (q in 2:6) {
  for (w in 1:4) {
    i = 1
    me = mean(masterdata[[q]][[2]][[w]], na.rm = TRUE)
    sd = sd(masterdata[[q]][[2]][[w]], na.rm = TRUE)
    
    for (e in c(1:467,469:533)) {
      ra = masterdata[[q]][[2]][[w]][e]
      if(abs((ra-me)/sd) > 4){
        masterdata[[q]][[2]][[w]][e] = NA
        print(i)
        i = i +1
      }
    }
  }
}



cut = masterdata[[2]][[2]][[2]]
res = (cut - mean(cut, na.rm = TRUE))/ sd(cut, na.rm = TRUE)
hist(res)

# -----------------------------------------------------------------

wip = weekdata
wip$site = as.numeric(wip$site)
wip$site = wip$site/14
wip$month = month(wip$date, label = TRUE, abbr = FALSE)
wip$month = as.factor(wip$month)
wip$week = as.factor(wip$week)

lag = 45


msedata = data.frame(location = rep(NA, 44*lag), layer = rep(NA, 44*lag), variabel = rep(NA, 44*lag), lag = rep(NA, 44*lag), mse = rep(NA, 44*lag))

i = 1

varnames = c("temp","sal","velu","velv")

for (q in 1:6) {

    for (w in 1:4) {
      for (e in 1:lag) {
        
        if(q == 1){
          test = cbind(wip[e:533,], var = masterdata[[q]][[1]][[w]][1:(534-e)])
          fit = glm (site ~  var, data = test, family = "quasibinomial")
          fit = mean((fit$residuals^2))
          msedata[i,] = c(MasterLoc[[q]][[5]], 1 ,varnames[w], (e-1), fit)
          
          i = i+1
          
        }
        else{
          test = cbind(wip[e:533,], var = masterdata[[q]][[1]][[w]][1:(534-e)])
          fit = glm (site ~  var, data = test, family = "quasibinomial")
          fit = mean((fit$residuals^2))
          msedata[i,] = c(MasterLoc[[q]][[5]], 1 ,varnames[w], (e-1), fit)
          
          i = i+1
          
          
          test = cbind(wip[e:533,], var = masterdata[[q]][[2]][[w]][1:(534-e)])
          fit = glm (site ~   var, data = test, family = "quasibinomial")
          fit = mean((fit$residuals^2))
          msedata[i,] = c(MasterLoc[[q]][[5]], 2 ,varnames[w], (e-1), fit)
          
          i = i+1
          
        }
      }

  }
}



tail(msedata)


# ----------------------------------- eda on results ------------------------------



for (q in 1:6) {
  for (w in 1:4) {
    msedata %>% 
      filter(location == MasterLoc[[q]][[5]], layer == 1, variabel == varnames[w]) -> las
    plot(mse~ lag, data = las, ylab = varnames[w], main = MasterLoc[[q]][[5]])
    
  }
}
for (q in 2:6) {
  for (w in 1:4) {
    msedata %>% 
      filter(location == MasterLoc[[q]][[5]], layer == 2, variabel == varnames[w]) -> las
    plot(mse~ lag, data = las, ylab = varnames[w], main = paste(MasterLoc[[q]][[5]], "bot"))
    
  }
}

wip



fit = glm (site ~ year + month, data = wip, family = "quasibinomial")
mean((fit$residuals^2))


e = 20 # lag
q = 1 # location
w = 1 # var

test = cbind(wip[e:533,], var = masterdata[[q]][[1]][[w]][1:(534-e)])
fit = glm (site ~  var, data = test, family = "quasibinomial")
res = predict.glm(fit, test)
plot( test$site ~ test$date, type="l" , col = "red")
lines( exp(res) ~ test$date, type="l"  , col = "blue")



fit = mean((fit$residuals^2))
msedata[i,] = c(MasterLoc[[q]][[5]], 1 ,varnames[w], (e-1), fit)



fit = glm( siteq ~ year + week , data=weekdata , family = "quasibinomial")
res = predict.glm(fit, weekdata)
plot( exp(res) ~ weekdata$date, type="l" , col = "red")
lines( weekdata$siteq ~ weekdata$date, type="l"  , col = "blue")


summary(fit)




