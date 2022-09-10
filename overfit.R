

library(ggplot2)

wip = weekdata
wip$site = as.numeric(wip$site)
wip$site = wip$site/14
wip$month = month(wip$date, label = TRUE, abbr = FALSE)
wip$month = as.factor(wip$month)
wip$week = as.factor(wip$week)


resfra = data.frame(location = NA, var = NA, Aux = NA, lag = 1, mse = 1.1, tval = 1.1)


locats = c("Vis","SouSib","Sib","Tab","Sur","Min")
varnames = c("temp","sal","velu","velv")
atmova = c("clo","rain")
rawano = c("anno", "raw")


roll = function(t){
for (w in 1:length(t)) {
  if(isTRUE(t[w])){z = w}
}
return(z)
}















i = 1

for (p in 1:150) {
  for (q in 1:6) {
    for (w in 1:4) {
      for (e in 1:2) {
        test = cbind(wip[166:533,], var = realmastdat[[q]][[1]][[w]][[e]][(167-p):(534-p)])
        fit = glm (site ~  month + var, data = test, family = "quasibinomial")
        der = summary(fit)$coefficients[13,3]
        fit = mean((fit$residuals^2))
        
        resfra[i,] = c(locats[q], varnames[w], rawano[e], p, fit, der)
        i = i+1
        
      }
  
      if(q>1 & q<7){
        test = cbind(wip[166:533,], var =realmastdat[[q]][[2]][[w]][(167-p):(534-p)])
        fit = glm (site ~  month + var, data = test, family = "quasibinomial")
        der = summary(fit)$coefficients[13,3]
        fit = mean((fit$residuals^2))
        
        resfra[i,] = c(locats[q], varnames[w], "bot", p, fit, der)
        i = i+1
        
      }
    }
  }
  for (r in 1:2) {
    for (t in 1:2) {
      test = cbind(wip[166:533,], var =realmastdat[[7]][[r]][[t]][(167-p):(534-p)])
      fit = glm (site ~  month + var, data = test, family = "quasibinomial")
      der = summary(fit)$coefficients[13,3]
      fit = mean((fit$residuals^2))
      
      resfra[i,] = c("Atmo", atmova[r], rawano[t], p, fit, der)
      i = i+1
      
    }
  }
}

resfra$mse = as.numeric(resfra$mse)
resfra$tval = as.numeric(resfra$tval)
resfra$lag = as.integer(resfra$lag)

best = resfra[resfra$mse == min(resfra$mse),1:4]



locats = c("Vis","SouSib","Sib","Tab","Sur","Min")
varnames = c("temp","sal","velu","velv")
atmova = c("clo","rain")
rawano = c("anno", "raw")


locats[]


if(best[1,1] == "Atmo"){
  cut =realmastdat[[7]][[roll(atmova==best[1,2])]][[roll(rawano==best[1,3])]][(167-best[[1,4]]):(534-best[[1,4]])] 
}
if(best[1,3] == "bot"){
  cut =realmastdat[[7]][[roll(atmova==best[1,2])]][[roll(rawano==best[1,3])]][(167-best[[1,4]]):(534-best[[1,4]])] 
}
if(){
  cut =realmastdat[[7]][[roll(atmova==best[1,2])]][[roll(rawano==best[1,3])]][(167-best[[1,4]]):(534-best[[1,4]])] 
  
}


















hist(resfra$mse)


ggplot(data = resfra, aes(abs(lag), mse))+
  geom_point(aes(color = Aux))

plot(mse~lag, data = resfra)

plot(
realmastdat$sibuyan$top$sal$raw ~ wip$day)





plot(site~date, data = wip[166:533,], type = "l")

plot( exp(res) ~ test$date, type="l" , col = "red")
lines( test$site ~ test$date, type="l"  , col = "blue")



abs(-3)

test = test[, c(2,7,8)]

test = cbind(wip[166:533,], var = realmastdat[[q]][[1]][[w]][[e]][(167-p):(534-p)])
fit = glm (test[,1] ~ test[,2:length(test)], family = "quasibinomial")
der = summary(fit)$coefficients[13,3]
fit = mean((fit$residuals^2))

listoffactors <- c("month","var")
reformulate(termlabels = listoffactors, response = "site")
y ~ factor1 + factor2






































roll = function()

resfra = data.frame(location = NA, var = NA, Aux = NA, lag = 1, mse = 1.1, tval = 1.1)

i = 1

for (p in 1:100) {
  for (q in 1:6) {
    for (w in 1:4) {
      for (e in 1:2) {
        test = cbind(wip[166:533,], var = realmastdat[[q]][[1]][[w]][[e]][(167-p):(534-p)])
        fit = glm (site ~  month + var, data = test, family = "quasibinomial")
        der = summary(fit)$coefficients[13,3]
        fit = mean((fit$residuals^2))
        
        resfra[i,] = c(q, w, e, p, fit, der)
        i = i+1
        
      }
      
      if(q>1 & q<7){
        test = cbind(wip[166:533,], var =realmastdat[[q]][[2]][[w]][(167-p):(534-p)])
        fit = glm (site ~  month + var, data = test, family = "quasibinomial")
        der = summary(fit)$coefficients[13,3]
        fit = mean((fit$residuals^2))
        
        resfra[i,] = c(q, w, 3, p, fit, der)
        i = i+1
        
      }
    }
  }
  for (r in 1:2) {
    for (t in 1:2) {
      test = cbind(wip[166:533,], var =realmastdat[[7]][[r]][[t]][(167-p):(534-p)])
      fit = glm (site ~  month + var, data = test, family = "quasibinomial")
      der = summary(fit)$coefficients[13,3]
      fit = mean((fit$residuals^2))
      
      resfra[i,] = c(7, r, t, p, fit, der)
      i = i+1
      
    }
  }
}

best = resfra[resfra$mse == min(resfra$mse),1:4]


if(best[1,1] == 7){
  cut =realmastdat[[7]][[best[1,2]]][[best[1,3]]][(167-best[[1,4]]):(534-best[[1,4]])] 
}
if(best[1,3] == 3){
  cut =realmastdat[[best[1,1]]][[2]][[best[1,2]]][(167-best[[1,4]]):(534-best[[1,4]])] 
}
if(best[1,1]<7 & best[1,3]<2){
  cut =realmastdat[[best[1,1]]][[1]][[best[1,2]]][[best[1,3]]][(167-best[[1,4]]):(534-best[[1,4]])] 
}


