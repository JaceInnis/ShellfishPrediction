
weekdata


MasterLoc




masterdata = list(
  Visayan = list(top = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  southsibuyan = list(top = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533)), bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  sibuyan = list(top = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533)), bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  tablas  = list(top = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533)), bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  surigao  = list(top = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533)), bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  mindoro  = list(top = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533)), bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533)))
)



rob = function(x){
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  tem = mean((geetodf(temp)[,3]*0.001)+20)
  tem = tem - x$var$temp[[yday(list)]]

  temp = wstolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  sal = mean((geetodf(temp)[,3]))
  sal = sal - x$var$Sal[[yday(list)]]
  
  temp = wvtolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  velu = mean((geetodf(temp)[,3]))
  velv = mean((geetodf(temp)[,4]))
  velu = velu - x$var$Velu[[yday(list)]]
  velv = velv - x$var$Velv[[yday(list)]]
  
  if(x[[2]] == 1){
    
    temp = wttolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    temd = mean((geetodf(temp)[,3]*0.001)+20)
    
    temp = wstolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    sald = mean((geetodf(temp)[,3]))
    
    temp = wvtolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    velud = mean((geetodf(temp)[,3]))
    velvd = mean((geetodf(temp)[,4]))
    
    return(c(tem, sal, velu, velv, temd, sald, velud, velvd ))
  }
  else{
    return(c(tem, sal, velu, velv))
  }
}


#63
#468
for (i in 468) {
  list = weekdata$date[i]
  print(list)
  for (z in 1:6) {
    print(MasterLoc[[z]][[5]])
    robres = rob(MasterLoc[[z]])
    
    if(z == 1){
      for (q in 1:4) {
        masterdata[[1]]$top[[q]][[i]] = robres[q]
      }
    }
      else{
        for (q in 1:4) {
          masterdata[[z]]$top[[q]][[i]] = robres[q]
          masterdata[[z]]$bot[[q]][[i]] = robres[q+4]
        }
      }
  }
  print(i/533)
  print("-------------------")
  }


weekdata$day = yday(weekdata$date)



for (q in 2:6) {
    for (e in 1:4) {
      plot(masterdata[[q]][[1]][[e]]~ weekdata$day)
      
    }
}




save(masterdata, file = "masterdata.Rdata")

