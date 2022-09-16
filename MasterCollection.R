
weekdata = read.csv("weekdata.csv")


MasterLoc

# this script makes a large list to host the data that corresponds to the master weekdata dataframe. after making the list 
# there is a data collection function 'rob' and then the loop to collect the data. 
# Below that I have a new list that extends the data to include the raw data and to include atmo data


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

yday(weekdata$date[468])


ymd(weekdata$date[468]) - 1



 
 for (i in 468) {
  list = ymd(weekdata$date[i]) - 1
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

# ------------------------------- atmo data collection -------------------------------
atmodata = data.frame(cloraw = rep(NA, 533), cloanno = rep(NA, 533), rainraw = rep(NA, 533), rainanno = rep(NA, 533))



for (i in 1:533) {
  list = weekdata$date[i]
  list = lubridate::ymd(list)
  i_date = as.character(list-7)
  f_date = as.character((list))
  
  data = ee$ImageCollection('NOAA/NCEP_DOE_RE2/total_cloud_coverage')$
    filterDate( i_date , f_date )$select("tcdc")$mean()$
    sample(region = atmos, scale = 160000 , geometries = TRUE, seed = 10)$getInfo()
  
  atmodata$cloraw[i] = mean(geetodf(data)$tcdc)
  atmodata$cloanno[i] = mean(geetodf(data)$tcdc) -  MasterLoc$atmos$var$cloud[yday(list)]     # anno minus
  
  
  data = ee$ImageCollection('NASA/GPM_L3/IMERG_V06')$
    filterDate( i_date , f_date )$select("precipitationCal")$mean()$
    sample(region = atmos, scale = 160000 , geometries = TRUE, seed = 10)$getInfo()
  
  
  atmodata$rainraw[i] = mean(geetodf(data)$precipitationCal)
  atmodata$rainanno[i] = mean(geetodf(data)$precipitationCal) -  MasterLoc$atmos$var$rain[yday(list)]     # anno minus
  
  
  print(i)
}

# ------------------------------- retrieve non anomolys data ----------------------------




realmastdat = list(
  Visayan = list(top = list(temp = list(anno = rep(NA, 533), raw = rep(NA, 533)), sal = list(anno = rep(NA, 533), raw = rep(NA, 533)), velu = list(anno = rep(NA, 533), raw = rep(NA, 533)), velv = list(anno = rep(NA, 533), raw = rep(NA, 533)))),
  southsibuyan = list(top = list(temp = list(anno = rep(NA, 533), raw = rep(NA, 533)), sal = list(anno = rep(NA, 533), raw = rep(NA, 533)), velu = list(anno = rep(NA, 533), raw = rep(NA, 533)), velv = list(anno = rep(NA, 533), raw = rep(NA, 533))),
                      bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  sibuyan = list(top = list(temp = list(anno = rep(NA, 533), raw = rep(NA, 533)), sal = list(anno = rep(NA, 533), raw = rep(NA, 533)), velu = list(anno = rep(NA, 533), raw = rep(NA, 533)), velv = list(anno = rep(NA, 533), raw = rep(NA, 533))),
                 bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  tablas  = list(top = list(temp = list(anno = rep(NA, 533), raw = rep(NA, 533)), sal = list(anno = rep(NA, 533), raw = rep(NA, 533)), velu = list(anno = rep(NA, 533), raw = rep(NA, 533)), velv = list(anno = rep(NA, 533), raw = rep(NA, 533))),
                 bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  surigao  = list(top = list(temp = list(anno = rep(NA, 533), raw = rep(NA, 533)), sal = list(anno = rep(NA, 533), raw = rep(NA, 533)), velu = list(anno = rep(NA, 533), raw = rep(NA, 533)), velv = list(anno = rep(NA, 533), raw = rep(NA, 533))),
                  bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533))),
  mindoro  = list(top = list(temp = list(anno = rep(NA, 533), raw = rep(NA, 533)), sal = list(anno = rep(NA, 533), raw = rep(NA, 533)), velu = list(anno = rep(NA, 533), raw = rep(NA, 533)), velv = list(anno = rep(NA, 533), raw = rep(NA, 533))),
                  bot = list(temp = rep(NA, 533), sal = rep(NA, 533), velu = rep(NA, 533), velv = rep(NA, 533)))
)




for (i in 1:6) {
    for (w in 1:4) {
      realmastdat[[i]][[1]][[w]][[1]][468] = masterdata[[i]][[1]][[w]][468]
      realmastdat[[i]][[1]][[w]][[2]][468] = masterdata[[i]][[1]][[w]][468]   +   MasterLoc[[i]]$var[[w]][365]
  }
}


for (i in 2:6) {
  for (w in 1:4) {
    realmastdat[[i]][[2]][[w]][468] = masterdata[[2]][[2]][[1]][468]
  }
}








for (t in 1:533) {
  for (i in 1:6) {
    for (w in 1:4) {
      for (r in 1:2) {
        if(  is.na(realmastdat[[i]][[1]][[w]][[r]][t]) ){
          if(r == 1){
            realmastdat[[i]][[1]][[w]][[r]][t] = 0
          }
          else{
            realmastdat[[i]][[1]][[w]][[r]][t] = MasterLoc[[i]]$var[[w]][yday(weekdata$date[t])]
            }
          }
        }
      }
    }
  for (i in 2:6) {
    for (w in 1:4) {
      realmastdat[[i]][[2]][[w]][t] = mean(realmastdat[[i]][[2]][[w]], na.rm = TRUE)
    }
  }
}






 
  for (i in 1:6) {
    for (w in 1:4) {
      for (r in 1:2) {
        plot(realmastdat[[i]][[1]][[w]][[r]], main = locats[i], ylab = varnames[w], xlab = rawano[r])
      }
    }
  }
  
  
  for (i in 2:6) {
    for (w in 1:4) {
      plot(realmastdat[[i]][[2]][[w]], main = locats[i], ylab = varnames[w], xlab = rawano[r])
    }
  }
 




plot(
realmastdat$southsibuyan$bot$velu)


save(realmastdat, file = "realmasterdata.Rdata")


realmastdat








for (t in 1:533) {
  for (i in 1:6) {
    for (w in 1:4) {
      realmastdat[[i]][[1]][[w]][[1]][468] = masterdata[[i]][[1]][[w]][468]
      realmastdat[[i]][[1]][[w]][[2]][468] = masterdata[[i]][[1]][[w]][468]   +   MasterLoc[[i]]$var[[w]][365]
    }
  }
  
  
  for (i in 2:6) {
    for (w in 1:4) {
      realmastdat[[i]][[2]][[w]][468] = masterdata[[2]][[2]][[1]][468]
    }
  }
}

