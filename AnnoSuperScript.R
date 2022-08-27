library(RGEEtools)

start()


# -------------------------- collection function -------------------------






rob = function(x){
  print("a")
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  temp = mean((geetodf(temp)[,3]*0.001)+20)
  addrowm = data.frame(date = as.character(list), Location = x[[5]], Layer = 0, Variable = "Temp", Value = temp)
  

  temp = wstolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  temp = mean((geetodf(temp)[,3]))
  addrowm[2,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 0, Variable = "Sal", Value = temp)
  
  temp = wvtolist(depth = 0, date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
  addrowm[3,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 0, Variable = "Velu", Value = mean(geetodf(temp)[,3]))
  addrowm[4,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 0, Variable = "Velv", Value = mean(geetodf(temp)[,4]))
  

  if(x[[2]] == 1){
    print("b")
    
    temp = wttolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    temp = mean((geetodf(temp)[,3]*0.001)+20)
    addrowm[5,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 1, Variable = "Temp", Value = temp)
    
    
    temp = wstolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    temp = mean((geetodf(temp)[,3]))
    addrowm[6,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 1, Variable = "Sal", Value = temp)
    
    temp = wvtolist(depth = x[[4]], date = list, interval = 7, ROI = x[[1]], scale = x[[3]])
    addrowm[7,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 1, Variable = "Velu", Value = mean(geetodf(temp)[,3]))
    addrowm[8,] = data.frame(date = as.character(list), Location = x[[5]], Layer = 1, Variable = "Velv", Value = mean(geetodf(temp)[,4]))
    
  }
  return(addrowm)
  }
  



# ----------------------- Colloction loop ---------------------


mega = data.frame(date = "as.ch", Location = NA, Layer = NA, Variable = NA, Value = NA)

date = ymd(20120106)



while (TRUE) {
  list = date + runif(1, 1, 3574)
  print(as.character(list))

  
  for (i in 1:6) {
    print(i)
    rex = rob(MasterLoc[[i]])
    mega = rbind(mega, rex)
  }
  
  
  print((nrow(mega)/44))
}

mega

write.csv(mega, "finalAnno.CSV")


