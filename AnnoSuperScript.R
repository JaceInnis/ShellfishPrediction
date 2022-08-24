




mega = data.frame(date = NA, Location = NA, Layer = NA, Variable = NA, Value = NA)

date = ymd(20120106)



while (TRUE) {
  list = date + runif(1, 1, 3574)
  
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  temp = wttolist(depth = 0, date = list, interval = 7, ROI = a, scale = 8000)
  temp = mean((geetodf(temp)$water_temp_0*0.001)+20)
  mega[nrow(mega)+1,] = data.frame(list, "Visayan",1, "Temp", temp)
  
  print(nrow(red))
}



temp = wstolist(depth = 10, date = date, interval = 7, ROI = f, scale = 13000)
nrow(geetodf(temp))

