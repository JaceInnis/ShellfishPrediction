

data = read.csv("7dayLabels.csv")[,2]

coln = NA
for (l in 1:length(mlist)) {
  for (v in 1:length(mlist[[l]]$bands)) {
    for (b in 2:length(mlist[[l]]$bands[[v]])) {
     coln = c(coln, paste(nlist[l],mlist[[l]]$bands[[v]][[b]], sep = "_"))
    }
  }
}

coln = coln[-1]


fada = data.frame(matrix(NA, nrow = length(data), ncol = length(coln)))

colnames(fada) = coln

fada = cbind(date = data, fada)


for (v in 1:nrow(fada)) {

  eday = fada[v,1]
  ed = ymd(eday)
  sd = ed - 7
  sday = as.character(sd)
  x = 2
  
  for (l in 1:length(mlist)) {
    for (b in 1:length(mlist[[l]]$bands)) {
      if(length(mlist[[l]]$bands[[b]]) == 3){
        try({temp = ee$ImageCollection(mlist[[l]]$bands[[b]][[1]])$
          filterDate( sday, eday )$mean()$
          select(c(mlist[[l]]$bands[[b]][[2]], mlist[[l]]$bands[[b]][[3]]))$
          sample(region = mlist[[l]]$loc, scale = 9000, geometries = TRUE)$getInfo()})
        
        fada[v,x:(x+1)] = c(try({mean(geetodf(temp)[,3])}), try({mean(geetodf(temp)[,4])}))
        
        x = x + 2
      }
      else{
        try({temp = ee$ImageCollection(mlist[[l]]$bands[[b]][[1]])$
          filterDate( sday, eday )$mean()$
          select(mlist[[l]]$bands[[b]][[2]])$
          sample(region = mlist[[l]]$loc, scale = 9000, geometries = TRUE)$getInfo()})
        
        fada[v,x] = try({mean(geetodf(temp)[,3])})
        
        x = x + 1
      }
      print(x)
    }
    print(v/length(data))
  }

}



for (i in 2:length(fada)) {
  plot(as.numeric(fada[,i]), main = paste0(coln[i-1],"_new"))
}






lab = read.csv('7dayLabels.csv')

write.csv(cbind(date = fada[,1],lab = lab$X61,fada[,2:10]), "7dayData/balite.csv", row.names = FALSE)
write.csv(cbind(date = fada[,1],lab = lab$X131,fada[,11:31]), "7dayData/BolinaoandAnda.csv", row.names = FALSE)
write.csv(cbind(date = fada[,1],lab = lab$X32,fada[,32:47]), "7dayData/matariano.csv", row.names = FALSE)
write.csv(cbind(date = fada[,1],lab = lab$X101,fada[,48:72]), "7dayData/milagros.csv", row.names = FALSE)
write.csv(cbind(date = fada[,1],lab = lab$X41,fada[,73:88]), "7dayData/carigara.csv", row.names = FALSE)
write.csv(cbind(date = fada[,1],lab = lab$X33,fada[,73:88]), "7dayData/cambatutay.csv", row.names = FALSE)
write.csv(cbind(date = fada[,1],lab = lab$X31,fada[,73:88]), "7dayData/irong-irong.csv", row.names = FALSE)
write.csv(cbind(date = fada[,1],lab = lab$X14,fada[,89:104]), "7dayData/bataan.csv", row.names = FALSE)

 

 

