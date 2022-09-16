load("realmasterdata.Rdata")
weekdata = read.csv("weekdata.csv")
locats = c("Vis","SouSib","Sib","Tab","Sur","Min")
varnames = c("temp","sal","velu","velv")
atmova = c("clo","rain")
rawano = c("anno", "raw")
rawanoa = c("raw", "anno")


# -------------------------------creates gen data frame------------------------------------

gendata = data.frame(weekdata$site)

colnames = c("resp")

for (q in 1:6) {
  for (w in 1:4) {
    for (e in 1:2) {
      gendata = cbind(gendata, realmastdat[[q]][[1]][[w]][[e]])
      colnames = c(colnames, paste0(locats[q],varnames[w],rawano[e]))
      
    }
    if(q>1 & q<7){
      gendata = cbind(gendata, realmastdat[[q]][[2]][[w]])
      colnames = c(colnames, paste0(locats[q],varnames[w],"deep"))
    }
  }
}
for (r in 1:2) {
  for (t in 1:2) {
    gendata = cbind(gendata, realmastdat[[7]][[r]][[t]])
    colnames = c(colnames, paste0("atmo",atmova[r],rawanoa[t]))
    
  }
}
colnames(gendata) = colnames

#gendata$Visveluraw

# --------------------------------NA data anal-----------------------------------

# gendata = na.omit(gendata)




f = 0
for (i in 1:nrow(gendata)) {
  if(sum(is.na(gendata[i,]))>0){
    f = f + 1
  print(paste(sum(is.na(gendata[i,])), i))
    }
}
f


f = 0
for (i in 1:nrow(gendata)) {
  for(w in 1:ncol(gendata)){
    if(is.na(gendata[i,w])){
      f = f + 1
    }
  }}
f


# ------------------------------- normalizes numbers ------------------------------------




train_data <- gendata[1:300,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(gendata, center = mean, scale = std)




write.csv(data, "prepeddata.csv")


# -------------------------------------------------------------------



