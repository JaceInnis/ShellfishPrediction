


library(stringr)
library(keras)



params = data.frame(site = NA, error = NA, epoch = NA, lookback = NA,
                    var = NA, dropout = NA, layer = NA, mse = NA, cut = NA)

sitname = list.files("Final7daydata/")

sitname = str_sub(sitname, start = 1L, end = -5L) 


for (sit  in sitname) {


keep1 = 99
keep2 = 99
keep3 = 99
stanerr1 = 99
stanerr2 = 99

for (cut in 1:2) {
  
  
  
stri = paste0("Final7daydata/",sit,".csv")
fada = read.csv(stri)


fada[is.na(fada[,2]),2] = fada[41,2]
  
if(cut == 2){
fada = fada[-1:-300,]
}
  


valsplit = 0.8
testsplit = 0.90
trainvec = 1:(as.integer(nrow(fada)-40)*valsplit)
valvec = (length(trainvec)+1):as.integer((nrow(fada)-40)*testsplit)


nam = colnames(fada)[-1:-2]
num = 2:(length(fada)-1)


vars = list(
all = num,
minanno = num[(str_which(nam,"anno")*-1)],
anno = c(num[str_which(nam,"anno")],(length(fada)-1)),
minvel = num[(str_which(nam,"_\\w_")*-1)],
mintemp = num[(str_which(nam,"temp")*-1)],
minsal = num[(str_which(nam,"sal")*-1)]
)



for (lookb in c(2,4,6)) {


lookback <- 6+lookb
delay <- 4
dat = array(NA, dim = c(  length(fada[(-1:-40),2]) , lookback , (length(fada)-1)))
lab = rep(NA, dim(dat)[1])




for (z in 1:dim(dat)[1]) {
  lab[z] = fada[(40 + z),2]
  for (x in 1:lookback) {
    dat[z,x,] = as.numeric(fada[(((41 + z)-delay)-x),-1])
  }
}

data2 = dat


print(lookb)

for (varpo in 1:6) {


dat = data2[,,vars[[varpo]]]
samp = dat[trainvec,,]
labt = lab[trainvec]
val = dat[valvec,,]
vlab = lab[valvec]


for (d in 1:2) {
  
dropout = .1*d

for (l in c(1:9)) {

if(l<4){
model <- keras_model_sequential() %>%
  layer_lstm(units = (18*l), 
             dropout = dropout, 
             recurrent_dropout = dropout,
             input_shape = list(NULL, (dim(dat)[[3]]))) %>%
  layer_dense(units = 1 , activation = "sigmoid") %>% 
  compile(
    optimizer = "rmsprop",
    loss = "Poisson",
    metrics = c(metric_true_positives())
  )
}


if(l>3 & l<7){
  model <- keras_model_sequential() %>%
    layer_lstm(units = (16*(l-3)), 
               dropout = dropout, 
               recurrent_dropout = dropout,
               input_shape = list(NULL, (dim(dat)[[3]]))) %>%
    layer_dense(units = (12*(l-3)) , activation = "sigmoid") %>% 
    layer_dense(units = 1 , activation = "sigmoid") %>% 
    compile(
      optimizer = "rmsprop",
      loss = "Poisson",
      metrics = c(metric_true_positives())
    )
}

if(l>6){
  model <- keras_model_sequential() %>%
    layer_lstm(units = (12*(l-6)), 
               dropout = dropout, 
               recurrent_dropout = dropout,
               return_sequences=TRUE,
               input_shape = list(NULL, (dim(dat)[[3]]))) %>%
    layer_lstm(units = (16*(l-6)) , activation = "sigmoid") %>% 
    layer_dense(units = (10*(l-6)) , activation = "sigmoid") %>% 
    layer_dense(units = 1 , activation = "sigmoid") %>% 
    compile(
      optimizer = "rmsprop",
      loss = "Poisson",
      metrics = c(metric_true_positives())
    )
}


lowerr = 999
param = data.frame(error = NA, epoch = NA)

for (i in 1:60) {
  try({history <- model %>% fit(
    samp,
    labt,
    epochs = 1,
    shuffle = TRUE,
    verbose=0)})
  
  vres = model %>% predict(val)
  pred = vres
  me = mean((vres-vlab)^2)
  pred[pred>0.5] = 1
  pred[!pred>0.5] = 0
  
  err = (sum(!pred == vlab)) + me
  
  if(err<lowerr){
    lowerr = err
    param$error = err
    param$epoch = i
    dse = vres
    model %>% save_model_hdf5("models/catch.hdf5")
  }
  
  plot(vlab, col = 'red', xlab = NA)
  points(vres)
}

keepe = param$error
errore = (param$error - as.integer(param$error))

print(data.frame(site = sit, error = param$error, epoch = param$epoch, lookback = lookb,
                 var = varpo, dropout = d, layer = l, mse = errore, cut = cut))

params = rbind(params,data.frame(site = sit, error = param$error, epoch = param$epoch, lookback = lookb,
                                 var = varpo, dropout = d, layer = l, mse = errore, cut = cut))



if(keepe < keep1){
  keep1 = keepe
  model = load_model_hdf5("models/catch.hdf5")
  stri = paste0("models/",sit,"1.hdf5")
  model %>% save_model_hdf5(stri)
  keepe = 999
}

if(keepe < keep2){
  keep2 = keepe
  stri = paste0("models/",sit,"2.hdf5")
  model %>% save_model_hdf5(stri)
  keepe = 999
}

if(keepe < keep3){
  keep3 = keepe
  stri = paste0("models/",sit,"3.hdf5")
  model %>% save_model_hdf5(stri)
  keepe = 999
}

if(errore < stanerr1){
  stanerr1 = errore
  stri = paste0("models/",sit,"e1.hdf5")
  model %>% save_model_hdf5(stri)
  errore = 999
}

if(errore < stanerr2){
  stanerr2 = errore
  stri = paste0("models/",sit,"e2.hdf5")
  model %>% save_model_hdf5(stri)
  errore = 999
}



print(keep1)


}}}}}}


















params[order(params$try),]





look = 2
var = 3
valsplit = 0.8
testsplit = 1
trainvec = 1:(as.integer(nrow(fada)-40)*valsplit)
valvec = (length(trainvec)+1):as.integer((nrow(fada)-40)*testsplit)

lookback <- 6+look
dat = array(NA, dim = c(  length(fada[(-1:-40),2]) , lookback , (length(fada)-1)))


for (z in 1:dim(dat)[1]) {
  lab[z] = fada[(40 + z),2]
  for (x in 1:lookback) {
    dat[z,x,] = as.numeric(fada[(((41 + z)-delay)-x),-1])
  }
}

  data2 = dat
  dat = data2[,,vars[[var]]]
  samp = dat[trainvec,,]
  labt = lab[trainvec]
  val = dat[valvec,,]
  vlab = lab[valvec]
  


model = load_model_hdf5("models/bataane1.hdf5")
vres = model %>% predict(val)
plot(vlab, col = 'red', xlab = NA)
points(vres)
pred = vres
me = mean((vres-vlab)^2)
pred[pred>0.5] = 1
pred[!pred>0.5] = 0

err = (sum(!pred == vlab)) + me



