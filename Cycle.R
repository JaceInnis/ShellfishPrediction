

# there should be a data frame named fada that contains rows of dates and data

fada = read.csv("Final7daydata/irong-irong.csv")

fada = fada[-1:-400,]


fada[is.na(fada[,2]),2] = 0
#every interval
#delay
#tested incomplete
# number of layers
# units in layer
# input variables
# lookback
# drop out


# data conversion
library(keras)

valsplit = 0.7
testsplit = 0.9
trainvec = 1:as.integer(nrow(fada)*valsplit)
valvec = (length(trainvec)+1):as.integer(nrow(fada)*testsplit)





lookback <- 10
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



## \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////
## \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////

# variable selection

thel = list(c(2,3))

for (q in 2:dim(data2)[3]) {
for (i in (1+q):dim(data2)[3]) {
  thel[length(thel)+1] = list(c(q,i))
}
}



thel = list(
  list(c(1:32))
)

params = data.frame(error = NA, layer = NA, Unit = NA, drop = NA, epoch = NA, var = NA)
scar = list(dse)

#(length(thel)-1)
for (w in 2:(length(thel)-1)) {


dat = data2[,,thel[[w]]]


for (d in c(.15,.3,.5)) {

dropout = d

samp = dat[trainvec,,]
labt = lab[trainvec]
val = dat[valvec,,]
vlab = lab[valvec]

p = 1
for (l in rep(1:4, 2)) {
  
  
  if( p < 5){
    model <- keras_model_sequential() %>%
      layer_lstm(units = (6*l), 
                 dropout = dropout, 
                 recurrent_dropout = dropout,
                 input_shape = list(NULL, (dim(dat)[[3]]))) %>%
      layer_dense(units = (12*l) , activation = "sigmoid") %>% 
      layer_dense(units = 1 , activation = "sigmoid") %>% 
      compile(
        optimizer = "rmsprop",
        loss = "Poisson",
        metrics = c(metric_true_positives())
      )
    
  }
  
  
  else{
model <- keras_model_sequential() %>%
  layer_lstm(units = (6*l), 
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



p = p+1

lowerr = 999

param = data.frame(error = NA, epoch = NA)

for (i in 1:60) {
  history <- model %>% fit(
    samp,
    labt,
    epochs = 1,
    shuffle = TRUE)
  
  vres = model %>% predict(val)
  pred = vres
  me = mean((vres-vlab)^2)
  pred[pred>0.5] = 1
  pred[!pred>0.5] = 0
  
  err = (sum(!pred == vlab)) + me # so the validation stat is the number of incorrect guesses 
  # plus the mse
  
  if(err<lowerr){
    lowerr = err
    param$error = err
    param$epoch = i
    dse = vres
  }
  
  #model %>% save_model_hdf5("")
  # plot(labt, col = "red", xlab = NA)
  # points(res) 
  #plot(vlab, col = 'red', xlab = NA)
  #points(pred)
}

params = rbind(params,
               data.frame(error = param$error, layer = p, Unit = l, 
                          drop = d, epoch = param$epoch,
                          var = w))

print(params)

scar[length(scar)+1] = list(dse)

}}}


params[(min(params$error, na.rm = TRUE) == params$error),]


plot(vlab, col = 'red', xlab = NA)
points(scar[[1465]])
print(param$params)
































lookback <- 12
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









 


colnames(fada[-1])
   
  
  dat = data2[,, c(-5,-6)]
  
  
     
    dropout = .1
    
    samp = dat[trainvec,,]
    labt = lab[trainvec]
    val = dat[valvec,,]
    vlab = lab[valvec]
    

    model <- keras_model_sequential() %>%
      layer_lstm(units = (8*2), 
                 dropout = dropout, 
                 recurrent_dropout = dropout,
                 input_shape = list(NULL, (dim(dat)[[3]]))) %>%
      layer_dense(units = (12*4) , activation = "sigmoid") %>% 
      layer_dense(units = 1 , activation = "sigmoid") %>% 
      compile(
        optimizer = "rmsprop",
        loss = "Poisson",
        metrics = c(metric_true_positives())
      )
       
      
    lowerr = 999
      
      param = data.frame(error = NA, epoch = NA)
      
      for (i in 1:60) {
        history <- model %>% fit(
          samp,
          labt,
          epochs = 1,
          shuffle = TRUE)
        
        vres = model %>% predict(val)
        pred = vres
        me = mean((vres-vlab)^2)
        pred[pred>0.5] = 1
        pred[!pred>0.5] = 0
        
        err = (sum(!pred == vlab)) + me # so the validation stat is the number of incorrect guesses 
        # plus the mse
        
        if(err<lowerr){
          lowerr = err
          param$error = err
          param$epoch = i
          dse = vres
        }
        
        #model %>% save_model_hdf5("")
        plot(vlab, col = 'red', xlab = NA)
        points(vres)
      }
      
      params = rbind(params,
                     data.frame(error = param$error, layer = p, Unit = l, 
                                drop = d, epoch = param$epoch,
                                var = w))
      
      print(params)
      
      scar[length(scar)+1] = list(dse)
      
 


params[(min(params$error, na.rm = TRUE) == params$error),]


plot(vlab, col = 'red', xlab = NA)
points(scar[[1465]])
print(param$params)


