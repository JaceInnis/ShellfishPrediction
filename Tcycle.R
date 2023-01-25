library(keras)


fada = read.csv("Final7daydata/matariano.csv")


fada[is.na(fada[,2]),2] = fada[41,2]




valsplit = 0.8
testsplit = 0.90
trainvec = 1:as.integer(nrow(fada)*valsplit)
valvec = (length(trainvec)+1):as.integer(nrow(fada)*testsplit)

  
  lookback <- 6
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
  

  dat = data2[,,c(-1)]
  samp = dat[trainvec,,]
  labt = lab[trainvec]
  val = dat[valvec,,]
  vlab = lab[valvec]
  dropout = .15
    

        model <- keras_model_sequential() %>%
          layer_lstm(units = (18), 
                     dropout = dropout, 
                     recurrent_dropout = dropout,
                     input_shape = list(NULL, (dim(dat)[[3]]))) %>%
          #layer_dense(units = (96) , activation = "sigmoid") %>% 
          layer_dense(units = 1 , activation = "sigmoid") %>% 
          compile(
            optimizer = "rmsprop",
            loss = "Poisson",
            metrics = c(metric_true_positives())
          )

      param = data.frame(error = NA, epoch = NA)
      lowerr = 9999
      for (i in 1:2) {
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
        
        err = (sum(!pred == vlab)) + me
        
        if(err<lowerr){
          lowerr = err
          param$error = err
          param$epoch = i
          dse = vres
          print(err)
          model %>% save_model_hdf5("models/catch.hdf5")
        }
        
        plot(vlab, col = 'red', xlab = NA)
        points(vres)
      }
      
      

      
      
      
      model = load_model_hdf5("models/matariano.hdf5")
      
      plot(vlab, col = 'red', xlab = NA)
      vres = model %>% predict(val)
      points(vres, col = 'blue')
      
      
      model %>% save_model_hdf5("models/matariano.hdf5")
      
      
      
      
      
      
      plot(vlab, col = 'red', xlab = NA)
      points(vres, col = 'green')
      newman = load_model_hdf5("models/catch.hdf5")
      vres = newman %>% predict(val)
      points(vres, col = 'blue')
      
      newman = model
      
      newman %>% save_model_hdf5("models/catch.hdf5")
      
      
      
      
      
      
      
      
      
      
      keepe = param$error
      
      params = rbind(params,data.frame(error = param$error, epoch = param$epoch, lookback = lookb,
                                       var = po, dropout = d, layer = l, try = jn))
      
      
      
      if(keepe < keepef){
        keepef = keepe
        modelk %>% save_model_hdf5("models/matariano.hdf5")
      }
      
      


















dat = data2[,,c(-1,-18)]

val = dat[valvec,,]
vlab = lab[valvec]

model = load_model_hdf5("models/matariano.hdf5")

vres = model %>% predict(val)

pred = vres
me = mean((vres-vlab)^2)
pred[pred>0.5] = 1
pred[!pred>0.5] = 0

err = (sum(!pred == vlab)) + me


plot(vlab, col = 'red', xlab = NA)
points(vres)


hist(params$error, )
