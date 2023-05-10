


library(stringr)
library(keras)



params = data.frame(error = NA, epoch = NA, lookback = NA,
                    var = NA, dropout = NA, layer = NA, mse = NA, cut = NA)




cut = 1    #change this
  

    
    
    fada = read.csv("Final7daydata/matariano.csv")
    
    
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
    
    
    
    lookb = 0     # change this 
      
      
      lookback <- 6+lookb
      delay <- 3
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
      
      varpo = 3    #change this
        
        
        dat = data2[,,vars[[varpo]]]
        samp = dat[trainvec,,]
        labt = lab[trainvec]
        val = dat[valvec,,]
        vlab = lab[valvec]
          
          dropout = .1

          
            

          
              model <- keras_model_sequential() %>%
                layer_lstm(units = 120 , 
                           dropout = dropout, 
                           recurrent_dropout = dropout,
                           return_sequences=TRUE,
                           input_shape = list(NULL, (dim(dat)[[3]]))) %>%
                layer_lstm(units = 360, activation = "sigmoid") %>% 
                layer_dense(units = 100 , activation = "sigmoid") %>% 
                layer_dense(units = 1 , activation = "sigmoid") %>% 
                compile(
                  optimizer = "rmsprop",
                  loss = "Poisson",
                  metrics = c(metric_true_positives())
                )
            
            
            

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
              
              plot(vlab, col = 'red', xlab = NA)
              points(vres)
            }
            
        
        
        