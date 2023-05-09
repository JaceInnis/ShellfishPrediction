library(stringr)
library(dplyr)
library(keras)



params = read.csv("results.csv")[,-1]

params[order(params$mse),]







fada = read.csv("Final7daydata/bataan.csv")
fada = fada[-1:-300,]


look = 6
var = 3
delay <- 4
valsplit = 0.8
testsplit = 0.9
trainvec = 1:(as.integer(nrow(fada)-40)*valsplit)
valvec = (length(trainvec)+1):as.integer((nrow(fada)-40)*testsplit)

lookback <- 6+look
dat = array(NA, dim = c(  length(fada[(-1:-40),2]) , lookback , (length(fada)-1)))
lab = rep(NA, dim(dat)[1])

for (z in 1:dim(dat)[1]) {
  lab[z] = fada[(40 + z),2]
  for (x in 1:lookback) {
    dat[z,x,] = as.numeric(fada[(((41 + z)-delay)-x),-1])
  }
}

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
err







