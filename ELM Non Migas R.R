library(forecast) 
library(tstools)
library(tseries)
library(nnfor)  

set.seed(099)
#input data
nonmigas = read.csv("C:/data/Skripsi/data/nonmigas/nonmigass.csv", header = TRUE, sep = ";")
View(nonmigas)
nonmigas.ts = ts(nonmigas$Non.Migas, start = c(2008,1), frequency = 12)
nonmigas.ts
ts.plot(nonmigas.ts, main = "TS: EKSPOR NON MIGAS")
summary(nonmigas)

## DATA TRAIN DAN TEST DENGAN RASIO 95%:5%
ntrain4 = round(0.95*NROW(nonmigas.ts),0)
ntrain4
train4 = window(nonmigas.ts, start = c(2008, 1), end = c(2022,3))
train4
ntest4 = round(0.05*NROW(nonmigas.ts),0)
ntest4
test4 = window(nonmigas.ts, start = c(2022,4))
test4

## model training 95% dengan 10 hd dan reps 5 
model1 = elm(train4, 
             m = frequency(nonmigas.ts),
             hd = 10, 
             type = c("lasso", "ridge", "step", "lm"),
             reps = 5, comb = c("median","mean","mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = FALSE, 
             model = NULL, 
             retrain = TRUE)
plot(model1)

# Prediksi 9 bulan
pred1 = forecast(model1, 9)
pred1

# Ukuran kesalahan untuk data testing
accuracy(pred1, test4)

# Plot data aktual, prediksi
plot(pred1)

## model training 95% dengan 10 hd dan reps 10 
model2 = elm(train4, 
             m = frequency(nonmigas.ts),
             hd = 10, 
             type = c("lasso", "ridge", "step", "lm"),
             reps = 10, comb = c("median","mean","mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = FALSE, 
             model = NULL, 
             retrain = TRUE)
plot(model2)

# Prediksi 9 bulan
pred2 = forecast(model2, 9)
pred2

# Ukuran kesalahan untuk data testing
accuracy(pred2, test4)

# Plot data aktual, prediksi
plot(pred2)

## model training 95% dengan 10 hd dan reps 15 
model3 = elm(train4, 
             m = frequency(nonmigas.ts),
             hd = 10, 
             type = c("lasso", "ridge", "step", "lm"),
             reps = 15, comb = c("median","mean","mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = FALSE, 
             model = NULL, 
             retrain = TRUE)
plot(model3)

# Prediksi 9 bulan
pred3 = forecast(model3, 9)
pred3

# Ukuran kesalahan untuk data testing
accuracy(pred3, test4)

# Plot data aktual, prediksi 
plot(pred3)

## model training 95% dengan 10 hd dan reps 20 
model4 = elm(train4, 
             m = frequency(nonmigas.ts),
             hd = 10, 
             type = c("lasso", "ridge", "step", "lm"),
             reps = 20, comb = c("median","mean","mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = FALSE, 
             model = NULL, 
             retrain = TRUE)
plot(model4)

# Prediksi 9 bulan
pred4 = forecast(model4, 9)
pred4

# Ukuran kesalahan untuk data testing
accuracy(pred4, test4)

# Plot data aktual, fitted, prediksi
plot(pred4)
