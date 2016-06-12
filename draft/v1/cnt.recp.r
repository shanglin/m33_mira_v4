f.dat = '~/Work/m33_phaseII/rf_model_sampsize/nb_features.dat'
## dat = read.table(f.dat, header=T, stringsAsFactors=F)
## idx = substr(dat[,1],1,4) == 'mira'
## dat = dat[idx,]
## print(nrow(dat))
df = 0.00027
idx = abs(dat[,2]-dat[,3]) < df
rate = sum(idx) * 100 / nrow(dat)
print(rate)
