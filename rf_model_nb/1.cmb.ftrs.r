## features from 3 files
## ordinary features, lc mean mag, and quadratic fitting.

outdir = '~/Work/m33_phaseII/rf_model_nb/'
ftrdir = '~/Work/m33_phaseII/sc_ext_ftrs/'

f.1 = paste0(ftrdir, 'features_allfile.dat')
f.2 = paste0(ftrdir, 'lc_mean_mag.dat')
f.3 = paste0(ftrdir, 'qdr_fit_features.dat')

ftr.1 = read.fwf(f.1, width=c(35,11,10,10,9,9,8,15,16,8,8,8,5,8,8,8,9))
ftr.2 = read.table(f.2)
ftr.3 = read.fwf(f.3, width=c(30,12,12,12))

ftr.1[,1] = as.character(ftr.1[,1])
ftr.1[,1] = gsub(' ','',ftr.1[,1])
ftr.2[,1] = as.character(ftr.2[,1])
ftr.3[,1] = as.character(ftr.3[,1])
ftr.3[,1] = gsub(' ','',ftr.3[,1])
ftr.3[,1] = paste0(ftr.3[,1], '.flc')
ftr.3[,1] = gsub('.slc.flc','.slc', ftr.3[,1])

ftr = ftr.1
idx = match(ftr[,1], ftr.2[,1])
ftr.2 = ftr.2[idx,]
ftr = cbind(ftr, ftr.2[,2])
idx = match(ftr[,1], ftr.3[,1])
ftr.3 = ftr.3[idx,]
ftr = cbind(ftr, ftr.3[,2:4])

## fix NAs


f.ftr = paste0(outdir, 'nb_features.dat')
ts = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m'
write(ts, f.ftr)
fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f'
out = do.call('sprintf', c(fmt, ftr))
write(out, f.ftr, append=T)


df = 0.00027
idx = substr(ftr[,1],1,4) == 'mira'
dat = ftr[idx,]
idx = abs(dat[,3]-dat[,2]) < df
a = sum(idx) / nrow(dat)
print(a)
