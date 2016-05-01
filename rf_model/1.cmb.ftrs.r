## features from 3 files
## ordinary features, bayes factor, and quadratic fitting.

outdir = '~/Work/m33_phaseII/rf_model/'
ftrdir = '~/Work/m33_phaseII/sc_ext_ftrs/'

f.1 = paste0(ftrdir, 'features_allfile.dat')
f.2 = paste0(ftrdir, 'bf_features.csv')
f.3 = paste0(ftrdir, 'qdr_fit_features.dat')

ftr.1 = read.fwf(f.1, width=c(35,11,10,10,9,9,8,15,16,8,8,8,5,8,8,8,9))
ftr.2 = read.csv(f.2)
ftr.3 = read.fwf(f.3, width=c(30,12,12,12))

ftr.1[,1] = as.character(ftr.1[,1])
ftr.1[,1] = gsub(' ','',ftr.1[,1])
ftr.2[,1] = as.character(ftr.2[,1])
ftr.3[,1] = as.character(ftr.3[,1])
ftr.3[,1] = gsub(' ','',ftr.3[,1])
ftr.3[,1] = paste0(ftr.3[,1], '.flc')
ftr.3[,1] = gsub('.slc.flc','.slc', ftr.3[,1])

ftr = ftr.1[,-c(3,4,5,10,15,16)]
colnames(ftr) = c('id','f.true','dQ.base','dQ.p2','theta.1','log.theta.2','A.lc','A.lc.9','n.obs','sd.err','i.mag')
idx = match(ftr[,1], ftr.2[,1])
ftr.2 = ftr.2[idx,]
ftr = cbind(ftr[,1], ftr.2[,'zz'], ftr[,-c(1)], ftr.2[,c(2,3,4,5,6,7,8,9,10,11)])
idx = match(ftr[,1], ftr.3[,1])
ftr.3 = ftr.3[idx,]
ftr = cbind(ftr, ftr.3[,2:4])

## fix NAs
idx = is.infinite(ftr[,2])
ftr[idx,2] = 9.999
idx = is.nan(ftr[,2])
ftr[idx,2] = -9.999

idx = is.infinite(ftr[,13])
ftr[idx,13] = 9.999
idx = is.nan(ftr[,13])
ftr[idx,13] = -9.999
idx = is.infinite(ftr[,14])
ftr[idx,14] = 9.999
idx = is.nan(ftr[,14])
ftr[idx,14] = -9.999
idx = is.infinite(ftr[,15])
ftr[idx,15] = 9.999
idx = is.nan(ftr[,15])
ftr[idx,15] = -9.999
idx = is.infinite(ftr[,16])
ftr[idx,16] = 9.999
idx = is.nan(ftr[,16])
ftr[idx,16] = -9.999


## idx = ftr.1[,3] != -1
## ftr[idx,17] = ftr.1[idx,3]
## ftr[idx,19] = ftr.1[idx,10]



f.ftr = paste0(outdir, 'mnm_features.dat')
ts = '                    ID                t.test    F.true   dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.lc   A.lc.9 n.obs sd.error  i.mag     bfM2S      bfS2C     bfGP2P     bfM2C     f.p1     l.p1    A.p1     f.p2     l.p2    A.p2  mean.sd qdr.rsd.sd ratio.q2m'
write(ts, f.ftr)
fmt = '%35s%9.3f%11.6f%11.3f%9.3f%15.5f%16.5f%8.3f%8.3f%5i%8.3f%9.3f%11.4f%10.4f%11.4f%10.4f%10.6f%8.4f%8.3f%10.6f%8.4f%8.3f%8.3f%9.3f%10.3f'
out = do.call('sprintf', c(fmt, ftr))
write(out, f.ftr, append=T)

