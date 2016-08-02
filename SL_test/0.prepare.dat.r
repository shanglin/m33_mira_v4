set.seed(101)
options(stringsAsFactors=F)
dir = '~/Work/m33_phaseII/SL_test/'
f.dat = '~/Work/m33_phaseII/rf_model_sampsize/nb_features.dat'
dat = read.table(f.dat, head = T)

type = substr(dat[,1],1,3)
idx = type == 'mir' | type == 'con' | type == 'srv'
sub = dat[idx,]

idx = sub[,3] > 0
sub = sub[idx,]

N = nrow(sub)
Ntrn = round(N / 2)
Ntst = N - Ntrn

idx = sample(1:N, Ntrn)
trn = sub[idx,]
tst = sub[-idx,]

f.trn = paste0(dir, 'sim_trn.dat')
f.tst = paste0(dir, 'sim_tst.dat')

header = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m'
write(header, f.trn)
write(header, f.tst)
fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f'
out = do.call('sprintf', c(trn, fmt))
write(out, f.trn, append=T)
out = do.call('sprintf', c(tst, fmt))
write(out, f.tst, append=T)
