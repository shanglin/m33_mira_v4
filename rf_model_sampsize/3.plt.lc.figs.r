outdir = '~/Work/m33_phaseII/rf_model_sampsize/'
epsdir = paste0(outdir, 'lc_figs/')
lcdir = '~/Work/m33_phaseII/m33_v4_lcs/'
f.cad = paste0(outdir,'m33_mira_candidate.dat')

dat = read.table(f.cad, header=T)
dat[,1] = as.character(dat[,1])
idx = rev(order(dat[,'Scaled_prob']))
dat = dat[idx,]
n.dat = nrow(dat)

## idx = dat[,'Scaled_prob'] > 0.99
## plot(1/dat[idx,3], dat[idx,'lc.mag'], ylim=c(22,18), pch=19, cex=0.1, xlab='Log P', ylab='I (mag)')
## stop()

n.dat = nrow(dat)
seqs = 1:n.dat

library(doParallel)
cl = makeCluster(7)
registerDoParallel(cl)

source('fun.mkfitfig.r')
foreach(i = seqs) %dopar% {
    id = dat[i,1]
    freq = dat[i,'F.peak']
    t1 = dat[i,'theta.1']
    t2 = 10^(dat[i,'log10.theta.2'])
    cyc = mkfitfig(id, freq, t1, t2)
    dat[i,'F.true'] = round(cyc,3)
}
stopCluster(cl)
print('')

