outdir = '~/Work/m33_phaseII/mira_cata/'
figdir = paste0(outdir, 'figs/')
epsdir = paste0(outdir, 'lc_figs/')
lcdir = '~/Work/m33_phaseII/m33_v4_lcs/'

f.dat = '~/Work/m33_phaseII/rf_model/m33_predictions.dat'
## dat = read.table(f.dat, header=T)
## dat[,1] = as.character(dat[,1])
## idx = rev(order(dat[,'Y_prob']))
## dat = dat[idx,]

a.cut = 0.6 #mag
t.cut = 9.5

## f.png = paste0(figdir,'t.test.mol.amp.png')
## png(f.png)
## plot(dat[,2], dat[,'A.p1'], xlab='T test value',ylab='Model Amplitude', pch=19, cex=0.1, col=rgb(0,0,0,0.1))
## abline(h = a.cut, col=4)
## abline(v = t.cut, col=4)
## abline(v = -t.cut, col=4)
## dev.off()

idx = abs(dat[,2]) < t.cut & dat[,'A.p1'] > a.cut
sub = dat[idx,]
a = sum(dat[,'Y_prob'] > 0.5)
b = sum(sub[,'Y_prob'] > 0.5)
## hist(sub[,'Y_prob'], breaks=100)

cad = sub[sub[,'Y_prob'] > 0.5,]
n.cad = nrow(cad)

source('fun.mkfitfig.r')
for (i in 1:n.cad) {
    id = cad[i,1]
    freq = cad[i,'f.p1']
    t1 = cad[i,'theta.1']
    t2 = 10^(cad[i,'log10.theta.2'])
    cyc = mkfitfig(id, freq, t1, t2)
    cad[i,3] = round(cyc,3)
    msg = paste0('   >> ',round(i*100/n.cad,2),' %      \r')
    message(msg, appendLF=F)
    ## Sys.sleep(2)
    ## stop()
}
print('')


