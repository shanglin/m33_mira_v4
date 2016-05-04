outdir = '~/Work/m33_phaseII/mira_cata_nb/'
figdir = paste0(outdir, 'figs/')
epsdir = paste0(outdir, 'lc_figs/')
lcdir = '~/Work/m33_phaseII/m33_v4_lcs/'

f.bld = '~/Work/m33_phaseII/m33_ofiles/blended_star.lst'
bld = read.table(f.bld)
bld = as.character(bld[,1])
bld = paste0(bld,'.slc')

f.dat = '~/Work/m33_phaseII/rf_model_nb/m33_predictions.dat'
## dat = read.table(f.dat, header=T)
## dat[,1] = as.character(dat[,1])
## idx = rev(order(dat[,'Y_prob']))
## dat = dat[idx,]

breaks = 50
cex = 2
col = 'skyblue'

f.eps = paste0(figdir,'amp_hist.eps')
setEPS()
postscript(f.eps, width=12, height=12)

par(mfrow=c(2,2))
a.cut = c(0, 0.8) #mag
hist(dat[,'A.model'], breaks=breaks,main='Entire M33 sample',xlab='Model amplitude (mag)', col=col, cex.lab=1.3, cex.axis=1.3)
abline(v=a.cut, col=2)
t1 = paste0('Total # = ',nrow(dat))
text(4, 30000, t1, cex=cex)

sub = dat[dat[,'Y_prob'] > 0.5,]
hist(sub[,'A.model'], breaks=breaks,main='P (Mira) > 0.5',xlab='Model amplitude (mag)', col=col, cex.lab=1.3, cex.axis=1.3)
abline(v=a.cut, col=2)
t2 = paste0('Total # = ',nrow(sub))
text(4, 2000, t2, cex=cex)


idx = !(sub[,1] %in% bld)
sub2 = sub[idx,]
hist(sub2[,'A.model'], breaks=breaks,main='P (Mira) > 0.5 & Remove blends',xlab='Model amplitude (mag)', col=col, cex.lab=1.3, cex.axis=1.3)
abline(v=a.cut, col=2)
t3 = paste0('Total # = ',nrow(sub2))
text(4, 1500, t3, cex=cex)


idx = sub2[,'A.model'] >= a.cut[2] | sub2[,'A.model'] < a.cut[1]
cad = sub2[idx,]
hist(cad[,'A.model'], breaks=breaks,main='P (Mira) > 0.5 & Remove blends & Remove low Amplitude',xlab='Model amplitude (mag)', col=col, cex.lab=1.3, cex.axis=1.3)
t4 = paste0('Total # = ',nrow(cad))
text(4, 1000, t4, cex=cex)
dev.off()

xlab = expression(paste(P['RF'], ' (Mira)'))
## hist(cad[,'Y_prob'], breaks=100, col=col, main='Candidates', xlab=xlab)

f.ftr = '~/Work/m33_phaseII/rf_model_nb/nb_features.dat'
## ftr = read.table(f.ftr, header=T)
## ftr[,1] = as.character(ftr[,1])
## idx = substr(ftr[,1],1,4) == 'mira'
## mir = ftr[idx,]

f.eps = paste0(figdir,'ratio_q2m_hist.eps')
setEPS()
postscript(f.eps, width=12, height=6)

par(mfrow=c(1,2))
r.cut = 1
xlab = 'Relative quadratic residual uncertainty'
xlim = c(0,1.5)
h = hist(mir[,'ratio.q2m'], breaks=50, col=col, main='SIM: Miras', xlab=xlab, xlim=xlim)
abline(v=r.cut, col=2)
xy = cbind(h$mids,h$counts)
lines(xy, col=2)

hist(cad[,'ratio.q2m'], breaks=50, col=col, main='M33: P (Mira) > 0.5 & Remove blends & low Amplitude',xlab=xlab,xlim=xlim)
abline(v=r.cut, col=2,lwd=3)
arrows(r.cut,400,r.cut-0.3,400, col=2, lwd=3)
idx = cad[,'ratio.q2m'] < r.cut
cad = cad[idx,]
t1 = paste0('# keep = ',nrow(cad))
text(1., 400, t1, cex=1.3, adj=0)
xy[,2] = xy[,2]/max(xy[,2])*500
lines(xy, col=2)
dev.off()


f.eps = paste0(figdir,'periods_hist.eps')
setEPS()
postscript(f.eps, width=12, height=12)
par(mfrow=c(2,2))
xlim = c(0,2000)
breaks=50
hist(1/mir[,2], xlab='Period [day]', breaks=breaks/2, cex.lab=1.3, cex.axis=1.3, main='SIM Mira: True Period', xlim=xlim, col=col)
hist(1/mir[,3], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='SIM Mira: Peak Period', xlim=xlim, col=col)
hist(1/cad[,3], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate: Peak Period', xlim=xlim, col=col)
abline(v=365, col=2)
t1 = 'P = 365d'
text(400, 2500, t1, col=2, adj=0,cex=1.5)
t3 = paste0('Total # = ',nrow(cad))
text(1000, 1600, t3, col=1, adj=0,cex=1.5)

idx = cad[,'Y_prob'] == 1
hist(1/cad[idx,3], xlab='Period [day]', breaks=breaks/2, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate w/ P(Mira) = 1: Peak Period', xlim=xlim, col=col)
t2 = paste0('Total # = ',sum(idx))
text(1000, 150, t2, col=1, adj=0,cex=1.5)
dev.off()


f.eps = paste0(figdir,'scale_prob_factor.eps')
setEPS()
postscript(f.eps, width=12, height=6)
par(mfrow=c(1,2))
xlim = c(0,2000)
breaks=50
h1 = hist(1/mir[,2], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='SIM Mira: True Period', xlim=xlim, col=col)
xy = cbind(h1$mids, h1$counts)
fit = smooth.spline(xy, df=20)
model = predict(fit)
lines(model, col=2, lwd=3)

h2 = hist(1/cad[,3], xlab='Period [day]', breaks=breaks*2, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate: Peak Period', xlim=xlim, col=col, ylab='Frequency & Scale factor*1000')
model.y = model$y/max(model$y)*200
idx = model.y < 5
model.y[idx] = 5

lines(model$x, model.y, col=2, lwd=3)
idx = match(h1$mids, h2$mids)
mids = h2$mids[idx]
counts = h2$counts[idx]

pfactor = model.y/counts
xy = cbind(mids, pfactor)
xx = seq(-10,2000,1)
newxy = predict(smooth.spline(xy, df=20), xx)
idx = newxy$y < 0
newxy$y[idx] = 0
newxy$y = newxy$y/max(newxy$y)
lines(xx, newxy$y*1000, lwd=3, col=1)
dev.off()

## test to find a good function
## par(mfrow=c(1,1))
## f = newxy$y
## p = exp(-1e-5*(xx-500)^2) + 0.4*exp(-2e-4*(xx-800)^2)
## p = p/max(p)
## p = p*0.5 + 0.5
## plot(xx,f, type='l')
## lines(xx, p, col=2)
## np = p * f^(2*(1-p))
## lines(xx, np, col=3)

f.eps = paste0(figdir,'scale_prob_hist.eps')
setEPS()
postscript(f.eps, width=12, height=12)
par(mfrow=c(2,2))
alpha = 2
periods = round(1/cad[,3])
idx = match(periods, xx)
s.factor = newxy$y[idx]
prob = cad[,'Y_prob']
scaled.prob = prob * s.factor^(alpha*(1-prob))
cad[,22] = scaled.prob

breaks=50
hist(prob,breaks=breaks/2,xlim=c(0,1),col=col,main='M33 cleaned: Original probability',cex.axis=1.3,cex.lab=1.3)
hist(scaled.prob,breaks=breaks,col=col,main='M33 cleaned: Scaled probability',cex.axis=1.3,cex.lab=1.3)
sp.cut = c(0.5, 0.9)
abline(v = sp.cut, col=2)
t1 = paste0('# (Scaled Prob > ',sp.cut[1],') = ',sum(scaled.prob>sp.cut[1]))
text(0.01, 1000, t1, cex=1.7, adj=0)
t2 = paste0('# (Scaled Prob > ',sp.cut[2],') = ',sum(scaled.prob>sp.cut[2]))
text(0.01, 800, t2, cex=1.7, adj=0)
idx = cad[,22] > sp.cut[1]
xlab = paste0('M33 cleaned: Periods for scaled prob > ',sp.cut[1])
hist(1/cad[idx,3],breaks=breaks/2,col=col,main=xlab,cex.axis=1.3,xlab='Period [day]',cex.lab=1.3)
t3 = paste0('# = ',sum(idx))
text(1000, 1000, t3, cex=1.7, adj=0)
idx = cad[,22] > sp.cut[2]
xlab = paste0('M33 cleaned: Periods for scaled prob > ',sp.cut[2])
hist(1/cad[idx,3],breaks=breaks/2,col=col,main=xlab,cex.axis=1.3,xlab='Period [day]',cex.lab=1.3)
t4 = paste0('# = ',sum(idx))
text(1000, 380, t4, cex=1.7, adj=0)
dev.off()

f.cad = paste0(outdir,'m33_mira_candidate.dat')
ts = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m  Scaled_prob Y_prob'
write(ts, f.cad)
fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f%10.3f%10.3f'
out2 = do.call('sprintf', c(fmt, cad))
write(out2, f.cad, append=T)

if (F) {
n.cad = nrow(cad)
seqs = seq(1, n.cad, 20)
source('fun.mkfitfig.r')
for (i in seqs) {
    id = cad[i,1]
    freq = cad[i,'F.peak']
    t1 = cad[i,'theta.1']
    t2 = 10^(cad[i,'log10.theta.2'])
    cyc = mkfitfig(id, freq, t1, t2)
    cad[i,'F.true'] = round(cyc,3)
    msg = paste0('   >> ',round(i*100/n.cad,2),' %      \r')
    message(msg, appendLF=F)
    ## Sys.sleep(2)
    ## stop()
}
print('')
}

