set.seed(101)
outdir = '~/Work/m33_phaseII/rf_model_sampsize/'
figdir = paste0(outdir, 'figs/')
fig.dir = figdir
newdir = '~/Work/meetings/may12_2016/figs/'
f.m33 = paste0(outdir, 'm33_predictions.dat')

cpimg = function(f.eps) {
    f.png = gsub(figdir, newdir, f.eps)
    f.png = gsub('.eps','.png',f.png)
    f.png = gsub('.pdf','.png',f.png)
    cmd = paste0('convert ',f.eps,' ', f.png)
    system(cmd)
}


col = 'skyblue'
breaks = 100
width = 12
height = 12
cex = 1.3
mar = c(5,5,3,3)
## dat = read.table(f.m33, header=T)
prob = dat[,'Y_prob']
f.eps = paste0(fig.dir, 'hist_prob.eps')
setEPS()
postscript(f.eps, width=width, height=height/2)
par(mfrow=c(1,2))
hist(prob, breaks=breaks, col=col, main='')
t1 = paste0('# = ',sum(prob>-1))
text(0.4, 130000, t1, adj=0, cex=2)
hist(prob[prob>0.5], breaks=breaks/2, col=col, main='')
t1 = paste0('# = ',sum(prob>0.5))
text(0.7, 350, t1, adj=0, cex=2)
dev.off()
cpimg(f.eps)


if (F) {
    f.crs = paste0(outdir, 'cross_validation.dat') 
    ## crs = read.table(f.crs, header=T)
    crs[,1] = as.character(crs[,1])
    pcuts = seq(0, 1, 0.04)
    rtos2 = rtos = mids = rep(NA, length(pcuts)-1)
    fct = 23.5
    for (i in 1:(length(pcuts)-1)) {
        mids[i] = 0.5 * (pcuts[i] + pcuts[i+1])
        idx = crs[,'Y_prob'] > pcuts[i] & crs[,'Y_prob'] < pcuts[i+1]
        sub = crs[idx,]
        rtos[i] = sum(substr(sub[,1],1,4) == 'mira') / nrow(sub)
        rtos2[i] = (sum(substr(sub[,1],1,4) == 'mira') / fct) / (sum(substr(sub[,1],1,4) == 'mira') / fct + sum(substr(sub[,1],1,4) != 'mira'))
    }
    f.eps = paste0(fig.dir, 'rf_check.eps')
    setEPS()
    postscript(f.eps, width=width, height=height/2)
    par(mfrow=c(1,2), mar=mar)
    plot(mids, rtos, pch=19, xlab = 'P(Mira)', ylab='Percentage', cex.lab=cex, cex.axis=cex)
    lines(mids, rtos)
    lines(0:1,0:1, col=2)
    plot(mids, rtos2, pch=19, xlab = 'P(Mira)', ylab='Scaled Percentage', cex.lab=cex, cex.axis=cex)
    lines(mids, rtos2)
    lines(0:1,0:1, col=2)
    dev.off()
    cpimg(f.eps)
}

##### plot A.model hist
f.bld = '~/Work/m33_phaseII/m33_ofiles/blended_star.lst'
bld = read.table(f.bld)
bld = as.character(bld[,1])
bld = paste0(bld,'.slc')

f.eps = paste0(fig.dir, 'amp_hist_cut.eps')
setEPS()
postscript(f.eps, width=width, height=height)
par(mfrow=c(2,2))
a.cut = c(-2, 0.6) #mag
xlim = c(-1,6.3)
hist(dat[,'A.model'], breaks=breaks,main='Entire M33 sample',xlab='Model amplitude (mag)', col=col, cex.lab=1.3, cex.axis=1.3, xlim=xlim)
abline(v=a.cut, col=2)
t1 = paste0('Total # = ',nrow(dat))
text(4, 30000, t1, cex=cex)

sub = dat[dat[,'Y_prob'] > 0.5,]
hist(sub[,'A.model'], breaks=breaks,main='P (Mira) > 0.5',xlab='Model amplitude (mag)', col=col, cex.lab=1.3, cex.axis=1.3, xlim=xlim)
abline(v=a.cut, col=2)
t2 = paste0('Total # = ',nrow(sub))
text(4, 450, t2, cex=cex)


idx = !(sub[,1] %in% bld)
sub2 = sub[idx,]
hist(sub2[,'A.model'], breaks=breaks,main='P (Mira) > 0.5 & Remove blends',xlab='Model amplitude (mag)', col=col, cex.lab=1.3, cex.axis=1.3, xlim=xlim)
abline(v=a.cut, col=2)
t3 = paste0('Total # = ',nrow(sub2))
text(4, 350, t3, cex=cex)


idx = sub2[,'A.model'] >= a.cut[2] | sub2[,'A.model'] < a.cut[1]
cad = sub2[idx,]
hist(cad[,'A.model'], breaks=breaks/2,main='P (Mira) > 0.5 & Remove blends & A > 0.6 mag',xlab='Model amplitude (mag)', col=col, cex.lab=1.3, cex.axis=1.3, xlim=xlim)
t4 = paste0('Total # = ',nrow(cad))
text(4, 350, t4, cex=cex)
dev.off()
cpimg(f.eps)


############ check q2m par
## f.ftr = '~/Work/m33_phaseII/rf_model_nb/nb_features.dat'
## ftr = read.table(f.ftr, header=T)
## ftr[,1] = as.character(ftr[,1])
## idx = substr(ftr[,1],1,4) == 'mira'
## mir = ftr[idx,]

f.eps = paste0(fig.dir, 'ratio_q2m_hist.eps')
setEPS()
postscript(f.eps, width=width, height=height/2)
par(mfrow=c(1,2))
xlab = 'Relative quadratic residual uncertainty'
xlim = c(0,1.5)
h = hist(mir[,'ratio.q2m'], breaks=50, col=col, main='SIM: Miras', xlab=xlab, xlim=xlim)
xy = cbind(h$mids,h$counts)
lines(xy, col=2)

hist(cad[,'ratio.q2m'], breaks=50, col=col, main='M33: P (Mira) > 0.5 & Remove blends & low Amplitude',xlab=xlab,xlim=xlim)
t1 = 'No cut for this parameter'
text(0.9, 200, t1, cex=1.3, adj=0)
xy[,2] = xy[,2]/max(xy[,2])*400
lines(xy, col=2)
dev.off()
cpimg(f.eps)


############ Aliasing periods
f.eps = paste0(fig.dir, 'periods_hist.eps')
setEPS()
postscript(f.eps, width=width, height=height)
par(mfrow=c(2,2))
xlim = c(0,2000)
breaks=50
hist(1/mir[,2], xlab='Period [day]', breaks=breaks/2, cex.lab=1.3, cex.axis=1.3, main='SIM Mira: True Period', xlim=xlim, col=col)
hist(1/mir[,3], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='SIM Mira: Peak Period', xlim=xlim, col=col)
hist(1/cad[,3], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate: Peak Period', xlim=xlim, col=col)
ppeak = 320
abline(v=ppeak, col=2)
t1 = paste0('P = ',ppeak,'d')
text(400, 1200, t1, col=2, adj=0,cex=1.5)
t3 = paste0('Total # = ',nrow(cad))
text(1000, 1000, t3, col=1, adj=0,cex=1.5)

idx = cad[,'Y_prob'] == 1
hist(1/cad[idx,3], xlab='Period [day]', breaks=breaks/5, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate w/ P(Mira) = 1: Peak Period', xlim=xlim, col=col)
t2 = paste0('Total # = ',sum(idx))
text(1000, 40, t2, col=1, adj=0,cex=1.5)
dev.off()
cpimg(f.eps)


f.pdf = paste0(fig.dir, 'periods_prob_scatter.pdf')
pdf(f.pdf, width=width, height=height/2)
par(mfrow=c(1,2))
idx = substr(crs[,1],1,4) == 'mira' & crs[,'Y_prob']>0.5 & crs[,3] > 0
cmir = crs[idx,]
idx = sample(1:nrow(cmir), nrow(cad))
plot(cad[,'Y_prob'], 1/cad[,3], pch=19, cex=0.3, col=rgb(0,0,0,0.2), xlab='P(Mira)', ylab='Period [day]', main = 'M33 cleaned sample')
plot(cmir[idx,'Y_prob'], 1/cmir[idx,3], pch=19, cex=0.3, col=rgb(0,0,0,0.2), xlab='P(Mira)', ylab='Period [day]', main='Same amount of Simulated Miras')
dev.off()
cpimg(f.pdf)


#################### Scale the Mira probabilities as function of period.
reg.1 = c(300, 360)
reg.2 = c(950, 2000)

f.eps = paste0(figdir,'scale_prob_factor.eps')
setEPS()
postscript(f.eps, width=12, height=6)

par(mfrow=c(1,2))
xlim = c(0,2000)
breaks=50
h1 = hist(1/mir[,2], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='SIM Mira: True Period', xlim=xlim, col='aliceblue')
xy = cbind(h1$mids, h1$counts)
mids = c(h1$mids, h1$mids + max(h1$mids) - 2*h1$mids[1] + h1$mids[2])
mids = mids[mids < 2000]
counts = rep(1, length(mids))
idx = match(h1$mids, mids)
counts[idx] = h1$counts
xy = cbind(mids, counts)
fit = smooth.spline(xy)
model = predict(fit)
lines(model, col=2, lwd=3)

h2 = hist(1/cad[,3], xlab='Period [day]', breaks=breaks*2, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate: Peak Period', xlim=xlim, col='aliceblue', ylab='Frequency & Scale factor*500')
model.y = model$y/max(model$y)*200
idx = model.y < 1
model.y[idx] = 1

lines(model$x, model.y, col=2, lwd=3)
m33.counts = m33.mids = rep(0.1, length(model.y))
m33.mids = model$x
m33.counts[1:length(h2$mids)] = h2$counts
idx = m33.counts < 10
m33.counts[idx] = 10

pfactor = model.y/m33.counts
xy = cbind(mids, pfactor)
xx = seq(min(mids), max(mids), 1)
newxy = predict(smooth.spline(xy), xx)

gfactor.x = 1:2000
gfactor.y = rep(1, length(gfactor.x))
sfc = cbind(gfactor.x, gfactor.y)
idx.1 = which(xx > reg.1[1] & xx < reg.1[2])
idx.2 = which(xx > reg.2[1])
sfc[match(newxy$x[idx.1], sfc[,1]), 2] = newxy$y[idx.1]
sfc[match(newxy$x[idx.2], sfc[,1]), 2] = newxy$y[idx.2]
sfc[sfc[,1]>max(newxy$x),2] = 0.1
lines(sfc[,1],sfc[,2]*500, lwd=5, col='white')
lines(sfc[,1],sfc[,2]*500, lwd=3, col=4)
legend(1000, 400, c('g = g(period)','Expected distribution'), lwd=c(3,3), col=c(4,2))
dev.off()
cpimg(f.eps)

f.sfc = paste0(outdir, 'scale_factor.dat')
ts = '#   period      factor'
write(ts, f.sfc)
sfc = round(sfc,3)
write.table(sfc, f.sfc, col.names=F, row.names=F, append=T, sep='   ')

######### update the scaled probability
f.eps = paste0(figdir,'scale_prob_hist.eps')
setEPS()
postscript(f.eps, width=12, height=12)
alpha = 2
periods = round(1/cad[,3])
idx = match(periods, sfc[,1])
prob = cad[,'Y_prob']
scaled.prob = prob * sfc[idx,2]^(alpha*(1-prob))
cad[,22] = round(scaled.prob,3)
par(mfrow=c(2,2))
hist(prob,breaks=breaks/2,xlim=c(0,1),col=col,main='M33 cleaned: Original probability',cex.axis=1.3,cex.lab=1.3)
hist(cad[,22],breaks=breaks,col=col,main='M33 cleaned: Scaled probability',cex.axis=1.3,cex.lab=1.3,xlab='Scaled probability')
sp.cut = c(0.4, 0.5)
abline(v = sp.cut, col=2)
t1 = paste0('# (Scaled Prob > ',sp.cut[1],') = ',sum(cad[,22]>sp.cut[1]))
text(0.01, 500, t1, cex=1.7, adj=0)
t2 = paste0('# (Scaled Prob > ',sp.cut[2],') = ',sum(cad[,22]>sp.cut[2]))
text(0.01, 400, t2, cex=1.7, adj=0)
idx = cad[,22] > sp.cut[1]
xlab = paste0('M33 cleaned: Periods for scaled prob > ',sp.cut[1])
hist(1/cad[idx,3],breaks=breaks/2,col=col,main=xlab,cex.axis=1.3,xlab='Period [day]',cex.lab=1.3, xlim=c(0,1900))
t3 = paste0('# = ',sum(idx))
text(1000, 500, t3, cex=1.7, adj=0)
plot(cad[,'N_prob'], 1/cad[,3], pch=19, cex=0.1, xlab='P(Mira)', ylab='Period [day]', main = 'M33 cleaned sample')
abline(v=sp.cut, lty=2, col=2)
dev.off()
cpimg(f.eps)

f.cad = paste0(outdir,'m33_mira_candidate.dat')
ts = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m  Scaled_prob Y_prob'
write(ts, f.cad)
fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f%10.3f%10.3f'
out2 = do.call('sprintf', c(fmt, cad))
write(out2, f.cad, append=T)


#########  model amplitude and lc amplitude
f.eps = paste0(figdir,'amp_ratio_cmp.eps')
setEPS()
postscript(f.eps, width=12, height=12/2)
par(mfrow=c(1,2))
ylim = c(0, 10)
a2a = cad[,'A.model'] / cad[,'A.lc.9']
plot(cad[,22], a2a, xlab='Scaled Probability', ylab='A (model) / A (0.9 percentile)', ylim=ylim, cex=0.3, main='M33 cleaned sample')
abline(h = c(median(a2a), median(a2a)+sd(a2a)), col=2, lty=c(1,2))

idx = substr(crs[,1],1,4) == 'mira' & crs[,'A.model'] > 0.6
cmir = crs[idx,]
idx = sample(1:nrow(cmir), nrow(cad))
a2a.cmir = cmir[idx,'A.model'] / cmir[idx,'A.lc.9']
plot(cmir[idx,23], a2a.cmir, ylim = ylim, xlab='Probability', ylab='A (model) / A (0.9 percentile)', cex=0.3, main='Simulated Miras with A (model) > 0.6')
dev.off()
cpimg(f.eps)

