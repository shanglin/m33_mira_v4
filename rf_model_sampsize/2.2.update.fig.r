set.seed(101)
outdir = '~/Work/m33_phaseII/rf_model_sampsize/'
figdir = paste0(outdir, 'figs_update/')
fig.dir = figdir
newdir = '~/Work/meetings/may16_2016/figs/'
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

f.bld = '~/Work/m33_phaseII/m33_ofiles/blended_star.lst'
bld = read.table(f.bld)
bld = as.character(bld[,1])
bld = paste0(bld,'.slc')

sub = dat[dat[,'Y_prob'] > 0.5,]
idx = !(sub[,1] %in% bld)
sub2 = sub[idx,]
a.cut = c(-2, 0.6) #mag
idx = sub2[,'A.model'] >= a.cut[2] | sub2[,'A.model'] < a.cut[1]
cad = sub2[idx,]


###################### aliasing periods
f.crs = '~/Work/m33_phaseII/rf_model_sampsize/cross_validation.dat'
## crs = read.table(f.crs, header=T)
idx = crs[,'A.model'] >= a.cut[2] & crs[,'Y_prob']  > 0.5
mir = crs[idx,]
f.eps = paste0(fig.dir, 'periods_hist.eps')
setEPS()
postscript(f.eps, width=width, height=height)
par(mfrow=c(2,2))
xlim = c(0,2000)
ppeak = c(320, 365)
breaks=150
hist(1/mir[,2], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='SIM Data w/ P>0.5 & A>0.6: True Period', xlim=xlim, col=col)
abline(v=ppeak, col=c(2,4), lwd=2)
hist(1/mir[,3], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='SIM Data w/ P>0.5 & A>0.6: Peak Period', xlim=xlim, col=col)
abline(v=ppeak, col=c(2,4), lwd=2)
hist(1/cad[,3], xlab='Period [day]', breaks=breaks, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate: Peak Period', xlim=xlim, col=col)
abline(v=ppeak, col=c(2,4), lwd=2)
t1 = paste0('P = ',ppeak[1],'d')
text(400, 250, t1, col=2, adj=0,cex=1.5)
t1 = paste0('P = ',ppeak[2],'d')
text(400, 220, t1, col=4, adj=0,cex=1.5)
t3 = paste0('Total # = ',nrow(cad))
text(1000, 200, t3, col=1, adj=0,cex=1.5)

idx = cad[,'Y_prob'] == 1
hist(1/cad[idx,3], xlab='Period [day]', breaks=breaks/5, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate w/ P(Mira) = 1: Peak Period', xlim=xlim, col=col)
t2 = paste0('Total # = ',sum(idx))
text(1000, 19, t2, col=1, adj=0,cex=1.5)
dev.off()
cpimg(f.eps)


f.pdf = paste0(fig.dir, 'periods_prob_scatter.pdf')
pdf(f.pdf, width=width, height=height/2)
par(mfrow=c(1,2))
idx = sample(1:nrow(mir), nrow(cad))
plot(cad[,'Y_prob'], 1/cad[,3], pch=19, cex=0.3, col=rgb(0,0,0,0.2), xlab='P(Mira)', ylab='Period [day]', main = 'M33 cleaned sample')
plot(mir[idx,'Y_prob'], 1/mir[idx,3], pch=19, cex=0.3, col=rgb(0,0,0,0.2), xlab='P(Mira)', ylab='Period [day]', main='SIM: P>0.5 & A>0.6')
dev.off()
cpimg(f.pdf)


f.pdf = paste0(fig.dir, 'periods_prob_scatter_nonmira.pdf')
pdf(f.pdf, width=width, height=height)
nondat = dat[dat['Y_prob']<0.5,]
nonmir = crs[crs['Y_prob']<0.5,]
par(mfrow=c(2,2))
idx = sample(1:nrow(nondat), nrow(cad)*3)
plot(nondat[idx,'Y_prob'], 1/nondat[idx,3], pch=19, cex=0.3, col=rgb(0,0,0,0.2), xlab='P(Mira)', ylab='Period [day]', main = 'M33: P<0.5')
abline(h=ppeak, col=c(2,4), lwd=0.5)
idx = sample(1:nrow(nonmir), nrow(cad))
plot(nonmir[idx,'Y_prob'], 1/nonmir[idx,3], pch=19, cex=0.3, col=rgb(0,0,0,0.2), xlab='P(Mira)', ylab='Period [day]', main='SIM: P<0.5')
abline(h=ppeak, col=c(2,4), lwd=0.5)
h3 = hist(1/nondat[,3], breaks=breaks*2, plot=F)
plot(h3$mids, h3$counts, type='s', xlab='Period [day]', cex.lab=1.3, cex.axis=1.3, main='M33: P<0.5', xlim=xlim, col=1, ylab='Frequency')
abline(v=ppeak, col=c(2,4), lwd=1.5)
t1 = paste0('P = ',ppeak[1],'d')
text(500, 25000/4, t1, col=2, adj=0,cex=1.5)
t1 = paste0('P = ',ppeak[2],'d')
text(500, 22000/4, t1, col=4, adj=0,cex=1.5)

h4 = hist(1/nonmir[,3], breaks=breaks*2, plot=F)
plot(h4$mids, h4$counts, type='s', xlab='Period [day]', cex.lab=1.3, cex.axis=1.3, main='SIM: P<0.5', xlim=xlim, col=1, ylab='Frequency')
abline(v=ppeak, col=c(2,4), lwd=1.5)
dev.off()
cpimg(f.pdf)
