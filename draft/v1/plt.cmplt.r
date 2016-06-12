dir = '~/Work/m33_phaseII/m33_ofiles/complete_fun/'
outdir = '~/Work/m33_phaseII/draft/v1/figures/'
f.eps = paste0(outdir,'lum_fun.eps')
mag.shift = 6.2

width=9
height = width/1.618
setEPS()
postscript(f.eps, width=width, height=height)
f.dat = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
dat = read.table(f.dat)
mag = dat[,4]
mag.lim.1 = 18.5
mag.lim.2 = 20
mar1 = c(0,4.5,1,3)
mar2 = c(4.5,4.5,0,3)
mar3 = c(4.5,4.5,0,1)
fig1 = c(0,0.55,0.56,1)
fig2 = c(0,0.55,0,0.56)
fig3 = c(0.45,1,0,1)
hcol1 = '#dddddd'
hcol2 = '#5555ee'

par(mar=mar1, tck=0.04, fig=fig1, mgp=c(1.9,0.4,0))
h = hist(mag, breaks=200, plot=F)
x = h$mids
y = h$counts
ly = log10(y)
xlim = c(18.5, 21.5)
xlab = expression(paste(italic(I),' [mag]'))
ylab = expression(paste('log (',italic(N),')'))
plot(x, ly, xlim=xlim, ylim=c(1.8,4.8),type='s', xlab='', ylab=ylab, cex.lab=1.3, cex.axis=1.1, xaxt='n', yaxt='n')
at = seq(18.5,21.5,0.5)
label = rep('',length(at))
axis(1, at = at, label=label)
at = seq(2,5,0.5)
label = as.character(at)
axis(2, at=at, label=label)

idx = which(x>mag.lim.1 & x<mag.lim.2)
xg = x[idx]
lyg = ly[idx]
lines(xg, lyg, pch=19, col=4, type='s', lwd=2)

n = length(xg)
M = matrix(1, nrow=n, ncol=2)
Y = matrix(lyg, nrow=n, ncol=1)
M[,1] = xg
fit = solve(t(M) %*% M) %*% (t(M) %*% Y)

xc = c(0,max(mag))
yc = fit[1]*xc + fit[2]
lines(xc, yc, col=4, lty=2)

ly.pred = fit[1]*x + fit[2]
y.pred = 10^ly.pred

par(mar=mar2, fig=fig2, new=T)
ylab = expression(paste(italic(N) %*% '1e4'))
plot(x, y.pred/1e4, type='l', col=4, xlim=xlim, xlab=xlab, ylab=ylab,
     cex.lab=1.3, cex.axis=1.1, lty=2)
lines(x, y/1e4, type='s')

cpl = y/y.pred
cpl[x<=mag.lim.2] = 1
cpl = predict(smooth.spline(x,cpl))$y
cpl[cpl>1] = 1
scpl = cpl*50000

lines(x, scpl/1e4, col=2)
axis(4, at=seq(0,50000,10000)/1e4, labels=seq(0,1,0.2), col=2, col.axis=2)
## dev.off()

xx = seq(10, 30, by=0.001)
yy = predict(smooth.spline(x,cpl), xx)$y
yy[yy>1] = 1
yy[xx>21.45] = 0
com = cbind(xx, yy)
com = round(com,3)
## f.com = paste0(dir,'lum_fun.dat')
## write.table(com, f.com, row.names=F, col.names=F, quote=F, sep='   ')


f.csv = '~/Work/m33_phaseII/lmc_ofiles/mira.cat.csv'
csv = read.csv(f.csv)
mi = csv[,'I']
mi = mi[mi>9]
mi = mi + mag.shift
h1 = hist(mi, plot=F, breaks=20)
par(fig=fig3, new=T, tck=0.02, mar=mar3)
plot(h1, main='', yaxt='n', ylab='', col=hcol1, xlab=xlab, cex.lab=1.3, cex.axis=1.1)

h2 = h1
idx = match(h2$mids, com[,1])
h2$counts = h2$counts * com[idx,2]
plot(h2, add=T, col=hcol2)

lines(com[,1],com[,2]*max(h1$counts)*0.8, col=2, lty=1, lwd=2)

dev.off()
