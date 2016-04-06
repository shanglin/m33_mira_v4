dir = '~/Work/m33_phaseII/m33_ofiles/complete_fun/'
f.eps = paste0(dir,'lum_fun.eps')
width=7
height = width*1.618
setEPS()
postscript(f.eps, width=width, height=height)
f.dat = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
dat = read.table(f.dat)
mag = dat[,4]
mag.lim.1 = 18.5
mag.lim.2 = 20
par(mfrow=c(2,1))
h = hist(mag, breaks=200, plot=F)
x = h$mids
y = h$counts
ly = log10(y)
plot(x, ly, xlim=c(16,21.5), ylim=c(1,4.2),type='s', xlab='I (mag)', ylab='Log (N)')
idx = which(x>mag.lim.1 & x<mag.lim.2)
xg = x[idx]
lyg = ly[idx]
lines(xg, lyg, pch=19, col=2, type='s', lwd=2)

n = length(xg)
M = matrix(1, nrow=n, ncol=2)
Y = matrix(lyg, nrow=n, ncol=1)
M[,1] = xg
fit = solve(t(M) %*% M) %*% (t(M) %*% Y)

xc = c(0,30)
yc = fit[1]*xc + fit[2]
lines(xc, yc, col=2, lty=2)

ly.pred = fit[1]*x + fit[2]
y.pred = 10^ly.pred

plot(x, y.pred, type='s', col=2, xlim=c(16,21.5), xlab='I (mag)', ylab='N')
lines(x, y, type='s')

cpl = y/y.pred
cpl[x<=mag.lim.2] = 1
cpl = predict(smooth.spline(x,cpl))$y
cpl[cpl>1] = 1
scpl = cpl*50000

lines(x, scpl, col=3)
axis(4, at=seq(0,50000,10000), labels=seq(0,1,0.2), col=3, col.axis=3)
dev.off()

xx = seq(10, 21.45, by=0.001)
yy = predict(smooth.spline(x,cpl), xx)$y
yy[yy>1] = 1
com = cbind(xx, yy)
com = round(com,3)
f.com = paste0(dir,'lum_fun.dat')
write.table(com, f.com, row.names=F, col.names=F, quote=F, sep='   ')
