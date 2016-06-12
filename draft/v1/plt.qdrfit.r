f.lc = '~/Work/m33_phaseII/sim_ofiles/flc_mira/mira_29842_wei8720_182.29.flc'
outdir = '~/Work/m33_phaseII/draft/v1/figures/'

f.eps = paste0(outdir, 'quadratic_fit.eps')
setEPS()
postscript(f.eps, width=10, height=6.5)

par(tck=0.03, mgp=c(1.7,0.2,0), cex.lab=1.3, cex.axis=1.1)
fig1 = c(0,1,0.5,1)
fig2 = c(0,1,0,0.5)
mar1 = mar2 = c(3,4,1,1)
par(fig = fig1, mar=mar1)
lwd = 1.3


qdr.fit.res = function(a) {
    x = a[,1]
    mean.x = mean(x)
    x = x - mean.x
    y = a[,2]
    n = nrow(a)
    X = matrix(1, ncol=3, nrow=n)
    X[,1] = x^2
    X[,2] = x
    Y = matrix(y, ncol=1, nrow=n)
    b = solve(t(X) %*% X) %*% (t(X) %*% Y)
    xc = seq(min(x), max(x), length=100)
    yc = b[1]*xc^2 + b[2]*xc + b[3]
    lines(xc+mean.x,yc,col=4, lwd=lwd)
    residuals = y - (b[1]*x^2 + b[2]*x + b[3])
    return(residuals)
}
seps = c(0,500,800,1200,2000,2800,3100,5000)
grp.sds = rep(NA, 7)
all.residuals = c()
no.data = T
lc = read.table(f.lc)
x = lc[,1]
y = lc[,2]
e = lc[,3]
xlab = 'HJD - 2450000'
ylab = expression(paste(italic(I), ' [mag]'))
plot(x, y, pch=19, ylim=c(max(y)+0.2,min(y)-0.2), xlab=xlab, ylab=ylab, main='', cex=0.5)
arrows(x, y+e, x, y-e, code=3, length=0, angle=90)
x = lc[,1]
for (i.grp in 1:7) {
    idx = x > seps[i.grp] & x < seps[i.grp+1]
    if (sum(idx) > 7) {
        grp = lc[idx,]
        grp.sds[i.grp] = sd(grp[,2])
        all.residuals = c(all.residuals, qdr.fit.res(grp))
        no.data = F
    }
}


par(fig = fig2, new=T, mar=mar2)
f.dat = '~/Work/m33_phaseII/sc_ext_ftrs/qdr_fit_features.dat'
## dat = read.table(f.dat)
dat[,1] = as.character(dat[,1])
idx = substr(dat[,1],1,3) == 'mir'
mir = dat[idx,]
idx = substr(dat[,1],1,3) == 'srv'
srv = dat[idx,]
idx = substr(dat[,1],1,3) == 'con'
con = dat[idx,]
print(paste(nrow(mir), nrow(srv), nrow(con)))

breaks = 200
h1 = hist(mir[,4], breaks=breaks, plot=F)
h2 = hist(srv[,4], breaks=breaks, plot=F)
h3 = hist(con[,4], breaks=breaks, plot=F)
xlab = expression(paste(sigma,'(',italic(R)[italic(q)],')','/',sigma,'(',italic(m),')'))
ylab = 'Counts'
plot(h1$mids, h1$counts, type='s', col=4, ylim=c(0, 6300), xlim=c(0,1.22), lwd=lwd, xlab=xlab, ylab=ylab)
lines(h2$mids, h2$counts, col=2, type='s', lwd=lwd)
lines(h3$mids, h3$counts, col=1, type='s', lwd=lwd)
leg = c('Mira','SRV','Constant star')
legend(0, 6.5e3, leg, lwd=lwd, col=c(4,2,1), bty='n')

dev.off()
