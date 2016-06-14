outdir = '~/Work/m33_phaseII/draft/v1/figures/'

f.eps = paste0(outdir, 'pl.eps')
setEPS()
postscript(f.eps, width=8, height=6)

par(tck=0.02, mgp=c(1.7,0.2,0), cex.lab=1.1, cex.axis=1.1)
mar = c(3,4,1,1)
par(mar=mar, mfrow=c(2,1))
cex1 = 0.5
cex2 = 0.3

dir = '~/Work/m33_phaseII/draft/v1/tables/'
f.dat = paste0(dir, 'pars.dat')
dat = read.table(f.dat, stringsAsFactors=F)

idx = dat[,8] == 'O-rich'
orh = dat[idx,]
crh = dat[!idx,]

xlab = expression(paste('log (',italic(P),')'))
ylab = expression(paste(italic(I)[min],' [mag]'))
x = log10(crh[,5])
y = crh[,7]
xlim = c(1.9,3.2)
ylim = c(21,17)
plot(x, y, pch=1, cex=cex2, col=2, ylim=ylim, xlim=xlim,
     xlab=xlab, ylab=ylab)

x = log10(orh[,5])
y = orh[,7]
points(x, y, pch=1, cex=cex1, col=4)


##############
ylab = expression(paste('<',italic(I),'> - A/2',' [mag]'))
x = log10(crh[,5])
y = crh[,4] - crh[,6]/2
xlim = c(1.9,3.2)
ylim = c(21,17)
plot(x, y, pch=1, cex=cex2, col=2, ylim=ylim, xlim=xlim,
     xlab=xlab, ylab=ylab)

x = log10(orh[,5])
y = orh[,4] - orh[,6]/2
points(x, y, pch=1, cex=cex1, col=4)


