outdir = '~/Work/m33_phaseII/draft/v3.0/figures/'

f.dat = paste0(outdir, 'figure2.dat')
dat = read.table(f.dat)
x = dat[,1]
y = dat[,2]
e = dat[,3]

w = 7
h = w * 2 / (1 + sqrt(5))
f.eps = paste0(outdir,'figure2.eps')
setEPS()
postscript(f.eps, width=w, height=h)
mar = c(5,5,3,3)
par(tck=0.04, cex.axis=1.1, cex.lab=1.3, mar=mar)

xlab = bquote(italic(I)~' [mag]')
ylab = bquote(sigma~' [mag]')
plot(x, y, xlab=xlab, ylab=ylab, pch=19,
     xlim=c(15.25,20.25),
     ylim=c(0,0.2))
arrows(x, y-e, x, y+e, code=3, length=0.01, angle=90)
at = seq(15,25, 0.25)
axis(1, at=at, labels=F, tck=0.015)
at = seq(0, 0.3, 0.01)
axis(2, at=at, labels=F, tck=0.015)
dev.off()
