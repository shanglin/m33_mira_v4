outdir = '~/Work/m33_phaseII/draft/v1/figures/'
f.dat = '~/Work/m33_phaseII/rf_model_sampsize/m33_predictions.dat'
## dat = read.table(f.dat, header=T, stringsAsFactors=F)

f.eps = paste0(outdir, 'mira_prob.eps')
setEPS()
postscript(f.eps, width=8, height=8*0.618)
p = dat[,'Y_prob']
h1 = hist(p, breaks=100)
## h1$counts = log10(h1$counts)
ylab = expression('Frequency'%*%'1e5')
xlab = 'Probability'
ylim = c(0, 2e5)
par(fig = c(0,1,0,1), mgp=c(1.5,0.2,0), tck=0.02, cex.lab=1.3, cex.axis=1.3, mar = c(3,3,0.5,0.3))
plot(h1, col='skyblue', main='', xlab=xlab, ylab=ylab, ylim=ylim, yaxt='n')
at = seq(0, 2, 0.5)
axis(2, at=at*1e5, labels=at)
x1 = 0.49
x2 = 1.01
y1 = -5000
y2 = 5000
lines(c(x1,x2),c(y2,y2), col=4)
lines(c(x1,x2),c(y1,y1), col=4)
lines(c(x1,x1),c(y1,y2), col=4)
lines(c(x2,x2),c(y1,y2), col=4)
x3 = 0.237
x4 = 1.037
y3 = 5.7e4
lines(c(x1,x3), c(y2,y3), col=4, lty=2)
lines(c(x2,x4), c(y2,y3), col=4, lty=2)

p2 = p[p>0.5]
h2 = hist(p2, breaks=50, plot=F)
xlim = c(0.5,1)
ylim = c(0,530)
par(fig = c(0.2,1,0.2,1), new=T, mar = c(3,3,0.5,0.3)*1.5)
plot(h2, col='skyblue', main='', xlab='', xlim=xlim, ylim=ylim)
box(col=4)
dev.off()

print(length(p2))
