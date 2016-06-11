set.seed(101)
dir = '~/Work/m33_phaseII/draft/v1/figures/'
flcdir = '~/Work/m33_phaseII/sim_ofiles/flc_mira/'
mlcdir = '~/Work/m33_phaseII/lmc_ofiles/mira.lcs/'
slcdir = '~/Work/m33_phaseII/m33_ofiles/islcs/'

mag.shift = 6.2
## fs.flc = list.files(flcdir)
## nfs = length(fs.flc)
## idx = sample(1:nfs,1)
## f.flc = fs.flc[idx]
f.flc = 'mira_42896_wki11760_204.74.flc'

width = 10
height = 8
f.eps = paste0(dir, 'simmira.eps')
setEPS()
postscript(f.eps, width=width, height=height)

par(mfrow=c(2,1), mar=c(2,5,3,1))

mid = substr(f.flc, 6, 10)
f.mlc = paste0(mlcdir, 'OGLE-LMC-LPV-',mid,'.dat')

wid = substr(f.flc, 12, 100)
wid = gsub('_','----------------------------',wid)
wid = substr(wid, 1, 20)
wid = gsub('-','',wid)
f.slc = paste0(slcdir, wid, '.slc')

tshift = substr(f.flc, 12, 100)
tshift = gsub(wid,'',tshift)
tshift = gsub('_','',tshift)
tshift = gsub('.flc','',tshift)
tshift = as.numeric(tshift)

lf.flc = paste0(flcdir, f.flc)

mlc = read.table(f.mlc)
x = mlc[,1]
y = mlc[,2] + mag.shift
e = mlc[,3]
xlab = 'HJD - 2450000'
ylab = expression(paste(italic('I'),' + 6.2 [mag]'))
main = ''
xlim = c(min(x)-5, max(x)+5)
ylim = c(max(y)+0.3, min(y)-0.3)
plot(x, y, xlab='', ylab=ylab, pch=19, cex=0.5, main=main, xlim=xlim, ylim=ylim, cex.axis=1.1, cex.lab=1.3)
arrows(x, y+e, x, y-e, angle=90, code=3, length=0.001)

f.lmc.cat = paste0('~/Work/m33_phaseII/lmc_ofiles/','mira','.cat.csv')
lmc.cat = read.csv(f.lmc.cat)[,c(1,9,11)]
lmc.cat[,1] = as.character(lmc.cat[,1])

f.para = '~/Work/m33_phaseII/lmc_ofiles/allLMC.txt'
f.basicfun = '~/Work/m33_phaseII/code.phaseii/GPcodes/r_v2/BasicFuns.R'
library(VarStar)
source(f.basicfun)
paraAll = read.table(f.para, header=FALSE, stringsAsFactors = F)
paraIDs = paraAll[,1]
thetasAll = as.matrix(paraAll[,3:9])
thetasAll = abs(thetasAll)
thetasAll[,4] = 100000

idx.para = paraIDs == paste0('OGLE-LMC-LPV-',mid)
id.para = paraIDs[idx.para]
ts.thetas = thetasAll[idx.para,]
period = lmc.cat[lmc.cat[,1]==id.para,'P_1']
x = new(gpModel, mlc[,1], mlc[,2], mlc[,3], mean(mlc[,2]))
x$set_period(period,0)
x$gp_setTheta(ts.thetas)
xcnt = seq(min(mlc[,1]), max(mlc[,1]), 1)
ycnt1 = x$gp_predict(xcnt,1)$predy
ycnt3 = x$gp_predict(xcnt,3)$predy
ycnt2 = x$gp_predict(xcnt,2)$predy
ycnt = ycnt1+ycnt2+ycnt3-2*mean(mlc[,2]) + mag.shift
lines(xcnt,ycnt,col=4)
xs = c(min(mlc[,1]), min(mlc[,1]) + tshift)
thisy = ylim[1] - 0.1*(ylim[1]-ylim[2])
arrows(xs[1], thisy, xs[2], thisy, lty=1, lwd=2, col=4, code=3, length=0.05)
thisy = ylim[1] - 0.05*(ylim[1]-ylim[2])
text(mean(xs), thisy, expression(italic('s')), col=4, cex=1.3)

par(mar=c(5,5,0,1))
slc = read.table(f.slc)
shifted.t = slc[,1] - min(slc[,1]) + min(mlc[,1]) + tshift
abline(v = shifted.t, lwd=0.5)

flc = read.table(lf.flc)
x = flc[,1]
y = flc[,2]
e = flc[,3]
x = x - min(x) + min(mlc[,1]) + tshift
ylab = expression(paste(italic('I'),' [mag]'))
plot(x, y, pch=19, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, cex=0.5,
     cex.axis=1.1, cex.lab=1.3)
arrows(x, y-e, x, y+e,
       code=3, length=0.0, angle=90)
lines(xcnt,ycnt,col=4)
dev.off()
