set.seed(101)
dir = '~/Work/m33_phaseII/draft/figures/'
flcdir = '~/Work/m33_phaseII/sim_ofiles/flc_mira/'
mlcdir = '~/Work/m33_phaseII/lmc_ofiles/mira.lcs/'
slcdir = '~/Work/m33_phaseII/m33_ofiles/islcs/'

fs.flc = list.files(flcdir)
nfs = length(fs.flc)
idx = sample(1:nfs,1)
f.flc = fs.flc[idx]

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
y = mlc[,2]
e = mlc[,3]
xlab = 'HJD - 2450000'
ylab = 'I [mag]'
main = ''
xlim = c(min(x)-5, max(x)+5)
ylim = c(max(y)+0.3, min(y)-0.3)
plot(x, y, xlab=xlab, ylab=ylab, pch=19, cex=0.5, main=main, xlim=xlim, ylim=ylim)
arrows(x, y+e, x, y-e, angle=90, code=3, length=0.001)

f.lmc.cat = paste0('~/Work/m33_phaseII/lmc_ofiles/','mira','.cat.csv')
lmc.cat = read.csv(f.lmc.cat)[,c(1,9,11)]
lmc.cat[,1] = as.character(lmc.cat[,1])

f.para = '~/Work/m33_phaseII/lmc_ofiles/allLMC.txt'
f.basicfun = '~/Work/m33_phaseII/code.phaseii/GPcodes/r_v2/BasicFuns.R'
library(VarStar)
source(f.basicfun)
paraAll = read.table(f.para, header=FALSE)
paraIDs = paraAll[,1]
thetasAll = as.matrix(paraAll[,3:9])
thetasAll = abs(thetasAll)
thetasAll[,4] = 100000

