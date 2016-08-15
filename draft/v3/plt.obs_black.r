lcdir = '~/Work/m33_phaseII/m33_ofiles/islcs/'
figdir = '~/Work/m33_phaseII/draft/v3.0/figures/'
tmpdir = '~/Desktop/tmp/'

f.dat = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
dat = read.table(f.dat)
dat[,1] = as.character(dat[,1])
n.dat = nrow(dat)
idx = sample(1:n.dat)
dat = dat[idx,]

size.direct = 11.5 * 11.5 # squared arc min
size.sss = 9.6 * 4.8 

f.jds = paste0(tmpdir,'jds.dat')
if (F) {
write('# jds of all the light curves', f.jds)
for (i in 1:n.dat) {
    msg = paste0('    >> ',round(i*100/n.dat,2), ' %    \r')
    message(msg, appendLF=F)
    cmd = paste0('awk \'{print $1}\' ',lcdir, dat[i,1], '.slc >> ',f.jds)
    system(cmd)
}
}

## dev.off()

plotsub = function(idx, xlab='', plot=T) {
    sub = jds[idx]
    tab.sub = table(sub)
    tab.sub = tab.sub / max(tab.sub)
    xlim = c(min(sub) - 5, max(sub) + 5)
    if (plot) {
        ylim = c(0, 1)
        main = paste0('(',icnt,')')
        plot(-1e19,-1e19, xlim=xlim, ylim=ylim, xlab=xlab, yaxt='n', ylab='', main=main)
        g = 1 - tab.sub
        g = 0
        abline(v = as.numeric(names(tab.jds)), col=rgb(g,g,g), lwd=0.5)
    }
    return(xlim)
}


f.eps = paste0(figdir, 'sample_obs_black.eps')
setEPS()
postscript(f.eps, width = 8, height=8)

jds = read.table(f.jds)
tab.jds = table(jds)
fooa = names(tab.jds)
foob = tab.jds
fooc = cbind(fooa,foob)
f.foo = paste0(figdir,'sample_obs_table.dat')
write.table(fooc, f.foo, row.names=F, col.names=F, quote=F, sep='   ')

names.jd = as.numeric(names(tab.jds))
idx = names.jd > 2000
tab.jds[idx] = tab.jds[idx] * size.direct / size.sss
tab.jds = tab.jds / max(tab.jds)
idx = tab.jds > 1
tab.jds[idx] = 1

yshift = c(0,0,-0.03,-0.03)

par(mar=c(3,1,1,1), mgp=c(1,0,0), tck=0.06, cex.lab=1.3)
par(fig=c(0,1,0.7,1)+yshift, new=F)

xlim = c(min(jds)-50, max(jds)+50)
ylim = c(0, 1)
plot(-1e19,-1e19, xlim=xlim, ylim=ylim, xlab='HJD - 2450000', yaxt='n', ylab='')
g = 1 - tab.jds
g = 0
abline(v = as.numeric(names(tab.jds)), col=rgb(g,g,g))
ypos = 0.8
idx = jds > 0 & jds < 500
xlim1 = plotsub(idx, plot=F)
lines(xlim1+c(-30,30),c(ypos,ypos))
text(xlim1[2]+40, ypos, '(1)', adj=0)

ypos = 0.6
idx = jds > 500 & jds < 1000
xlim2= plotsub(idx, plot=F)
lines(xlim2+c(-30,30),c(ypos,ypos))
text(xlim2[2]+40, ypos, '(2)', adj=0)

ypos = 0.4
idx = jds > 1000 & jds < 1300
xlim3 = plotsub(idx, plot=F)
lines(xlim3+c(-30,30),c(ypos,ypos))
text(xlim3[2]+40, ypos, '(3)', adj=0)

ypos = 0.2
idx = jds > 1300 & jds < 1500
xlim4 = plotsub(idx, xlab='HJD - 2450000', plot=F)
lines(xlim4+c(-30,30),c(ypos,ypos))
text(xlim4[2]+40, ypos, '(4)', adj=0)


icnt = 1
labdist = 0
par(mar=c(1,1,1,1), mgp=c(0,labdist,0))
par(fig = c(0,0.45,0.55,0.7)+yshift, new=T)
idx = jds > 0 & jds < 500
xlim1 = plotsub(idx)

icnt = icnt + 1
par(fig = c(0,0.45,0.4,0.55)+yshift, new=T)
idx = jds > 500 & jds < 1000
xlim2= plotsub(idx)

icnt = icnt + 1
par(fig = c(0,0.45,0.25,0.4)+yshift, new=T)
idx = jds > 1000 & jds < 1300
xlim3 = plotsub(idx)

icnt = icnt + 1
par(mar=c(3,1,1,1), mgp=c(1,labdist,0))
par(fig = c(0,0.45,0.055,0.25)+yshift, new=T)
idx = jds > 1300 & jds < 1500
xlim4 = plotsub(idx, xlab='HJD - 2450000')

par(mar=c(3,3,1,1), mgp=c(1,0,0), tck=0.02)
par(fig = c(0.45,1,0.055,0.7)+yshift, new=T)
h = hist(dat[,6], plot=F)
## plot(h$mids, h$counts/max(h$counts), type='s', xlab='Number of valid measurements', ylab='Relative frequency')
h$counts = h$counts/max(h$counts)
plot(h,xlab='Number of valid measurements', ylab='Relative frequency', col='grey', main='',xlim=c(0,200))

dev.off()
