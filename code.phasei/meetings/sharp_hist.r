out.dir = '~/Work/m33_mira/meeting/mar24_2016/'
fig.dir = paste0(out.dir, 'figs/')

field = 'x1i'
dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/')
lc.dir = paste0(dir, 'trialp_all_',field,'/')
f.fnl = paste0(field,'.fnl')
lf.fnl = paste0(lc.dir, f.fnl)
fnl = read.table(lf.fnl, skip=1)

mag = fnl[,4]
sharpness = fnl[,8]
chi = fnl[,7]
main = paste0('Field: ', field)

mag.1 = 22.5
mag.2 = 23.5
f.png = paste0(fig.dir, 'mag_regions.png')
png(f.png)
hist(mag, breaks=100, main = main, xlab = 'Instrumental I (mag)')
abline(v = mag.1, col=2)
abline(v = mag.2, col=2)
dev.off()

idx = mag > mag.1 & mag < mag.2
cnl = fnl[idx,]

f.png = paste0(fig.dir, 'sharpness.hist.png')
png(f.png)

hist(sharpness, breaks=100, main = main)
dev.off()

idx = order(cnl[,8])
cnl = cnl[idx,]
sharps = seq(-6,6,0.3)
nsharps = length(sharps)
id.last = 0
icnt = 1
for (i in 1:nsharps) {
    sharp = sharps[i]
    idx = which.min(abs(cnl[,8]-sharp))
    id = cnl[idx,1]
    if (id != id.last) {
        f.png = paste0(fig.dir, 'lc_shp_',icnt,'.png')
        png(f.png)
        f.lc = paste0(lc.dir, 'lcV.', id)
        lc = read.table(f.lc)
        x = lc[,1] - 2450000
        y = lc[,2]
        e = lc[,3]
        ts.main = paste0('Sharpness = ',round(cnl[idx,8],1),' (id ',id,')')
        ylim = c(cnl[idx,4]+1, cnl[idx,4]-1)
        xlim = c(1350,1500)
        plot(x,y, main = ts.main, pch=19, ylim = ylim, xlim = xlim)
        arrows(x, y-e, x, y+e, angle=90, code=3, length=0.001)
        id.last = id
        icnt = icnt + 1
        dev.off()
    }
}

f.png = paste0(fig.dir,'chi_hist.png')
png(f.png)
hist(chi[chi<3], breaks=100, main=main, xlab='chi_fnl')
arrows(2.7,50,3,50,length=0.1)
text(2.9,100, paste0('Max ~ ',round(max(chi))))
dev.off()

cs = seq(0, 30, 0.5)
ncs = length(cs)
id.last = 0
icnt = 1
for (i in 1:ncs) {
    c = cs[i]
    idx = which.min(abs(cnl[,7]-c))
    id = cnl[idx,1]
    if (id != id.last) {
        f.png = paste0(fig.dir, 'lc_chi_',icnt,'.png')
        png(f.png)
        f.lc = paste0(lc.dir, 'lcV.', id)
        lc = read.table(f.lc)
        x = lc[,1] - 2450000
        y = lc[,2]
        e = lc[,3]
        ts.main = paste0('Chi = ',round(cnl[idx,7],1),' (id ',id,')')
        ylim = c(cnl[idx,4]+1, cnl[idx,4]-1)
        xlim = c(1350,1500)
        plot(x,y, main = ts.main, pch=19, ylim = ylim, xlim = xlim)
        arrows(x, y-e, x, y+e, angle=90, code=3, length=0.001)
        id.last = id
        icnt = icnt + 1
        dev.off()
    }
}
