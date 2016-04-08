dir = '~/Work/m33_phaseII/m33_ofiles/islcs/'
f.dat = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
## dat = read.table(f.dat)
dat[,1] = as.character(dat[,1])
n = nrow(dat)
dat = dat[order(dat[,4]),]

for (i in 1000:n) {
    f.lc = paste0(dir, dat[i,1], '.slc')
    lc = read.table(f.lc)
    m = dat[i,4]
    x = lc[,1]
    y = lc[,2]
    e = lc[,3]
    xlim = c(100,4000)
    ylim = c(m+0.5, m-0.5)
    par(mfrow=c(2,1))
    plot(x, y, pch=19, cex=0.6, ylim=ylim, xlim=xlim, main=m)
    arrows(x, y-e, x, y+e, code=3, angle=90, length=0)

    ylim = c(max(y)+0.3, min(y)-0.3)
    plot(x, y, pch=19, cex=0.6, ylim=ylim, xlim=xlim, main=dat[i,1])
    arrows(x, y-e, x, y+e, code=3, angle=90, length=0)
    Sys.sleep(0.2)
}


