dir = '~/Work/m33_mira/cmb_lcs/light_curves/m33_islcs/'
f.dat = '~/Work/m33_mira/cmb_lcs/light_curves/m33_i_catalog.dat'

dat = read.table(f.dat)
dat[,1] = as.character(dat[,1])
dat = dat[dat[,6]>9,]
idx = order(dat[,4])
dat = dat[idx,]

seq = seq(1, nrow(dat), 100)
for (i in seq) {
    id = dat[i,1]
    m = dat[i,4]
    f.lc = paste0(dir,id,'.slc')
    lc = read.table(f.lc)
    x = lc[,1]
    y = lc[,2]
    e = lc[,3]
    xlim = c(100,4000)
    ylim = c(m+2, m-2)
    plot(x, y, pch=19, cex=0.6, ylim=ylim, xlim=xlim, main=m)
    arrows(x, y-e, x, y+e, code=3, angle=90, length=0)
    Sys.sleep(0.2)
}

## It is reasonable to ignore light curves fainter than 21.45 mag, where the luminosity function starts to drop.
