rm(list=ls())

figdir = '~/Work/m33_phaseII/mira_cata_sampsize/snapshots/'
pngdir = '~/Work/m33_phaseII/mira_cata_sampsize/snapshots/'

library(FITSio)
f.lst = '~/Work/m33_phaseII/MasseyVI/m33_cad_vi_sampsize.dat'
if (T) {
    dat = read.table(f.lst, header=T)
    dat[,1] = as.character(dat[,1])
    dat[,2] = as.character(dat[,2])
    idx = dat[,5] != 0
    dat = dat[idx,]
}

sky2xy = function(ra, dec) {
    cmd = paste0('~/Programs/wcstools-3.9.2/bin/sky2xy ',lf.fits,' ',ra,' ',dec,' > xy.tmp')
    system(cmd)
    xy = read.table('xy.tmp')
    if (ncol(xy) > 6) {
        xy = -1
    } else {
        xy = xy[1,5:6]
    }
    system('rm -f xy.tmp')
    return(xy)
}


n.dat = nrow(dat)
xys = as.data.frame(matrix(NA, ncol=2, nrow=n.dat))
fs.fits = rep(NA, n.dat)
fitsdir = '~/Work/m33_mira/astrometry/massey_reduced/'
for (i in 1:n.dat) {
    msg = paste0('   >> [xys] ',round(i*100/n.dat), ' %   \r')
    message(msg, appendLF=F)
    f.fits = 'M33CVm.fits'
    lf.fits = paste0(fitsdir, f.fits)
    xy = sky2xy(dat[i,5], dat[i,6])
    if (xy[1] != -1) {
        xys[i,1] = xy[1]
        xys[i,2] = xy[2]
        fs.fits[i] = f.fits
    } else {
        f.fits = 'M33NVm.fits'
        lf.fits = paste0(fitsdir, f.fits)
        xy = sky2xy(dat[i,5], dat[i,6])
        if (xy[1] != -1) {
            xys[i,1] = xy[1]
            xys[i,2] = xy[2]
            fs.fits[i] = f.fits
        } else {
            f.fits = 'M33SVm.fits'
            lf.fits = paste0(fitsdir, f.fits)
            xy = sky2xy(dat[i,5], dat[i,6])
            if (xy[1] != -1) {
                xys[i,1] = xy[1]
                xys[i,2] = xy[2]
                fs.fits[i] = f.fits
            } else {
                stop(dat[i,1])
            }
        }
    }
}
print('')

idx = order(fs.fits)
xys = xys[idx,]
dat = dat[idx,]
fs.fits = fs.fits[idx]
f.fits = fs.fits[1]


rad = 25
colorbarshift = 100
white.length = 1.1
width = 5
height = 5

for (i in 1:n.dat) {
    msg = paste0('   >> [fig] ',round(i*100/n.dat), ' %   \r')
    message(msg, appendLF=F)
    id = dat[i,1]
    sid = gsub('.slc','',id)
    ra = dat[i,5]
    dec = dat[i,6]
    field = substr(id,1,3)
    tid = gsub(field,'',sid)
    tmid = dat[i,2]
    while (nchar(tid) < 5) tid = paste0('0',tid)
    tid = paste0(field,tid)

    if (fs.fits[i] != f.fits | !exists('fits')) {
        f.fits = fs.fits[i]
        lf.fits = paste0(fitsdir, f.fits)
        fits = readFITS(lf.fits)
    }
    x = round(xys[i,1])
    y = round(xys[i,2])
    dim = dim(fits$imDat)
    if (x>rad & y>rad & x<(dim[1]-rad) & y<(dim[2]-rad)) {
        sub = fits$imDat[(x-rad):(x+rad),(y-rad):(y+rad)]
    } else {
        x1 = max(1, x-rad)
        x2 = min(x+rad, dim[1])
        y1 = max(1, y-rad)
        y2 = min(y+rad, dim[2])
        ssub = fits$imDat[x1:x2,y1:y2]
        sub = matrix(median(ssub)-colorbarshift, 2*rad+1, 2*rad+1)
        sub[(x1:x2)-x+rad+1, (y1:y2)-y+rad+1] = ssub
    }
    sub = sub - min(sub) + colorbarshift
    sub[1:round(rad*white.length),round(rad*1.87):(1+rad*2)] = colorbarshift
    f.eps = paste0(figdir, sid, '_',tmid, '_img.eps')
    setEPS()
    postscript(f.eps, width=width, height = height)
    par(mar=c(0,0,0,0))
    image(log10(sub), col=gray((255:0)/255), xaxt='n', yaxt='n')
    text(0, 0.95, tmid, adj=0, cex=1.5)
    dev.off()
}
print('')
