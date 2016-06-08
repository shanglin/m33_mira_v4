figdir = '~/Work/m33_phaseII/mira_cata_sampsize/snapshots/'
pngdir = '~/Work/m33_phaseII/mira_cata_sampsize/snapshots/'

library(FITSio)
f.co = '~/Work/m33_phaseII/mira_cata_sampsize/cadco.csv'
f.dat = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
if (T) {
    co = read.csv(f.co)
    dat = read.table(f.dat)
    co[,1] = as.character(co[,1])
    idx = order(co[,1])
    co = co[idx,]
    dat[,1] = as.character(dat[,1])
    dat[,1] = paste0(dat[,1], '.slc')
    idx = match(co[,1], dat[,1])
    dat = dat[idx,]
}

sky2xy = function(ra, dec) {
    cmd = paste0('~/Programs/wcstools-3.9.2/bin/sky2xy ',f.fits,' ',ra,' ',dec,' > xy.tmp')
    system(cmd)
    xy = read.table('xy.tmp')[1,5:6]
    system('rm -f xy.tmp')
    return(xy)
}

rad = 20
colorbarshift = 100
white.length = 0.8
width = 5
height = 5

n.co = nrow(co)
## n.co = 1
old.field = substr(co[1,1],1,3)
for (i in 1:n.co) {
    id = co[i,1]
    sid = gsub('.slc','',id)
    ra = dat[i,2]
    dec = dat[i,3]
    field = substr(id,1,3)
    tid = gsub(field,'',sid)
    while (nchar(tid) < 5) tid = paste0('0',tid)
    tid = paste0(field,tid)
    fitdir = paste0('~/Work/m33_mira/astrometry/',field,'/')
    f.fits = paste0(fitdir, field,'w.fits')
    if (field != old.field | !exists('fits')) {
        fits = readFITS(f.fits)
        old.field = field
    }
    xy = sky2xy(ra, dec)
    x = round(xy[1,1])
    y = round(xy[1,2])
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
    if (substr(field,1,1) == 'w') sub = t(sub[,ncol(sub):1]) ## roughtly north is up and left is east
    sub[1:round(rad*white.length),round(rad*1.87):(1+rad*2)] = colorbarshift
    f.eps = paste0(figdir, sid, '_img.eps')
    setEPS()
    postscript(f.eps, width=width, height = height)
    par(mar=c(0,0,0,0))
    image(log10(sub), col=gray((255:0)/255), xaxt='n', yaxt='n')
    text(0, 0.95, tid, adj=0, cex=2.5)
    dev.off()
}
