figdir = '~/Work/m33_phaseII/mira_cata_sampsize/snapshots/'
pngdir = '~/Work/m33_phaseII/mira_cata_sampsize/snapshots/'

library(FITSio)
f.lst = '~/Work/m33_phaseII/mira_cata_sampsize/mapids4snap.dat'
if (T) {
    dat = read.table(f.lst)
    dat[,1] = as.character(dat[,1])
    for (i in 4:7) dat[,i] = as.character(dat[,i])
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


for (j in 4:7) {
    idx = order(dat[,j])
    dat = dat[idx,]

    idx = !is.na(dat[,j])
    odat = dat[idx,]
    
    n.dat = nrow(odat)
    old.field = substr(dat[1,j],1,3)
    
    for (i in 1:n.dat) {
        id = odat[i,1]
        sid = id
        ra = odat[i,2]
        dec = odat[i,3]
        pid = odat[i, j]

        field = substr(pid,1,3)
        tfield = substr(id,1,3)
        
        tid = gsub(tfield,'',sid)
        while (nchar(tid) < 5) tid = paste0('0',tid)
        tid = paste0(tfield,tid)
        
        tpid = gsub(field,'',pid)
        while (nchar(tpid) < 5) tpid = paste0('0',tpid)
        tpid = paste0(field,tpid)

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
        if (substr(field,1,1) == 'w') sub = t(sub[,ncol(sub):1])
        sub[1:round(rad*white.length),round(rad*1.87):(1+rad*2)] = colorbarshift
        f.eps = paste0(figdir, sid, '_', pid, '_img.eps')
        setEPS()
        postscript(f.eps, width=width, height = height)
        par(mar=c(0,0,0,0))
        image(log10(sub), col=gray((255:0)/255), xaxt='n', yaxt='n')
        text(0, 0.95, tpid, adj=0, cex=2.5)
        dev.off()
    }
}
