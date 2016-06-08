figdir = './figs/'
pngdir = './pngs/'

library(FITSio)
f.co = 'cadco.csv'
f.dat = 'm33i_bright.dat'
if (F) {
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
    cmd = paste0('sky2xy ',f.fits,' ',ra,' ',dec,' > xy.tmp')
    system(cmd)
    xy = read.table('xy.tmp')[1,5:6]
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
    f.fits = paste0(field,'w.fits')
    if (field != old.field | !exists('fits')) {
        fits = readFITS(f.fits)
        old.field = field
    }
    xy = sky2xy(ra, dec)
    x = round(xy[1,1])
    y = round(xy[1,2])
    sub = fits$imDat[(x-rad):(x+rad),(y-rad):(y+rad)]
    sub = sub - min(sub) + colorbarshift
    sub[1:round(rad*white.length),round(rad*1.87):(1+rad*2)] = colorbarshift
    f.eps = paste0(figdir, sid, '_img.eps')
    setEPS()
    postscript(f.eps, width=width, height = height)
    f.png = 
    par(mar=c(0,0,0,0))
    image(log10(sub), col=gray((255:0)/255), xaxt='n', yaxt='n')
    text(0, 0.95, tid, adj=0, cex=2.5)
    dev.off()
}
