ap2apl = function(f.ap, idx.ap) {
    f.apl = gsub('.ap','.tmpl',f.ap)
    f.mag = gsub('.ap','.tmp1',f.ap)
    f.err = gsub('.ap','.tmp2',f.ap)
    cmd = paste0("awk 'NR>3&&NR%3==2' ",f.ap,' > ',f.mag)
    system(cmd)
    cmd = paste0("awk 'NR>3&&NR%3==0' ",f.ap,' > ',f.err)
    system(cmd)
    mag = read.table(f.mag)
    err = read.table(f.err)
    cmd = paste0('rm -f ',f.mag)
    system(cmd)
    cmd = paste0('rm -f ',f.err)
    system(cmd)
    cmd = paste0('rm -f ',f.ap)
    system(cmd)
    x = mag[,2]
    y = mag[,3]
    m = mag[,3 + idx.ap]
    e = err[,3 + idx.ap]
    edge.pix = 30
    x0 = 800
    y0 = 1040
    dist = (x - x0)^2 + (y - y0)^2
    r0sqr = 70^2
    idx = m < 90 & m > -90 & e < 9 & x > edge.pix & x < max(x) - edge.pix & y > edge.pix & y < max(y) - edge.pix & dist > r0sqr
    mag = mag[idx,]
    err = err[idx,]
    id = mag[,1]
    x = mag[,2]
    y = mag[,3]
    m = mag[,3 + idx.ap]
    e = err[,3 + idx.ap]
    sky = err[,1]
    apl = cbind(id,x,y,m,e,sky)
    return(apl)
}
  

source('~/Work/m33_mira/codes/M33MiraRlib/fun_phot.r')

daophot = '~/Programs/dao/daophot'
allstar = '~/Programs/dao/allstar'
dir = '~/Work/m33_mira/phot/i/x1i_new/'
fs.fits = list.files(dir,pattern='ii.....fits$')
for (f.fits in fs.fits) {
    print(f.fits)
    ret = dao.find.stars(dir,f.fits,daophot=daophot)
    f.ap = gsub('.fits','.ap',f.fits)
    lf.ap = paste0(dir,f.ap)
    apl = ap2apl(lf.ap,1)
    plot(apl[,'m'],apl[,'e'],pch=19,cex=0.3,col=rgb(0,0,0,0.5),main=f.fits)
    f.apl = gsub('.fits','.apl',f.fits)
    lf.apl = paste0(dir,f.apl)
    ts = ' NL    NX    NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
    write(ts,lf.apl)
    ts = '  1  2035  2045   668.1 20000.0  176.36    7.00   4.000  10.000    7.47'
    write(ts,lf.apl,append=T)
    write('',lf.apl,append=T)
    n.apl = nrow(apl)
    for (i in 1:n.apl) {
        tt = apl[i,]
        ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.2f%9.0f%9.2f%9.3f',
            tt[1],tt[2],tt[3],tt[4],tt[5],tt[6],5.,0.,0.)
        write(ts,lf.apl,append=T)
    }
}




