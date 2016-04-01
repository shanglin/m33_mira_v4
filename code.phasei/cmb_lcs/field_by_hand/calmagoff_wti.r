## library(stats)
rm.file = function(f) {
    cmd = paste0('rm -f ',f)
    system(cmd)
}

my.lm = function(x, y) {
    n = length(x)
    d = matrix(1, ncol=2, nrow=n)
    d[,1] = x
    Y = matrix(y, ncol=1, nrow=n)
    bet = solve(t(d) %*% d) %*% (t(d) %*% Y)
    return(bet)
}

field = 'wti'

dir = '~/Work/m33_mira/cmb_lcs/mag_off/'
png.dir = paste0(dir, 'pngs/')
cmd = paste0('mkdir -p ',png.dir)
system(cmd)
phot.dir = '~/Work/m33_mira/phot/i/'
f.dat = '~/Work/m33_mira/astrometry/massey/massey_i.dat'
dat = read.table(f.dat, skip=1)
f.rd = 'massey_rd_wti.txt'
lf.rd = paste0(dir, f.rd)
cmd = paste0('awk \'NR>1 {print $2,$3}\' ',f.dat,' > ',lf.rd)
system(cmd)
dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
code.daomatch = paste0(dir.fun, 'fun.dao.match.r')
source(code.daomatch)
sky2xy = '~/Programs/wcstools-3.9.2/bin/sky2xy'
daomatch = '~/Programs/dao_e2/daomatch'
daomaster = '~/Programs/dao_e2/daomaster'

f.zer = 'fnl_zero_point_wti.dat'
lf.zer = paste0(dir, f.zer)
ts = '# field     a     b    ## true mag = instrumetal_mag * a + b'
write(ts, lf.zer)

phot.dir = '~/Work/m33_mira/phot/i/'
## fields = list.files(phot.dir, pattern='....allframe')

## for (lfield in fields) {
    ## field = substr(lfield,1,3)
    print(field)
    fnl.dir = paste0(phot.dir, field, '_allframe/trialp_all_', field, '/')
    f.fnl = paste0(field, '.fnl')
    of.fnl = paste0(fnl.dir, f.fnl)
    lf.fnl = paste0(dir, f.fnl)
    cmd = paste0('cp ',of.fnl,' ',lf.fnl)
    system(cmd)
    ast.dir = paste0('~/Work/m33_mira/astrometry/',field,'/')
    f.fits = paste0(field, 'w.fits')
    wf.fits = paste0(ast.dir, f.fits)
    lf.fits = paste0(dir, f.fits)
    cmd = paste0('cp ',wf.fits,' ',lf.fits)
    system(cmd)

    f.rdxy = 'rdxy.tmp'
    lf.rdxy = paste0(dir, f.rdxy)
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(sky2xy,' ',f.fits,' @',f.rd,' > ',f.rdxy)
    system(cmd)
    setwd(dir.current)

    rdxy = read.table(lf.rdxy, fill=T)
    idx = which(rdxy[,7] == '')
    n.idx = length(idx)
    if (n.idx < 10) stop('Too few stars in common.')
    f.faf = 'tmp.faf'
    lf.faf = paste0(dir, f.faf)
    ts = ' NL    NX    NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
    write(ts,lf.faf)
    ts = '  1  2035  2045   668.1 20000.0  176.36    7.00   4.000  10.000    7.47'
    write(ts,lf.faf,append=T)
    write('',lf.faf,append=T)
    for (i in 1:n.idx) {
        j = idx[i]
        ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.2f%9.0f%9.2f%9.3f',
            j, rdxy[j,5], rdxy[j,6], dat[j,4], 0.02, 100.00, 5., 1.50, 0.000)
        write(ts, lf.faf, append=T)
    }

    f.raf = paste0(field,'.raf')
    lf.raf = paste0(dir, f.raf)
    ts = ' NL    NX    NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
    write(ts,lf.raf)
    ts = '  1  2035  2045   668.1 20000.0  176.36    7.00   4.000  10.000    7.47'
    write(ts,lf.raf,append=T)
    write('',lf.raf,append=T)
    fnl = read.table(lf.fnl, skip=1)
    n.fnl = nrow(fnl)
    for (i in 1:n.fnl) {
        ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.2f%9.0f%9.2f%9.3f',
            fnl[i,1], fnl[i,2], fnl[i,3], fnl[i,4], fnl[i,5], 100.00, 5., 1.50, 0.000)
        write(ts, lf.raf, append=T)
    }

    f.mch = paste0(field,'.mch')
    f.tfr = paste0(field,'.tfr')
    fs.mag = c(f.faf, f.raf)
    ret = dao.match.fast(dir, fs.mag, f.mch, daomatch=daomatch, clean=T)
    ret = dao.master.mtr(dir, f.mch, daomaster=daomaster, f.tfr=f.tfr, clean=T)

    f.mtr = paste0(field, '.mtr')
    lf.mtr = paste0(dir, f.mtr)
    mtr = read.table(lf.mtr, skip=3)
    n.mtr = nrow(mtr)
    faf = read.table(lf.faf, skip=3)
    raf = read.table(lf.raf, skip=3)

    f.cmp = paste0(field, '.cmp')
    lf.cmp = paste0(dir, f.cmp)
    ts = '# massey.id   massey.m   id   m   e'
    write(ts, lf.cmp)
    for (i in 1:n.mtr) {
        mid = mtr[i,1]
        id = mtr[i,10]
        idx = mid == faf[,1]
        mmag = faf[idx,4]
        idx = id == raf[,1]
        mag = raf[idx,4]
        err = raf[idx,5]
        ts = paste(mid, mmag, id, mag, err, sep='   ')
        write(ts, lf.cmp, append=T)
    }

    cmp = read.table(lf.cmp)
    if (substr(field,1,1) == 'w') {
        mag.lim = quantile(cmp[,4], 0.25)
        sigma = 3
    } else {
        mag.lim = quantile(cmp[,4], 0.1)
        sigma = 2.3
    }
    sub = cmp[cmp[,4] < mag.lim,]

    for (i in 1:20) {
        x = sub[,4] ## this I mag
        y = sub[,2] ## Massey I mag
        ## plot(x, y, pch=19, cex=0.5, col=rgb(0,0,0,0.5), xlab='I (mag)', ylab='I_Massey (mag)')
        ## lines(c(0,30), c(0,30), col=2, lty=2)
        xy = as.data.frame(cbind(x, y))
        ## fit = lm(y ~ x, dat=xy)
        fit = my.lm(x, y)
        ## residual = y - predict(fit, xy)
        residual = y - fit[1]*x - fit[2]
        ## plot(x, residual, pch=19, cex=0.5, col=rgb(0,0,0,0.5))
        idx = abs(residual) < sigma * sd(residual)
        sub = sub[idx,]
        x = sub[,4] ## this I mag
        y = sub[,2] ## Massey I mag
        xy = as.data.frame(cbind(x, y))
        ## fit = lm(y ~ x, dat=xy)
        fit = my.lm(x, y)
    }

    f.png = paste0('zer_',field,'.png')
    lf.png = paste0(png.dir, f.png)
    png(lf.png)
    x0 = cmp[,4]
    y0 = cmp[,4] - cmp[,2]
    plot(x0, y0, pch=1, cex=0.5, col='grey', xlab='I (mag)', ylab='I_Massey (mag)', main=field)
    x = sub[,4]
    y = sub[,4] - sub[,2]
    points(x, y, pch=19, cex=0.5)
    ## a = coef(fit)[2]
    ## b = coef(fit)[1]
    a = fit[1]
    b = fit[2]
    a = round(a, 5)
    b = round(b, 5)
    xc = c(0, 30)
    yc = xc - a*xc - b 
    lines(xc, yc, col=4)
    dev.off()

    ts = paste(field, a, b, sep='   ')
    write(ts, lf.zer, append=T)
    ## x = cmp[,4]
    ## x.corr = x*a + b
    ## x.mass = cmp[,2]
    ## y =  x.mass - x.corr
    ## plot(x,y)
    ## abline(h=0,col=4)

    rm.file(lf.fnl)
    rm.file(lf.mtr)
    rm.file(lf.fits)
    rm.file(lf.raf)
    rm.file(lf.faf)
    lf.tmp = paste0(dir,'*tmp*')
    rm.file(lf.tmp)
    lf.tfr = paste0(dir,field,'.tfr')
    rm.file(lf.tfr)
    lf.mch = paste0(dir,field,'.mch')
    rm.file(lf.mch)
    ## stop()
## }
