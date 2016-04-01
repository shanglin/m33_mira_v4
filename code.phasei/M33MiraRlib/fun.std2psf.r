std2psf = function(dir, f.als, f.apl, f.std, f.mag = 'default',
    daomatch = 'daomatch', daomaster = 'daomaster', daophot = 'daophot') {
    dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
    code.daomatch = paste0(dir.fun, 'fun.dao.match.r')
    code.daophot = paste0(dir.fun, 'fun.dao.phot.r')
    source(code.daomatch)
    source(code.daophot)
    f.psf = gsub('.apl', '.psf', f.apl)
    f.mch = 'tmp.mch'
    f.tfr = 'tmp.tfr'
    dao.match.fast(dir, c(f.als, f.apl), f.mch, daomatch = daomatch, clean = T)
    dao.master(dir, f.mch, daomaster = daomaster, f.tfr = f.tfr, clean = T)
    lf.tfr = paste0(dir, f.tfr)
    if (file.exists(lf.tfr)) {
        tfr = read.table(lf.tfr, skip = 3)
        lf.std = paste0(dir, f.std)
        std = read.table(lf.std)
        lf.apl = paste0(dir, f.apl)
        apl = read.table(lf.apl, skip = 3)
        n.std = nrow(std)
        f.lst = gsub('.apl', '.lst', f.apl)
        lf.lst = paste0(dir, f.lst)
        cmd = paste0('head -3 ',lf.apl,' > ',lf.lst)
        system(cmd)
        for (i in 1:n.std) {
            id.1 = std[i, 1]
            idx = tfr[,1] == id.1
            if (sum(idx) == 1) {
                line.num = tfr[idx, 5]
                tt = apl[line.num, ]
                ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f',
                    tt[1,1], tt[1,2], tt[1,3], tt[1,4], tt[1,5])
                write(ts, lf.lst, append = T)
            }
        }
        f.fits = gsub('.apl', '.fits', f.apl)
        ret = dao.update.psf(dir, f.fits, daophot = daophot, f.mag = f.mag)
        cmd = paste0('rm -f ',lf.tfr)
        system(cmd)
        cmd = paste0('rm -f ', dir, 'tmp.mch')
        system(cmd)
    } else {
        cmd = paste0('rm -f ', dir, 'tmp.mch')
        system(cmd)
    }
}
