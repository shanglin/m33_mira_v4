

field = 'whi'
sfields = c(0:9, letters[1:19])
## sfields = letters[3:19]
fields = paste0('w', sfields, 'i')
## cat(' Please input the field (i.e. x1i): ')
## f.ask = file('stdin')
## field = readLines(f.ask,1)
## close(f.ask)

dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
code.dao.opt = paste0(dir.fun, 'fun.dao.wrt.opt.r')
code.phot = paste0(dir.fun, 'fun.dao.phot.r')
code.get.apl = paste0(dir.fun, 'fun.ap2apl.r')
code.wrt.reg = paste0(dir.fun, 'fun.wrt.ds9reg.r')
code.daomatch = paste0(dir.fun, 'fun.dao.match.r')
code.montage = paste0(dir.fun, 'fun.dao.montage.r')
code.std2psf = paste0(dir.fun, 'fun.std2psf.r')
code.clean.als = paste0(dir.fun, 'fun.clean.als.r')
source(code.dao.opt)
source(code.montage)
source(code.phot)
source(code.get.apl)
source(code.wrt.reg)
source(code.daomatch)
source(code.std2psf)
source(code.clean.als)

daophot = '~/Programs/dao_e2/daophot'
daomatch = '~/Programs/dao_e2/daomatch'
daomaster = '~/Programs/dao_e2/daomaster'
allstar = '~/Programs/dao_e2/allstar'
allframe = '~/Programs/dao_e2/allframe'
montage = '~/Programs/dao_e2/montage2'


## for (field in fields) {

    if (sum(fields == field) != 1) stop(' No such field.')
    band = substr(field,3,3)
    sfield = substr(field,2,2)

    dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/')
    if (!file.exists(dir)) {
        cmd = paste0('mkdir -p ',dir)
        system(cmd)
        raw.dir = paste0('~/Work/m33_mira/phot/rawdata/wiyn_',band,'/m0',sfield,'/')
        cmd = paste0('cp ',raw.dir,'*.fits ',dir)
        system(cmd)
    }


    fs.fits = list.files(dir, pattern = '........fits')
    fs.fits = fs.fits[fs.fits != 'montage.fits']
    nfs.fits = length(fs.fits)
    n.frames = nfs.fits

    ## (2) Roughly find the stars at first
    write.photo.opt(dir, c(7,9,11,13,15), is = 15, os = 20)
    write.daophot.opt(dir, th = 3, ga = 4, re = 10, hi = 20000, lo = 20, fw = 4, fi = 5)
    write.allstar.opt(dir, fi = 5)
    for (i in 1:nfs.fits) {
        f.fits = fs.fits[i]
        print(paste0(field,': Finding stars in ',f.fits,' ...'))
        lf.fits = paste0(dir, f.fits)
        dao.find.stars(dir, f.fits, daophot = daophot, n.frame.average = 1, n.frame.sum = 1)
        f.ap = gsub('.fits','.ap',f.fits)
        lf.ap = paste0(dir,f.ap)
        apl = ap2apl(lf.ap,1) ## aper = 7
        lf.apl = gsub('.ap','.apl',lf.ap)
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


    ## (3) Use the first frame to find some good stars in order to derive the first guess of PSF
    print(' First estimation of each PSF...')
    write.allstar.opt(dir, fi = 5)
    for (i.fits in 1:length(fs.fits)) {
        f.fits = fs.fits[i.fits]
        ret = dao.get.psf(dir, f.fits, n.star = 30, mag.lim = 19, daophot = daophot)
        f.psf = gsub('.fits', '.psf', f.fits)
        lf.psf = paste0(dir, f.psf)
        con = file(lf.psf, 'r')
        psf = readLines(con)
        close(con)
        if (length(psf) > 0) {
            ret = dao.allstar(dir, f.fits, allstar = allstar)
            f.als = gsub('.fits', '.als', f.fits)
            ret = clean.als(dir, f.als)
            f.ref.als = f.als
            lf.als = paste0(dir, f.als)
            als = read.table(lf.als, skip = 3)
            als.all = als
            x = als[,2]
            y = als[,3]
            edge.pix = 5
            idx = x > edge.pix & x < max(x) - edge.pix & y > edge.pix & y < max(y) - edge.pix
            als = als[idx,]
            n.als = nrow(als)
            mag.lim.bri = quantile(als[,4],0.002)
            mag.lim.fai = quantile(als[,4],0.05)
            err.lim = 0.02
            n.idx = 0
            while (n.idx < 200 & err.lim < 0.3) {
                idx = als[,4] > mag.lim.bri & als[,4] < mag.lim.fai & als[,5] < err.lim & als[,7] < 8 & als[,8] < 5
                err.lim = err.lim + 0.02
                n.idx = sum(idx)
            }
            if (n.idx < 5) stop(' Too few secondary standards were found.')
            std.condidates = als[idx,]
            n.std.cond = nrow(std.condidates)
            near.dist = rep(NA, n.std.cond)
            for (i.con in 1:n.std.cond) {
                als.all = als.all[-which(als.all[,1]==std.condidates[i.con, 1]),]
                near.dist[i.con] = min((als.all[,2] - std.condidates[i.con,2])^2 + (als.all[,3] - std.condidates[i.con,3])^2)
            }
            idx = rev(order(near.dist))
            std.condidates = std.condidates[idx,]
            n.std = min(50, n.std.cond)
            std = std.condidates[1:n.std,]
            lf.reg = paste0(dir,'test.reg')
            write.ds9reg.image(lf.reg,std[,2],std[,3],r=7,color='magenta')
            f.std = 'std.guess'
            lf.std = paste0(dir, f.std)
            cmd = paste0('rm -f ',lf.std)
            system(cmd)
            for (i.std in 1:n.std) {
                tt = std[i.std,]
                ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.3f%9s%9.3f%9.3f',
                    tt[1,1],tt[1,2],tt[1,3],tt[1,4],tt[1,5],tt[1,6],paste0(tt[1,7],'.'),tt[1,8],tt[1,9])
                write(ts, lf.std, append = T)
            }

            fs.apl = list.files(dir, pattern = '........apl$')
            nfs.apl = length(fs.apl)
            for (i.apl in 2:nfs.apl) {
                f.apl = fs.apl[i.apl]
                f.fits = gsub('.apl', '.fits', f.apl)
                f.ap = gsub('.apl', '.ap', f.apl)
                f.psf = gsub('.fits', '.psf', f.fits)
                lf.psf = paste0(dir, f.psf)
                std2psf(dir, f.ref.als, f.apl, f.std, f.mag = f.ap, daomatch = daomatch, daomaster = daomaster, daophot = daophot)
                if (!file.exists(lf.psf) | file.info(lf.psf)$size == 0) {
                    next
                }
            }
            break
        } else {
            next
        }
    }

    ## (4) Select sharpest images based on the PSF files
    fwhms = rep(NA, nfs.fits)
    sharp = cbind(fs.fits, fwhms)
    for (i.fits in 1:nfs.fits) {
        f.fits = fs.fits[i.fits]
        idx = sharp[,1] == f.fits
        if (sum(idx) == 1) {
            f.psf = gsub('.fits','.psf',f.fits)
            lf.psf = paste0(dir, f.psf)
            con = file(lf.psf, 'r')
            foo = readLines(con)
            fwhms = as.character(foo[2])
            close(con)
            x.see = as.numeric(substr(fwhms,1,14))
            y.see = as.numeric(substr(fwhms,15,27))
            sharp[i.fits,2] = sqrt(x.see * y.see)
        }
    }
    sharp = sharp[!is.na(sharp[,2]),]
    if (nrow(sharp) < 1) stop(' Sharp images not found.')
    idx = order(sharp[,2])
    sharp = sharp[idx,]


    ## (5) Find star list in master frame, select secondary standards
    f.master = paste0(field,'.fits')
    lf.master = paste0(dir, f.master)
    lf.as.master = paste0(dir, sharp[1,1])
lf.psf = gsub('.fits','.psf',lf.master)
lf.as.psf = gsub('.fits','.psf',lf.as.master)
cmd = paste0('cp ',lf.as.psf,' ',lf.psf)
system(cmd)
lf.ap = gsub('.fits','.ap',lf.master)
lf.as.ap = gsub('.fits','.ap',lf.as.master)
cmd = paste0('cp ',lf.as.ap,' ',lf.ap)
system(cmd)
    cmd = paste0('cp ',lf.as.master,' ',lf.master)
    system(cmd)
    print(' PSF photometry on the master frame...')
    ## dao.find.stars(dir, f.master, daophot = daophot, n.frame.average = 1, n.frame.sum = 1)
    ## ret = dao.get.psf(dir, f.master, n.star = 30, mag.lim = 19, daophot = daophot)
    ret = dao.allstar(dir, f.master, allstar = allstar)
    f.als = gsub('.fits', '.als', f.master)
    ret = clean.als(dir, f.als)
    lf.als = paste0(dir, f.als)
    als = read.table(lf.als, skip = 3)
    als.all = als
    lf.als.header = paste0(dir,'als.header')
    cmd = paste0('head -3 ', lf.als, ' > ', lf.als.header)
    system(cmd)
    lf.reg = gsub('.als', '.reg', lf.als)
    write.ds9reg.image(lf.reg, als[,2], als[,3], r = 3, color = 'green')

    print(' Selecting secondary standards...')
    mag.lim.bri = quantile(als[,4],0.002)
    mag.lim.fai = quantile(als[,4],0.05)
    err.lim = 0.02
    n.idx = 0
    x = als[,2]
    y = als[,3]
    edge.pix = 5
    while (n.idx < 200 & err.lim < 0.25) {
        idx = als[,4] > mag.lim.bri & als[,4] < mag.lim.fai & als[,5] < err.lim & als[,7] < 8 & als[,8] < 5 &
            x > edge.pix & x < max(x) - edge.pix & y > edge.pix & y < max(y) - edge.pix & als[,6] < 10000
        err.lim = err.lim + 0.02
        n.idx = sum(idx)
    }
    if (n.idx < 5) stop(' Too few secondary standards were found.')
    std.condidates = als[idx,]
    n.std.cond = nrow(std.condidates)
    near.dist = rep(NA, n.std.cond)
    for (i.con in 1:n.std.cond) {
        als.all = als.all[-which(als.all[,1]==std.condidates[i.con, 1]),]
        near.dist[i.con] = min((als.all[,2] - std.condidates[i.con,2])^2 + (als.all[,3] - std.condidates[i.con,3])^2)
    }
    idx = rev(order(near.dist))
    std.condidates = std.condidates[idx,]
    n.std = min(50, n.std.cond)
    std = std.condidates[1:n.std,]
    lf.reg = paste0(dir,'test.reg')
    write.ds9reg.image(lf.reg,std[,2],std[,3],r=7,color='magenta')
    f.std = paste0(f.als, '.std')
    lf.std = paste0(dir, f.std)
    cmd = paste0('rm -f ',lf.std)
    system(cmd)
    for (i.std in 1:n.std) {
        tt = std[i.std,]
        ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.3f%9s%9.3f%9.3f',
            tt[1,1],tt[1,2],tt[1,3],tt[1,4],tt[1,5],tt[1,6],paste0(tt[1,7],'.'),tt[1,8],tt[1,9])
        write(ts, lf.std, append = T)
    }
    ## Update the PSF for master frame.
    print(' Updating the PSF by fitting the secondary standards...')
    f.lst = gsub('.als', '.lst', f.als)
    lf.lst = paste0(dir, f.lst)
    cmd = paste0('head -3 ',lf.als.header,' > ',lf.lst)
    system(cmd)
    for (i.std in 1:n.std) {
        tt = std[i.std,]
        ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f',
            tt[1,1],tt[1,2],tt[1,3],tt[1,4],tt[1,5])
        write(ts, lf.lst, append = T)
    }
    ret = dao.update.psf(dir, f.master, daophot = daophot)

    ## Rerun ALLSTAR use updated PSF
    print(' PSF photometry for master frame using new PSF...')
    ret = dao.allstar(dir, f.master, allstar = allstar)
    f.als = gsub('.fits', '.als', f.master)
    ret = clean.als(dir, f.als)
    lf.als = paste0(dir, f.als)
    f.master.als = f.als
    lf.als.header = paste0(dir,'als.header')
    cmd = paste0('head -3 ', lf.als, ' > ', lf.als.header)
    system(cmd)
    lf.reg = gsub('.als', '.reg', lf.als)
    write.ds9reg.image(lf.reg, als[,2], als[,3], r = 3, color = 'green')


    ## Update the psf for each frame & Run ALLSTAR
    fs.apl = list.files(dir, pattern = '........apl$')
    nfs.apl = length(fs.apl)
    code.std2psf = paste0(dir.fun, 'fun.std2psf.r')
    source(code.std2psf)
    f.master.psf = gsub('.fits', '.psf', f.master)
    lf.master.psf = paste0(dir, f.master.psf)
    for (i.apl in 1:nfs.apl) {
        f.apl = fs.apl[i.apl]
        f.fits = gsub('.apl', '.fits', f.apl)
        f.ap = gsub('.apl', '.ap', f.apl)
        print(paste0(field,': PSF & ALLSTAR for ',f.fits,'...'))
        f.psf = gsub('.fits', '.psf', f.fits)
        lf.psf = paste0(dir, f.psf)
        if (f.fits == f.master) {
            cmd = paste0('cp ',lf.master.psf,' ',lf.psf)
            system(cmd)
        } else {
            std2psf(dir, f.master.als, f.apl, f.std, f.mag = f.ap, daomatch = daomatch, daomaster = daomaster, daophot = daophot)
        }
        if (!file.exists(lf.psf) | file.info(lf.psf)$size == 0) {
            cmd = paste0('cp ',lf.master.psf,' ',lf.psf)
            system(cmd)
        }
        ret = dao.allstar(dir, f.fits, allstar = allstar)
        f.als = gsub('.fits', '.als', f.fits)
        ret = clean.als(dir, f.als)
        lf.als = paste0(dir, f.als)
        als = read.table(lf.als, skip=3)
        lf.reg = gsub('.als','.reg',lf.als)
        write.ds9reg.image(lf.reg, als[,2], als[,3])
    }

    ## Match all the frames to the master frame use als files
    print(' Matching all the frames to the master frame...')
    fs.als = list.files(dir, pattern = '........als$')
    fs.all.frame = c(f.master.als, fs.als)
    f.mch = 'all_frame.mch'
    dao.match.fast(dir, fs.all.frame, f.mch, daomatch = daomatch)
    dao.master(dir, f.mch, daomaster = daomaster)
    write.allframe.opt(dir)
    ## lf.mch = paste0(dir, f.mch)
    ## mch = read.table(lf.mch)
    ## if (ncol(mch) != 23) stop(' Failed to create master frame.')
    ## stop('Please check the .MCH files and run the code below.')
    ## f.new = paste0(field, '.mch')
    ## lf.new = paste0(dir, f.new)
    ## cmd = paste0('awk \'NR>1\' ',lf.mch,' > ',lf.new)
    ## system(cmd)
    ## write.allframe.opt(dir)
    ## print('Running ALLFRAME...')
    ## ret = dao.allframe(dir, f.new, allframe = allframe) 
## }
