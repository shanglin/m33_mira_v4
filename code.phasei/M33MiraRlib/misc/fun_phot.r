
## The function below performs DAO FIND AND DAO APERTRUE PHOT
dao.find.stars = function(dir, f.fits,
    n.frame.average = 1, n.frame.sum = 1,
    daophot='daophot') {

    ## (1) Check needed files
    lf.fits = paste0(dir,f.fits)
    if (!file.exists(lf.fits)) stop(paste0(lf.fits,' does not exist'))
    lf.daophot.opt = paste0(dir,'daophot.opt')
    if (!file.exists(lf.daophot.opt)) stop(paste0(lf.daophot.opt,' does not exist'))
    lf.photo.opt = paste0(dir,'photo.opt')
    if (!file.exists(lf.photo.opt)) stop(paste0(lf.photo.opt,' does not exist'))
    ## (2) find stars
    root = gsub('.fits','',f.fits)
    f.in = paste0('do_daofind_',root,'.in')
    f.out = paste0('do_daofind_',root,'.out')
    lf.in = paste0(dir,f.in)
    lf.out = paste0(dir,f.out)
    ts = paste0('attach ',f.fits)
    write(ts,lf.in)
    write('find',lf.in,append=T)
    ts = paste(n.frame.average,n.frame.sum,sep=',')
    write(ts,lf.in,append=T)
    write('',lf.in,append=T)
    write('y',lf.in,append=T)
    write('phot',lf.in,append=T)
    for (ifoo in 1:4) write('',lf.in,append=T)
    write('exit',lf.in,append=T)

    cmd = paste0('rm -f ',dir,root,'.coo')
    system(cmd)
    cmd = paste0('rm -f ',dir,root,'.ap')
    system(cmd)
    cmd = paste0('rm -f ',dir,root,'.psf')
    system(cmd)
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(daophot,' < ',lf.in,' > ',lf.out)
    system(cmd)
    setwd(dir.current)

    dir.log = paste0(dir,'daofind_Logs/')
    cmd = paste0('mkdir -p ',dir.log)
    system(cmd)
    cmd = paste0('mv ',lf.in,' ',dir.log)
    system(cmd)
    cmd = paste0('mv ',lf.out,' ',dir.log)
    system(cmd)
    cmd = paste0('rm -f ',dir,root,'.coo')
    system(cmd)
    return(1)
}

## The function below performs DAO ALLSTAR
dao.allstar = function(dir, f.fits,
    psf = 'default', allstar = 'allstar') {

    ## (1) Check needed files
    root = gsub('.fits','',f.fits)
    lf.fits = paste0(dir,f.fits)
    if (!file.exists(lf.fits)) stop(paste0(lf.fits,' does not exist'))
    lf.allstar.opt = paste0(dir,'allstar.opt')
    if (!file.exists(lf.allstar.opt)) stop(paste0(lf.allstar.opt,' does not exist'))
    if (psf == 'default') psf = paste0(root,'.psf')
    lf.psf = paste0(dir,psf)
    if (!file.exists(lf.psf)) stop(paste0(lf.psf,' does not exist'))
    lf.ap = paste0(dir,root,'.ap')
    if (!file.exists(lf.ap)) stop(paste0(lf.ap,' does not exist'))

    ## (2) Perform allstar
    f.in = paste0('do_allstar_',root,'.in')
    f.out = paste0('do_allstar_',root,'.out')
    lf.in = paste0(dir,f.in)
    lf.out = paste0(dir,f.out)
    write('',lf.in)
    write(f.fits, lf.in, append = T)
    write(psf, lf.in, append = T)
    for (ifoo in 1:3) write('',lf.in,append=T)

    cmd = paste0('rm -f ',dir,root,'.als')
    system(cmd)
    lf.subtract = paste0(dir,root,'s.fits')
    cmd = paste0('rm -f ',lf.subtract)
    system(cmd)
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(allstar,' < ',lf.in,' > ',lf.out)
    system(cmd)
    setwd(dir.current)
    
    dir.log = paste0(dir,'allstar_Logs/')
    cmd = paste0('mkdir -p ',dir.log)
    system(cmd)
    cmd = paste0('mv ',lf.in,' ',dir.log)
    system(cmd)
    cmd = paste0('mv ',lf.out,' ',dir.log)
    system(cmd)
    cmd = paste0('mv ',lf.subtract,' ',dir.log)
    system(cmd)
    return(1)
}

## Obtain PSF blindly
dao.get.psf = function(dir,f.fits,
    n.star = 30, mag.lim = 19,
    daophot = daophot) {

    ## (1) Check needed files
    lf.fits = paste0(dir,f.fits)
    if (!file.exists(lf.fits)) stop(paste0(lf.fits,' does not exist'))
    lf.daophot.opt = paste0(dir,'daophot.opt')
    if (!file.exists(lf.daophot.opt)) stop(paste0(lf.daophot.opt,' does not exist'))

    ## (2) get PSF
    root = gsub('.fits','',f.fits)
    f.in = paste0('do_getpsf_',root,'.in')
    f.out = paste0('do_getpsf_',root,'.out')
    lf.in = paste0(dir,f.in)
    lf.out = paste0(dir,f.out)
    ts = paste0('attach ',f.fits)
    write(ts,lf.in)
    write('pick',lf.in,append=T)
    write('',lf.in,append=T)
    ts = paste(n.star,mag.lim,sep=',')
    write(ts,lf.in,append=T)
    write('',lf.in,append=T)
    write('psf',lf.in,append=T)
    for (ifoo in 1:3) write('',lf.in,append=T)
    write('exit',lf.in,append=T)

    cmd = paste0('rm -f ',dir,root,'.psf')
    system(cmd)
    cmd = paste0('rm -f ',dir,root,'.lst')
    system(cmd)
    cmd = paste0('rm -f ',dir,root,'.nei')
    system(cmd)
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(daophot,' < ',lf.in,' > ',lf.out)
    system(cmd)
    setwd(dir.current)

    dir.log = paste0(dir,'getpsf_Logs/')
    cmd = paste0('mkdir -p ',dir.log)
    system(cmd)
    cmd = paste0('mv ',lf.in,' ',dir.log)
    system(cmd)
    cmd = paste0('mv ',lf.out,' ',dir.log)
    system(cmd)
    cmd = paste0('rm -f ',dir,root,'.coo')
    system(cmd)
    return(1)
}


## dir = '~/Work/m33_mira/phot/i/x1i_new/'
## f.fits = 'ii6214.fits'
## f.fits = 'x1i.fits'
## daophot = '~/Programs/dao_e2/daophot'
## allstar = '~/Programs/dao_e2/allstar'
## n.star = 40
## mag.lim = 19
## ret = dao.find.stars(dir,f.fits,daophot=daophot)
## ret = dao.get.psf(dir,f.fits,daophot=daophot,n.star=n.star,mag.lim=mag.lim)
## ret = dao.allstar(dir,f.fits,allstar=allstar)

## fs.fits = list.files(dir,pattern='ii.....fits$')
## for (f.fits in fs.fits) {
##     print(f.fits)
##     ret = dao.find.stars(dir,f.fits,daophot=daophot)
##     ret = dao.get.psf(dir,f.fits,daophot=daophot,n.star=n.star,mag.lim=mag.lim)
##     ret = dao.allstar(dir,f.fits,allstar=allstar)
## }
