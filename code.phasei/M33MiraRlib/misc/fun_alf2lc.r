
dao.alf2lc = function(root, dir, trialp.type = 'Yosemite',
    lc.type = 'all', multiplier = 1, trialp = 'trialp') {

    ## (1) Check needed files, prepare .frc file
    req.files = c('inf','prs','lib','fet','clb','tfr','nmg')
    for (req.file in req.files) {
        if (!file.exists(paste0(dir,root,'.',req.file))) {
            stop(paste0(req.file,' file not found!'))
        }
        assign(paste0('f.',req.file),paste0(root,'.',req.file))
        assign(paste0('lf.',req.file),paste0(dir,root,'.',req.file))
    }
    f.frc = paste0(root,'.frc')
    lf.frc = paste0(dir,f.frc)
    cmd = paste0('cp ',lf.nmg,' ',lf.frc)
    system(cmd)

    ## (2) Prepare space for output light curves; clean old files
    dir.out = paste0(dir,'trialp_',lc.type,'_',root,'/')
    cmd = paste0('rm -rf ',dir.out)
    system(cmd)
    cmd = paste0('mkdir -p ',dir.out)
    system(cmd)
    out.files = c('per','fnl','vry','zer')
    for (out.file in out.files) {
        assign(paste0('f.',out.file),paste0(root,'.',out.file))
        assign(paste0('lf.',out.file),paste0(dir,root,'.',out.file))
        cmd = paste0('rm -f ',paste0('lf.',out.file))
        system(cmd)
    }
    fs.lc = list.files(dir,pattern='^lcV.*')
    if (length(fs.lc) > 0) {
        stop('Please remove the lcV.* files before start')
    }

    ## (3) Run trialp with the .frc file implicitly
    f.in = paste0('do_trialp_',lc.type,'.in')
    lf.in = paste0(dir, f.in)
    f.out = paste0('do_trialp_',lc.type,'.out')
    lf.out = paste0(dir, f.out)
    write(f.tfr, lf.in)
    for (ifoo in 1:4) write('', lf.in, append=T)
    write(paste0(multiplier), lf.in, append=T)
    write('1e19,5,30,-99', lf.in, append=T)
    write('', lf.in, append=T)
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(trialp,' < ',lf.in,' > ',lf.out)
    system(cmd)
    setwd(dir.current)

    ## (4) Move the output files into the output directory
    cmd = paste0('mv ',lf.fnl,' ',lf.vry,' ',lf.per,' ',lf.zer,' ',dir.out)
    system(cmd)
    for (istart in 1000:9999) {
        cmd = paste0('mv ',dir,'lcV.',istart,'* ',dir.out)
        system(cmd)
    }
    cmd = paste0('mv ',dir,'lcV.* ',dir.out)
    system(cmd)
    ## (5) Actually nothing to return for now
    return(1)
}

## trialp = '~/Programs/dao/trialp'
## dir = '~/Work/m33_mira/phot/i/x1i/'
## root = 'x1i'
## ret = dao.alf2lc(root=root,dir=dir,trialp=trialp)
