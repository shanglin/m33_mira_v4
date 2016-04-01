dao.match.fast = function(dir, fs, f.mch, daomatch = 'daomatch', clean = F) {
    lf.mch = paste0(dir,f.mch)
    cmd = paste0('rm -f ', lf.mch)
    system(cmd)
    lf.in = paste0(dir, f.mch, '.in')
    lf.out = paste0(dir, f.mch, '.out')
    if (length(fs) > 1) {
        write(fs[1], lf.in)
        write(f.mch, lf.in, append = T)
        for (i in 2:length(fs)) {
            write(fs[i], lf.in, append = T)
        }
        write('', lf.in, append = T)
        write('', lf.in, append = T)
        write('n', lf.in, append = T)
        write('', lf.in, append = T)
        dir.current = getwd()
        setwd(dir)
        cmd = paste0(daomatch,' < ',lf.in,' > ',lf.out)
        system(cmd)
        setwd(dir.current)
        if (clean == T) {
            system(paste0('rm -f ', lf.in))
            system(paste0('rm -f ', lf.out))
        }
    }
}

dao.master = function(dir, f.mch, daomaster = 'daomaster', f.tfr = 'no', clean = F) {
    lf.in = paste0(dir, f.mch, '_mst.in')
    lf.out = paste0(dir, f.mch, '_mst.out')
    lf.mch = paste0(dir, f.mch)
    if (f.tfr != 'no') {
        lf.tfr = paste0(dir, f.tfr)
        cmd = paste0('rm -f ',lf.tfr)
        system(cmd)
    }
    mch = read.table(lf.mch)
    n = nrow(mch)
    write(f.mch, lf.in)
    ts = paste(round(0.8*n), 0.8, round(0.8*n), sep = ',')
    write(ts, lf.in, append = T)
    write('0.25', lf.in, append = T)
    write('20', lf.in, append = T)
    write('-3', lf.in, append = T)
    for (i in 1:7) write('3', lf.in, append = T)
    for (i in 1:7) write('2', lf.in, append = T)
    for (i in 1:7) write('1', lf.in, append = T)
    write('0', lf.in, append = T)
    for (i in 1:4) write('n', lf.in, append = T)
    write('y', lf.in, append = T)
    write(f.mch, lf.in, append = T)
    write('', lf.in, append = T)
    if (f.tfr != 'no') {
        write('y', lf.in, append = T)
        write(f.tfr, lf.in, append = T)
    }
    write('e', lf.in, append = T)
    
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(daomaster,' < ',lf.in,' > ',lf.out)
    system(cmd)
    setwd(dir.current)
    if (clean == T) {
        system(paste0('rm -f ', lf.in))
        system(paste0('rm -f ', lf.out))
    }
}    



dao.master.mtr = function(dir, f.mch, daomaster = 'daomaster', f.tfr = 'default', clean = F) {
    lf.in = paste0(dir, f.mch, '_mst.in')
    lf.out = paste0(dir, f.mch, '_mst.out')
    lf.mch = paste0(dir, f.mch)
    if (f.tfr == 'default') {
        f.tfr = gsub('.mch', '.tfr', f.mch)
    }
    lf.tfr = paste0(dir, f.tfr)
    cmd = paste0('rm -f ',lf.tfr)
    system(cmd)
    mch = read.table(lf.mch)
    n = nrow(mch)
    write(f.mch, lf.in)
    if (n > 2) {
        ts = paste(round(0.8*n), 0.8, round(0.8*n), sep = ',')
    } else if (n == 2) {
        ts = '2,1,2'
    } else {
        stop(' [DAOMASTER] Invalid number of lines in the .mch file')
    }
    write(ts, lf.in, append = T)
    write('0.25', lf.in, append = T)
    write('20', lf.in, append = T)
    write('-3', lf.in, append = T)
    for (i in 1:7) write('3', lf.in, append = T)
    for (i in 1:7) write('2', lf.in, append = T)
    for (i in 1:7) write('1', lf.in, append = T)
    write('0', lf.in, append = T)
    for (i in 1:4) write('n', lf.in, append = T)
    write('y', lf.in, append = T)
    write(f.mch, lf.in, append = T)
    write('', lf.in, append = T)
    write('y', lf.in, append = T)
    write(f.tfr, lf.in, append = T)
    write('n', lf.in, append = T)
    write('y', lf.in, append = T)

    ## in case of not working properly
    write('', lf.in, append = T)
    write('', lf.in, append = T)
    write('e', lf.in, append = T)
    write('n', lf.in, append = T)
    write('exit', lf.in, append = T)
    write('exit', lf.in, append = T)
    write('', lf.in, append = T)
    write('', lf.in, append = T)
    write('e', lf.in, append = T)
    write('n', lf.in, append = T)
    write('exit', lf.in, append = T)
    write('exit', lf.in, append = T)
    
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(daomaster,' < ',lf.in,' > ',lf.out)
    system(cmd)
    setwd(dir.current)
    if (clean == T) {
        system(paste0('rm -f ', lf.in))
        system(paste0('rm -f ', lf.out))
    }
}    
