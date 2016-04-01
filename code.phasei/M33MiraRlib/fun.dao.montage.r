dao.montage = function(dir, f.mch, min.frame = 3, montage = montage) {
    lf.in = paste0(dir, f.mch, '_mtg.in')
    lf.out = paste0(dir, f.mch, '_mtg.out')
    write(f.mch, lf.in)
    write('', lf.in, append = T)
    ts = paste0(min.frame,',0.5')
    write(ts, lf.in, append = T)
    for (i in 1:2) write('100,200', lf.in, append = T)
    write('1', lf.in, append = T)
    write('y', lf.in, append = T)
    write('', lf.in, append = T)

    lf.fits = paste0(dir, gsub('.mch', '.fits', f.mch))
    cmd = paste0('rm -f ',lf.fits)
    system(cmd)
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(montage,' < ',lf.in,' > ',lf.out)
    system(cmd)
    cmd = paste0('grep \'<<\' ' ,lf.out, ' > tmp.out')
    system(cmd)
    tmp = read.table('tmp.out')
    cmd = 'rm -f tmp.out'
    system(cmd)
    setwd(dir.current)

    idx = tmp[,1] == min.frame
    x1 = tmp[idx,2]
    x2 = tmp[idx,3]
    y1 = tmp[idx,4]
    y2 = tmp[idx,5]
    write(f.mch, lf.in)
    write('', lf.in, append = T)
    ts = paste0(min.frame,',0.5')
    write(ts, lf.in, append = T)
    ts = paste(x1, x2, sep = ',')
    write(ts, lf.in, append = T)
    ts = paste(y1, y2, sep = ',')
    write(ts, lf.in, append = T)
    write('1', lf.in, append = T)
    write('y', lf.in, append = T)
    write('', lf.in, append = T)
    
    cmd = paste0('rm -f ',lf.fits)
    system(cmd)
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(montage,' < ',lf.in,' > ',lf.out)
    system(cmd)
    setwd(dir.current)
}
