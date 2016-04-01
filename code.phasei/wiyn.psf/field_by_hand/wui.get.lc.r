field = 'wui'
sfields = c(0:9, letters[1:21])
fields = paste0('w', sfields, 'i')

## cat(' Please input the field (i.e. x1i): ')
## f.ask = file('stdin')
## field = readLines(f.ask,1)
## close(f.ask)

## for (field in fields) {
    print(field)

    if (sum(fields == field) != 1) stop(' No such field.')
    band = substr(field,3,3)
    sfield = substr(field,2,2)
sfield = 'b'

    dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/')
    dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
    code.alf2lc = paste0(dir.fun, 'fun.alf2lc.r')
    source(code.alf2lc)
    dir.info = '~/Work/m33_mira/phot/info_files/'
    f.inf = paste0('m0',sfield,band,'.inf')
    f.prs = paste0('m0',sfield,band,'.prs')
    f.clb = paste0('m0',sfield,band,'.clb')
    lfo.inf = paste0(dir.info, f.inf)
    lfn.inf = paste0(dir, field, '.inf')
    lfo.prs = paste0(dir.info, f.prs)
    lfn.prs = paste0(dir, field, '.prs')
    fs.alf = list.files(dir, pattern = '.*.alf$')
    con = file(lfo.inf, 'r')
    inf = readLines(con)
    close(con)
    inf.roots = substr(inf, 1, 8)
    inf.roots = gsub(' ','', inf.roots)
    img.roots = gsub('.alf', '', fs.alf)
    idx = inf.roots %in% img.roots
    inf = inf[idx]
    inf = gsub(paste0('m0',sfield,'i'), paste0(' ', field), inf)

    write(inf, lfn.inf)
    
    ## con = file(lfo.prs, 'r')
    ## prs = readLines(con)
    ## close(con)
    ## idx = ((1:length(prs)) %% 3) == 1
    ## prs.roots = prs[idx]
    ## prs.roots = gsub(' ','', prs.roots)
    ## idx = prs.roots %in% img.roots
    ## idces = which(idx)
    cmd = paste0('rm -f ',lfn.prs)
    system(cmd)
    ## for (idx in idces) {
    ##     ts = prs[idx * 3 - 2]
    ##     write(ts, lfn.prs, append = T)
    ##     ts = prs[idx * 3 - 1]
    ##     write(ts, lfn.prs, append = T)
    ##     ts = prs[idx * 3 - 0]
    ##     write(ts, lfn.prs, append = T)
    ## }
    
    for (f.alf in fs.alf) {
        prs.root = gsub('.alf','',f.alf)
        write(paste0(' ',prs.root), lfn.prs, append = T)
        write('', lfn.prs, append = T)
        write(' 0.5', lfn.prs, append = T)
    }

    lfo.clb = paste0(dir.info, f.clb)
    lfn.clb = paste0(dir, field, '.clb')
    cmd = paste0('cp ',lfo.clb, ' ', lfn.clb)
    system(cmd)

    f.fits = paste0(field, '.fits')
    lf.fits = paste0(dir, f.fits)
    imhead = '~/Programs/wcstools-3.9.2/bin/imhead'
    cmd = paste0(imhead, ' ',lf.fits, ' | grep EXPTIME > foo.tmp')
    system(cmd)
    foo = read.table('foo.tmp')
    exptime = foo[1,3]
    system('rm -f foo.tmp')
    ## make .lib & .fet file
    f.lib = paste0(field,'.lib')
    f.fet = paste0(field,'.fet')
    lf.lib = paste0(dir, f.lib)
    lf.fet = paste0(dir, f.fet)

    f.nmg = paste0(field,'.nmg')
    lf.nmg = paste0(dir, f.nmg)
    f.std = paste0(field,'.als.std')
    lf.std = paste0(dir, f.std)

    std = read.table(lf.std)
    nmg = read.table(lf.nmg, skip = 3)

    write(' 1 MAGNITUDE:            V', lf.lib)
    cmd = paste0('head -3 ', lf.nmg, ' > ', lf.fet)
    system(cmd)

    n.std = nrow(std)
    for (i.std in 1:n.std) {
        id = std[i.std, 1]
        idx = nmg[,1] == id
        if (sum(idx) == 1) {
            if (nmg[idx,6] < 50) {
                sid = as.character(id)
                while (nchar(sid) < 7) sid = paste0('0',sid)
                lid = paste0(field,'-',sid)
                llid = paste0('0',lid)
                ts = sprintf('%12s%9.3f%8.3f%7.4f%4i%5i',
                    lid, nmg[idx,5], nmg[idx,4]+2.5*log10(exptime),
                    nmg[idx,5] / sqrt(nmg[idx,7]), nmg[idx,7], nmg[idx,7])
                write(ts, lf.lib, append = T)
                write(llid, lf.fet, append = T)
                tt = nmg[idx,]
                ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9s%9s%9.3f%9.3f',
                    id, tt[1,2], tt[1,3], tt[1,4], tt[1,5],
                    paste0(tt[1,6],'.'), paste0(tt[1,7],'.'),
                    tt[1,8], tt[1,9])
                write(ts, lf.fet, append = T)
            }
        }
    }

    trialp = '~/Programs/dao_e2/trialp'
    root = field
    ret = dao.alf2lc(root, dir, trialp = trialp)


## }
