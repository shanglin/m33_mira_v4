options(stringsAsFactors=F)

dir = '~/Work/m33_mira/evaluate_phot/'
f.dat = paste0(dir, 'iden_uid.dat')
dat = read.table(f.dat)
ndat = nrow(dat)

outdir = '~/Work/m33_mira/evaluate_phot/'
f.out = paste0(outdir, 'iden_merd.dat')
ts = '#  M33SSSid     sid     mag      lcerr      err      ra       dec      x       y'
write(ts, f.out)

pdir = '~/Work/m33_mira/phot/i/'
lcdir = '~/Work/m33_mira/cmb_lcs/light_curves/tmp_lcs/'

f.rd = '~/Work/m33_mira/cmb_lcs/light_curves/uid_rd.dat'
rd = read.table(f.rd)

for (i in 1:ndat) {
    sid = dat[i,2]
    field = substr(sid, 1, 3)
    nid = gsub(field,'',sid)
    nid = as.numeric(nid)
    
    sdir = paste0(pdir, field, '_allframe/')
    f.fnl = paste0(sdir, 'trialp_all_',field,'/',field,'.fnl')
    fnl = read.table(f.fnl, skip=1)
    idx = fnl[,1] == nid
    if (sum(idx) != 1) stop(sid)
    e = fnl[idx, 5]
    x = fnl[idx, 2]
    y = fnl[idx, 3]

    m = NA
    me = NA
    f.lc = paste0(sid,'.ulc')
    lf.lc = paste0(lcdir,f.lc)
    if (file.exists(lf.lc)) {
        lc = read.table(lf.lc)
        ms = lc[,2]
        for (ifoo in 1:5) {
            idx = abs(ms - mean(ms)) < 3*sd(ms)
            ms = ms[idx]
        }
        m = mean(ms)
        dof = max(1, length(ms)-1)
        me = sd(ms) / sqrt(dof)
    }

    r = NA
    d = NA
    idx = rd[,1] == sid
    if (sum(idx) == 1) {
        r = rd[idx,2]
        d = rd[idx,3]
    }
    
    
    ts = sprintf('%12s%12s%9.3f%9.4f%9.4f%12.5f%12.5f%10.3f%10.3f',
        dat[i,1], sid, m, me, e, r, d, x, y)
    print(paste(i, ndat))

    write(ts, f.out, append=T)
}

          
