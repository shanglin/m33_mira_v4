dir = '~/Work/m33_mira/phot/i/'
sdirs = list.files(dir)
fields = substr(sdirs, 1, 3)
nfields = length(fields)

options(stringsAsFactors=F)

outdir = '~/Work/m33_mira/evaluate_phot/'
f.out = paste0(outdir, 'f_id_err.dat')
ts = '#  field   ID     err'
write(ts, f.out)
for (ifield in 1:nfields) {
    field = fields[ifield]
    sdir = paste0(dir, field, '_allframe/')
    f.lib = paste0(sdir, field, '.lib')
    f.fnl = paste0(sdir, 'trialp_all_',field,'/',field,'.fnl')
    lib = read.table(f.lib, skip=1)
    ids = lib[,1]
    ids = as.numeric(substr(ids,5,12))
    fnl = read.table(f.fnl, skip=1)
    
    idx = match(ids, fnl[,1])
    sub = fnl[idx,]
    idx = !is.na(sub[,1])
    ids = ids[idx]
    sub = sub[idx,]

    out = as.data.frame(cbind(sub[,1],sub[,1],sub[,5]))
    out[,1] = field

    fmt = '%5s%10i%9.4f'
    outs = do.call('sprintf',c(out, fmt))
    write(outs, f.out, append=T)
}
