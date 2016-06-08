f.co = '~/Work/m33_phaseII/mira_cata_sampsize/cadco.csv'
f.dat = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
mapdir = '~/Work/m33_mira/cmb_lcs/m33_lcs/'
outdir = '~/Work/m33_phaseII/mira_cata_sampsize/'

fields = c(0:9,letters[1:21])
fields = paste0('w',fields,'i')
fields = c(fields, 'x1i', 'x2i', 'x3i', 'x4i',
    'y1i', 'y2i', 'y3i', 'y4i',
    'zai', 'zbi', 'zci')

if (T) {
    co = read.csv(f.co)
    dat = read.table(f.dat)
    co[,1] = as.character(co[,1])
    idx = order(co[,1])
    co = co[idx,]
    dat[,1] = as.character(dat[,1])
    dat[,1] = paste0(dat[,1], '.slc')
    idx = match(co[,1], dat[,1])
    dat = dat[idx,]
}

n.co = nrow(co)
old.field = substr(co[1,1],1,3)
f.lst = paste0(outdir, 'mapids4snap.dat')
write('#   sid    ra    dec    m1    m2    m3   m4', f.lst)
for (i in 1:n.co) {
    id = co[i,1]
    sid = gsub('.slc','',id)
    ra = dat[i,2]
    dec = dat[i,3]
    field = substr(id,1,3)
    if (field != old.field | !exists('map')) {
        f.map = paste0(mapdir, field,'_uid_map.dat')
        map = read.table(f.map, sep=',', colClasses='character')
        old.field = field
    }
    col.idx = which(fields == field)
    idx = map[,col.idx] == sid
    if (sum(idx) != 1) stop(sid)
    tt = map[idx,]
    tt = tt[!is.na(tt)]
    tt = tt[tt != sid]
    while (length(tt) < 4) tt = c(tt, NA)
    ts = sprintf('%13s%14.5f%11.5f%13s%13s%13s%13s',
        sid, ra, dec, tt[1], tt[2], tt[3], tt[4])
    write(ts, f.lst, append=T)
}
