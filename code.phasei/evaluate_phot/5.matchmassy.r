dir = '~/Work/m33_mira/evaluate_phot/'
f.dat = paste0(dir, 'iden_merd.dat')

f.mas = '~/Work/m33_mira/astrometry/massey/massey_i.dat'
options(stringsAsFactors=F)
dat = read.table(f.dat)
idx = !is.na(dat[,5])
dat = dat[idx,]
idx = !is.na(dat[,3])
dat = dat[idx,]

mas = read.table(f.mas, skip=1)

new = cbind(dat, dat[,1], dat[,3])
new[,10] = NA
new[,11] = NA
new[,2] = substr(new[,2],1,3)

ndat = nrow(dat)

dist = 0.5 / 3600
distsqr = dist^2

for (i in 1:ndat) {
    ra = dat[i,6]
    dec = dat[i,7]

    idx = (mas[,2] - ra)^2 + (mas[,3] - dec)^2 < distsqr
    if (sum(idx) == 1) {
        lid = mas[idx,1]
        lm = mas[idx, 4]
        new[i,10] = lid
        new[i,11] = lm
    }
}

fmt = '%12s%6s%9.3f%9.4f%9.4f%12.5f%12.5f%10.3f%10.3f%21s%8.3f'
out = do.call('sprintf',c(new, fmt))
f.out = paste0(dir, 'id_merd_mas.dat')
ts = '#  M33SSSid   field   mag      lcerr       err        ra         dec        x         y         LGGS_id          LGGS_I'
write(ts, f.out)
write(out, f.out, append=T)
