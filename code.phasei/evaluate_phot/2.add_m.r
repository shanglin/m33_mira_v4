f.dat = '~/Work/m33_mira/evaluate_phot/f_id_err.dat'
dat = read.table(f.dat)
n = nrow(dat)

dat = cbind(dat,dat[,3],dat[,2:3])
dat[,5] = NA
dat[,6] = NA
dir = '~/Work/m33_mira/cmb_lcs/light_curves/tmp_lcs/'

f.rd = '~/Work/m33_mira/cmb_lcs/light_curves/uid_rd.dat'
rd = read.table(f.rd)

for (i in 1:n) {
    print(paste(i,'/',n))
    id = paste0(dat[i,1],dat[i,2])
    f = paste0(id,'.ulc')
    lf = paste0(dir,f)
    lc = read.table(lf)
    ms = lc[,2]
    for (ifoo in 1:5) {
        idx = abs(ms - mean(ms)) < 3*sd(ms)
        ms = ms[idx]
    }
    m = mean(ms)
    dat[i,3] = m
    idx = rd[,1] == id
    if (sum(idx) == 1) {
        dat[i,5] = rd[idx,2]
        dat[i,6] = rd[idx,3]
    }
    ## plot(lc[,1:2], ylim=c(m+0.5,m-0.5))
    ## abline(h=m)
    ## a = readline()
}
idx = !is.na(dat[,6])
dat = dat[idx,]

fmt = '%5s%10i%9.3f%9.4f%12.5f%12.5f'
out = do.call('sprintf',c(dat, fmt))
f.out = '~/Work/m33_mira/evaluate_phot/f_id_m_e_rd.dat'
write('#  field   ID       mag     err        ra         dec', f.out)
write(out, f.out, append=T)

