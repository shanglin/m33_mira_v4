outdir = '~/Work/m33_phaseII/draft/v1/tables/'
f.dat = paste0(outdir, 'foo.dat')
dat = read.table(f.dat, stringsAsFactors=F)
idx = order(dat[,2])
dat = dat[idx,]
n = 20

f.tex = paste0(outdir, 'pars.tex')
system(paste0('rm -f ',f.tex))
for (i in 1:(n-1)) {
    tt = dat[i,]
    ts = sprintf('%10s%3s%12.5f%3s%10.5f%3s%9.3f%3s%10.2f%3s%9.3f%3s%9.3f%3s%10s%3s%4i%5s',
        tt[1,1],'&',tt[1,2],'&',tt[1,3],'&',tt[1,4],'&',tt[1,5],'&',tt[1,6],'&',tt[1,7],'&',tt[1,8],'&',tt[1,9],'\\\\')
    write(ts, f.tex, append=T)
}
tt = dat[n,]
ts = sprintf('%10s%3s%12.5f%3s%10.5f%3s%9.3f%3s%10.2f%3s%9.3f%3s%9.3f%3s%10s%3s%4i',
        tt[1,1],'&',tt[1,2],'&',tt[1,3],'&',tt[1,4],'&',tt[1,5],'&',tt[1,6],'&',tt[1,7],'&',tt[1,8],'&',tt[1,9])
write(ts, f.tex, append=T)
