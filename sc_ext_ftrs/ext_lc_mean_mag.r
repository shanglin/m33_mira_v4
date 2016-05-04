set.seed(101)

lc.dir = '~/Work/m33_phaseII/m33_v4_lcs/'
out.dir = '~/Work/m33_phaseII/sc_ext_ftrs/'
fs = list.files(lc.dir)
nfs = length(fs)

f.out = paste0(out.dir, 'lc_mean_mag.dat')
write('#                        id           mean_mag', f.out)

for (i in 1:nfs) {

    msg = paste0('   >> ',round(100*i/nfs, 2), ' %   \r')
    message(msg, appendLF=F)

    f.lc = fs[i]
    lf.lc = paste0(lc.dir, f.lc)
    lc = read.table(lf.lc)
    y = lc[,2]
    mm = mean(y)
    sd = sd(y)
    idx = abs(y - mm) < 3*sd
    y = y[idx]
    mmag = mean(y)
    
    ## if (sum(idx) < nrow(lc)) {
    ##     plot(lc[,1:2])
    ##     points(lc[idx,1:2],pch=19)
    ##     Sys.sleep(2)
    ## }

    ts = sprintf('%35s%12.3f',f.lc, mmag)
    write(ts, f.out, append=T)
}
