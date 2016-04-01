field = 'x1i'
dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/')
lc.dir = paste0(dir, 'trialp_all_',field,'/')
f.fnl = paste0(field,'.fnl')
lf.fnl = paste0(lc.dir, f.fnl)
fnl = read.table(lf.fnl, skip=1)
nfnl = nrow(fnl)


out.dir = '~/Work/m33_mira/meeting/mar24_2016/lcsd/'
f.lcsd = paste0(out.dir,field,'_lcsd.dat')
ts = '#   id    m    e    lc_sd  n'
write(ts, f.lcsd)
for (i in 1:nfnl) {
    msg = paste0(' >> ',round(100*i/nfnl,2),' %   \r')
    message(msg, appendLF=F)
    id = fnl[i,1]
    m = fnl[i,4]
    e = fnl[i,5]
    f.lc = paste0(lc.dir, 'lcV.', id)
    lc = read.table(f.lc)
    n = nrow(lc)
    if (n > 2) {
        sd = sd(lc[,2])
        ts = paste(id, m, e, round(sd,4), n, sep = '   ')
        write(ts, f.lcsd, append = T)
    }
}
print('')

dat = read.table(f.lcsd)
f.png = paste0(out.dir,field,'_lcsd.png')
png(f.png)
plot(dat[,2], dat[,4], pch=19, cex=0.1, col=rgb(0,0,0,0.5), xlab='Instrumental I (mag)', ylab='Standard Deviation of I in each light curve (mag)', ylim=c(0,1))
dev.off()

f.png = paste0(out.dir, field, '_lcsd_sigma.png')
png(f.png)
plot(dat[,3]*sqrt(dat[,5]-1), dat[,4], pch=19, cex=0.1, col=rgb(0,0,0,0.5), xlab='sigma * sqrt(n-1) (mag)', ylab='Standard Deviation of I in each light curve (mag)', ylim=c(0,1), xlim=c(0,1))
lines(c(0,10),c(0,10),col=2,lty=2)
dev.off()
