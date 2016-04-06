dir = '~/Work/m33_phaseII/m33_ofiles/sig_mag/'
fit.dir = '~/Work/m33_mira/cmb_lcs/sig_mag_fit/'
f.fit = 'sig_mag_fit.dat'
lf.fit = paste0(fit.dir, f.fit)
fit = read.table(lf.fit)
idx = which(fit[,1] != 'w8i' & fit[,2] != 'g07nie9')
fit = fit[idx,]
idx = which(fit[,1] != 'wbi' & fit[,2] != 'g04niec')
fit = fit[idx,]
idx = which(fit[,1] != 'wsi' & fit[,2] != 'f25nie9')
fit = fit[idx,]
n.fit = nrow(fit)
cal.err = function(x) return(a^(x-b)+c)

f.eps = paste0(dir, 'sig_instr_mag.eps')
## setEPS()
width = 8
height = width * 0.618
## postscript(f.eps, width=width, height=height)
f.pdf = paste0(dir, 'sig_instr_mag.pdf')
pdf(f.pdf, width=width, height=height)
x = seq(10, 24, 0.1)
col = rgb(0,0,0,0.05)
ylim = c(0, 2)
xlim = c(14, 24)
## n.fit = 10
for (i in 1:n.fit) {
    a = fit[i,3]
    b = fit[i,4]
    c = fit[i,5]
    y = cal.err(x)
    if (i==1) {
        plot(x, y, type='l', col=col, xlim=xlim, ylim=ylim, xlab='Instrumental I (mag)', ylab='Sigma (mag)')
    } else {
        lines(x, y, col=col)
    }
}
dev.off()

cfit = fit
par(mfrow = c(2,1))



phot.dir = '~/Work/m33_mira/phot/i/'
for (i in 1:n.fit) {
    field = cfit[i, 1]
    root = cfit[i,2]
    zer.dir = paste0(phot.dir, field, '_allframe/trialp_all_', field, '/')
    f.zer = paste0(zer.dir, field, '.zer')
    f.tmp = 'tmp.zer'
    cmd = paste0('grep \'', root, '\' ', f.zer, ' | grep \'<\' > ', f.tmp)
    system(cmd)
    zer = read.table(f.tmp)
    dm = zer[1,2]
    cfit[i,4] = cfit[i,4] + dm
}
cmd = paste0('rm -f ',f.tmp)
system(cmd)

hist(cfit[,4], breaks=50)


off.dir = '~/Work/m33_mira/cmb_lcs/mag_off/'
f.off = paste0(off.dir, 'fnl_zero_point.dat')
off = read.table(f.off)
off[,1] = as.character(off[,1])
fit[,1] = as.character(fit[,1])
fit[,2] = as.character(fit[,2])
fields = off[,1]
for (field in fields) {
    idx = which(off[,1] == field)
    a = off[idx,2]
    b = off[idx,3]
    idx = which(cfit[,1] == field)
    cfit[idx, 4] = cfit[idx, 4] * a + b
}
hist(cfit[,4], breaks=50)

