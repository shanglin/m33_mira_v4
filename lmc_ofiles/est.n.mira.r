f.lum = '~/Work/m33_phaseII/m33_ofiles/complete_fun/lum_fun.dat'
lum = read.table(f.lum)

f.bri = '~/Work/m33_phaseII/lmc_ofiles/ogleIIphot/bright.dat'
bri = read.table(f.bri)

mag.shift = 6.2
bri[,3] = bri[,3] + mag.shift

idx = match(bri[,3], lum[,1])
num = sum(lum[idx,2], na.rm=T)

print(num)

f.csv = '~/Work/m33_phaseII/lmc_ofiles/mira.cat.csv'
csv = read.csv(f.csv)
imag = csv[,'I']
idx = imag < 21.45 - mag.shift
print(sum(idx))
sub = imag[idx]
idx = match(sub+mag.shift, lum[,1])
num = sum(lum[idx,2], na.rm=T)
print(num)


f.m33 = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
m33 = read.table(f.m33)
idx = m33[,4] > 18.5 & m33[,4] < 20
h1 = hist(m33[idx,4], breaks=100, plot=F)

f.eps = '~/Work/m33_phaseII/lmc_ofiles/ogleIIphot/mch_lf.eps'
setEPS()
postscript(f.eps, width=12, height=12)
par(mfrow = c(2,2))
xlab = 'I (mag)'
ylab = 'Counts OR counts * factor'
main = 'counts(LMC) * 8'
plot(h1$mids, h1$counts, type='s', ylim=c(0,max(h1$counts)), xlab=xlab, ylab=ylab, main = main)

idx = bri[,3] > 18.5 & bri[,3] < 20
h2 = hist(bri[idx,3], breaks=100, plot=F)
lines(h2$mids, h2$counts, type='s', col=2)
lines(h2$mids, h2$counts*8, type='s', col=2, lty=2)
legend('topleft',c('M33','LMC','LMC_scaled'),lty=c(1,1,2), col=c(1,2,2))

for (i in c(9,10)) {
    main = paste0('counts(LMC) * ',i)
    plot(h1$mids, h1$counts, type='s', ylim=c(0,max(h1$counts)), xlab=xlab, ylab=ylab, main=main)
    lines(h2$mids, h2$counts, type='s', col=2)
    lines(h2$mids, h2$counts*i, type='s', col=2, lty=2)
}

sfactor = h1$counts/h2$counts
sweight = h1$counts
i = sum(sweight*sfactor)/(sum(sweight))
i = round(i,2)
print(i)

main = paste0('counts(LMC) * ',i)
plot(h1$mids, h1$counts, type='s', ylim=c(0,max(h1$counts)), xlab=xlab, ylab=ylab, main=main)
lines(h2$mids, h2$counts, type='s', col=2)
lines(h2$mids, h2$counts*i, type='s', col=2, lty=2)
t1 = 'Best fit with weight = counts'
text(18.5, 1300, t1, adj=0, cex=1.5)
dev.off()


f.eps = '~/Work/m33_phaseII/lmc_ofiles/ogleIIphot/new_completeness.eps'
setEPS()
postscript(f.eps, width=6, height=6)
idx = m33[,4] > 18.5 & m33[,4] < 21.45
h1 = hist(m33[idx,4], breaks=100, plot=F)
idx = bri[,3] > 18.5 & bri[,3] < 21.45
h2 = hist(bri[idx,3], breaks=100, plot=F)
xlab = 'I (mag)'
ylab = 'Counts OR counts * factor OR completeness'
main = paste0('counts(LMC) * ',i)
ylim = c(0, max(h2$counts)*i)
plot(h1$mids, h1$counts, type='s', ylim=ylim, xlab=xlab, ylab=ylab, main = main)
h2 = hist(bri[idx,3], breaks=100, plot=F)
lines(h2$mids, h2$counts, type='s', col=2)
lines(h2$mids, h2$counts*i, type='s', col=2, lty=2)
legend('topleft',c('M33','LMC','LMC_scaled','Completeness * 15000','Original completeness'),lty=c(1,1,2,1,2), col=c(1,2,2,3,3))

complete = h1$counts / (h2$counts*i)
idx = h2$mids < 20
complete[idx] = 1
scomp = complete*15000
lines(h2$mids, scomp, col=3)
lines(lum[,1],lum[,2]*15000, col=3, lty=2)
dev.off()


comp = scomp/15000
lum2 = lum
lum2[,2] = (lum2[,2] - 1)*(-1)
lum2[,2] = lum2[,2] * (1-comp[length(comp)]) / (1-lum[nrow(lum),2])
lum2[,2] = lum2[,2] * (-1) + 1
f.png = 'adj_lum.png'
png(f.png)
plot(h2$mids, comp,type='l', xlab='I (mag)', ylab='Updated Luminosity Function')
lines(lum2, col=2)
dev.off()

f.lum2 = 'adj_lum.dat'
write.table(lum2, f.lum2, row.names=F, col.names=F, quote=F)
lum = read.table(f.lum2)


idx = match(bri[,3], lum[,1])
num = sum(lum[idx,2], na.rm=T)
print(num)

imag = csv[,'I']
idx = imag < 21.45 - mag.shift
print(sum(idx))
sub = imag[idx]
idx = match(sub+mag.shift, lum[,1])
num = sum(lum[idx,2], na.rm=T)
print(num)
