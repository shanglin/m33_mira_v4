f.dat = '~/Work/m33_phaseII/lmc_ofiles/lmc_cpp_pars_all.dat'
dat = read.table(f.dat)
idx = dat[,3] > 0
dat = dat[idx,]

op = dat[,2]
gp = dat[,3]
x = op
y = gp - op

f.pdf = '~/Work/m33_phaseII/lmc_ofiles/lmc_gp_pars/gp_period.pdf'
pdf(f.pdf, width=7, height=8)
par(mfrow=c(2,1), mar=c(5,5,1,1))
plot(x, y, pch=1, cex=0.3,
     xlab = 'OGLE Period [day]',
     ylab = expression(paste('P'[GP],' - P'[OGLE],' [day]'))
     )



y = y / x

plot(x, y, pch=1, cex=0.3,
     xlab = 'OGLE Period [day]',
     ylab = expression(paste(Delta,' P / P'[OGLE])),
     ylim=c(-0.1,0.1)
     )

dev.off()
