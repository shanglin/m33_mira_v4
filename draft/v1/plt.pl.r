dir = '~/Work/m33_phaseII/draft/v1/tables/'
f.dat = paste0(dir, 'pars.dat')
dat = read.table(f.dat, stringsAsFactors=F)

idx = dat[,8] == 'O-rich'
orh = dat[idx,]
crh = dat[!idx,]

x = log10(orh[,5])
y = orh[,7]
ylim = c(21,18)
plot(x, y, pch=19, cex=0.5, col=rgb(1,0,0,0.3), ylim=ylim)

x = log10(crh[,5])
y = crh[,7]
points(x, y, pch=19, cex=0.5, col=2)

