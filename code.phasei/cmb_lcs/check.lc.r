fig.dir = '~/Work/m33_mira/meeting/apr08_2016/fig.lc/'
dir = '~/Desktop/'
f = 'm33_i_catalog.dat'
lf = paste0(dir,f)
## dat = read.table(lf)
ndat = dat[dat[,6] > 9,]

mags = ndat[,4]
errs = ndat[,5]
ras = ndat[,2]
decs = ndat[,3]
n.obs = ndat[,6]

h1 = hist(dat[,4], breaks=100, plot=F)
h2 = hist(ndat[,4], breaks=100, plot=F)

width = 8
height = width*0.618
f.eps = paste0(fig.dir, 'luminosity_func.eps')
setEPS()
postscript(f.eps, width=width, height=height)
mag.cut = 21.45
plot(h1$mids, h1$counts, col=2, type='s', xlim=c(16, 24), xlab='I (mag)', ylab='Frequency', main='')
lines(h2$mids, h2$counts, col=4, type='s')
legend('topleft', legend=c('All light curves','Light curves with Nobs > 9'), col=c(2,4), lwd=3)
abline(v=mag.cut, col='grey', lty=2)
dev.off()

library(ggplot2)
sd = cbind(dat[,4], dat[5]*sqrt(dat[,6]))
nsd = cbind(ndat[,4], ndat[5]*sqrt(ndat[,6]))
colnames(sd) = c('m', 'sd')
colnames(nsd) = c('m', 'sd')
## sd = sd[1:10000,]

thm = theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())

p = ggplot(sd, aes(x = m, y = sd)) + stat_bin2d(bins=200) +
    scale_fill_gradient(low='gray100', high='gray0') +
    thm + xlim(16,24) + ylim(0, 1) +
    xlab('I (mag)') +
    ylab('Standard Deviation') +
    geom_vline(xintercept=mag.cut, colour='red', linetype=2) +
    annotate('text', x = 17, y=0.9, label = 'All light curves')

f.eps = paste0(fig.dir, 'mag_sd_all.eps')
setEPS()
postscript(f.eps, width=width, height=height)
print(p)
dev.off()

p = ggplot(nsd, aes(x = m, y = sd)) + stat_bin2d(bins=200) +
    scale_fill_gradient(low='gray100', high='gray0') +
    thm + xlim(16,24) + ylim(0, 1) +
    xlab('I (mag)') +
    ylab('Standard Deviation') +
    geom_vline(xintercept=mag.cut, colour='red', linetype=2) +
    annotate('text', x = 17.5, y=0.9, label = 'Light curves with Nobs > 9')

f.eps = paste0(fig.dir, 'mag_sd_cut.eps')
setEPS()
postscript(f.eps, width=width, height=height)
print(p)
dev.off()


## plot(mags, errs, pch=19, cex=0.1, col=rgb(0,0,0,0.2))
## hist(n.obs,breaks=max(n.obs))
## plot(ras, decs, pch=19, cex=0.02)


