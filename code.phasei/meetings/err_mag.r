library(stats)
cal.err = function(x, a, b, c) {
    return(a^(x-b) + c)
}

out.dir = '~/Work/m33_mira/meeting/mar24_2016/sig_mag/'

field = 'x1i'
dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/')

fs.alf = list.files(dir, pattern = '.*.alf$')
nfs.alf = length(fs.alf)

for (i in 1:nfs.alf) {
    f.alf = fs.alf[i]
    f.png = paste0(out.dir,field,'_',gsub('.alf','',f.alf),'.png')
    png(f.png)
    lf.alf = paste0(dir, f.alf)
    alf = read.table(lf.alf, skip=3)
    m = alf[,4]
    e = alf[,5]
    ylim = c(0, 1)
    main = paste0('Field: ',field,'  Frame: ',f.alf)
    plot(m, e, pch=19, cex=0.1, ylim=ylim, main=main, xlab='Instrumental I (mag)', ylab='sigma (mag)')

    by = 0.2
    x = seq(12, 20 , by=by)
    n = length(x)
    y = rep(NA, n)
    for (i.x in 1:n) {
        idx = m > x[i.x] - by/2 & m < x[i.x] + by/2
        if (sum(idx) > 20) {
            y[i.x] = quantile(e[idx], 0.4)
        }
    }
    idx = !is.na(y)
    x = x[idx]
    y = y[idx]
    ## points(x, y, pch=19, cex=1, col=4)
    fit = nls(y ~ cal.err(x, a, b, c),
        start = list(a=2, b=1, c=0))
    pars = coef(fit)
    a = pars['a']
    b = pars['b']
    c = pars['c']
    xc = seq(10, 20, 0.1)
    yc = cal.err(xc, a, b, c)
    lines(xc, yc, col=2)
    text = paste0('a=',round(a,4),'\n b=',round(b,4),'\n c=',round(c,4))
    text(10, 0.9 ,text, cex=1)
    x.shift = 0.5
    y.shift = 0.05
    yc = cal.err(xc + x.shift, a, b, c) + y.shift
    lines(xc, yc, col=4, lty=2)
    dev.off()
    ## stop()
}
