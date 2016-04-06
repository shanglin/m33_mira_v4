dir = '~/Work/m33_phaseII/m33_ofiles/sig_mag/'
lc.dir = '~/Work/m33_phaseII/m33_ofiles/islcs/'

phot.dir = '~/Work/m33_mira/phot/i/'
fields = list.files(phot.dir, pattern='.*.allframe$')
fields = gsub('_allframe','', fields)

f.all = paste0(dir,'slc_all.dat')
if (F) {
    cmd = paste0('rm -f ',f.all)
    system(cmd)
    for (field in fields) {
        print(field)
        fs.lc = list.files(lc.dir, pattern=paste0('^',field,'.*.slc$'))
        nfs = length(fs.lc)
        for (i in 1:nfs) {
            cmd = paste0('cat ',lc.dir,fs.lc[i],' >> ',f.all)
            system(cmd)
        }
    }
}


all = read.table(f.all)
tbl = table(all[,1])
sframes = names(tbl)
fig.dir = paste0(dir, 'pngs/')
cal.err = function(x, a, b, c) return(a^(x-b)+c)
f.dat = 'lc_sig_mag.dat'
lf.dat = paste0(dir, f.dat)
ts = '# MJD          a         b         c'
write(ts, lf.dat)
for (sframe in sframes) {
    f.png = paste0(fig.dir, sframe, '.png')
    png(f.png)
    idx = which(all[,1] == sframe)
    sub = all[idx,]
    m = sub[,2]
    e = sub[,3]
    ylim = c(0, 2)
    xlim = range(m)
    main = sframe
    plot(m, e, pch=19, cex=0.1, ylim=ylim, main=main, xlab='I (mag)', ylab='sigma (mag)')
    by = 0.2
    x = seq(16, 24 , by=by)
    n = length(x)
    y = rep(NA, n)
    for (i.x in 1:n) {
        idx = m > x[i.x] - by/2 & m < x[i.x] + by/2
        if (sum(idx) > 3) {
            y[i.x] = quantile(e[idx], 0.4)
        }
    }
    idx = !is.na(y)
    x = x[idx]
    y = y[idx]
    pars = -1
    try(fit <- nls(y ~ cal.err(x, a, b, c),
        start = list(a=2, b=20, c=0.01)), silent=T
        )
    pars = coef(fit)
    if (pars[1] == -1) {
        fit = nls(e ~ cal.err(m, a, b, c),
            start = list(a=2, b=20, c=0.01))
        pars = coef(fit)
    }
    a = round(pars['a'],5)
    b = round(pars['b'],5)
    c = round(pars['c'],5)
    xc = seq(0, 30, 0.1)
    yc = cal.err(xc, a, b, c)
    lines(xc, yc, col=2)
    dev.off()
    ts = paste(sframe, a, b, c, sep='   ')
    write(ts, lf.dat, append = T)
}
