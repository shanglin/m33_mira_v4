library(stats)

field = 'wti'

dir = '~/Work/m33_mira/cmb_lcs/'
phot.dir = '~/Work/m33_mira/phot/i/'

fit.dir = paste0(dir, 'sig_mag_fit/')
cmd = paste0('mkdir -p ', fit.dir)
system(cmd)
f.dat = 'sig_mag_fit_wti.dat'
lf.dat = paste0(fit.dir, f.dat)
png.dir = paste0(fit.dir, 'pngs/')
cmd = paste0('mkdir -p ', png.dir)
system(cmd)

## fields = list.files(phot.dir, pattern='....allframe')
ts = '# field  image      a        b         c     x.shift  y.shift'
write(ts, lf.dat)

## for (lfield in fields) {
    ## field = substr(lfield,1,3)
    print(field)

    alf.dir = paste0(phot.dir, field, '_allframe/')
    
    cal.err = function(x, a, b, c) {
        return(a^(x-b) + c)
    }

    fs.alf = list.files(alf.dir, pattern='*.alf$')
    nfs.alf = length(fs.alf)
    
    for (i.alf in 1:nfs.alf) {
        f.alf = fs.alf[i.alf]
        root = gsub('.alf', '', f.alf)
        f.png = paste0('sig_mag_', field, '_', root, '.png')
        lf.png = paste0(png.dir, f.png)

        png(lf.png)
        lf.alf = paste0(alf.dir, f.alf)
        alf = read.table(lf.alf, skip=3)
        m = alf[,4]
        e = alf[,5]
        ylim = c(0, 2)
        xlim = range(m)
        main = paste0('Field: ',field,'  Frame: ',root)
        plot(m, e, pch=19, cex=0.1, ylim=ylim, main=main, xlab='Instrumental I (mag)', ylab='sigma (mag)')

        by = 0.1
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
        fit = nls(y ~ cal.err(x, a, b, c),
            start = list(a=2.5, b=19, c=0.01))
        pars = coef(fit)
        a = pars['a']
        b = pars['b']
        c = pars['c']
        xc = seq(0, 30, 0.1)
        yc = cal.err(xc, a, b, c)
        lines(xc, yc, col=2)
        text = paste0('a=',round(a,4),'\n b=',round(b,4),'\n c=',round(c,4))
        text(xlim[1]+2, ylim[2]-0.2 ,text, cex=1)
        ne = e - cal.err(m, a, b, c)
        idx = m < quantile(m, 0.5)
        x.shift = 3 * sqrt(sd(ne[idx]))
        y.shift = 0.5 * sqrt(sd(ne[idx]))
        yc = cal.err(xc + x.shift, a, b, c) + y.shift
        lines(xc, yc, col=4, lty=2)
        dev.off()
        
        ts = sprintf('%5s%10s%9.5f%10.5f%9.5f%8.3f%9.3f',field, root, round(a, 5), round(b, 5), round(c, 5), x.shift, y.shift)
        write(ts, lf.dat, append = T)
    }
## }
