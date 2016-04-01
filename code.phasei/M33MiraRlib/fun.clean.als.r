library(stats)

clean.als = function(dir, f.als, f.new = 'default', sigma.cut = 3) {
    if (f.new == 'default') f.new = f.als
    lf.als = paste0(dir, f.als)
    lf.new = paste0(dir, f.new)
    als = read.table(lf.als, skip = 3)
    x = als[,2]
    y = als[,3]
    sky = als[,6]

    fit = lm(V6 ~ V2 + V3, data = als)
    new.sky = sky - predict(fit, als)

    idx = new.sky < sigma.cut*sd(new.sky)
    new.sky = new.sky[idx]
    x = x[idx]
    y = y[idx]
    con = file(lf.als, 'r')
    str.als = readLines(con)
    close(con)
    idx = c(T,T,T, idx)
    str.als = str.als[idx]
    
    fit.x = smooth.spline(x, new.sky)
    new.sky = new.sky - predict(fit.x, x)$y
    fit.y = smooth.spline(y, new.sky)
    new.sky = new.sky - predict(fit.y, y)$y

    ## plot(y, new.sky, col=rgb(0,0,0,0.1))
    ## abline(h = sigma.cut*sd(new.sky), col= 2)
    idx = new.sky < sigma.cut*sd(new.sky)
    idx = c(T,T,T, idx)
    str.als = str.als[idx]
    write(str.als, lf.new)
    
    return(1)
}

## dir = '~/Work/m33_mira/phot/i/w9i_allframe/'
## f.als = 'w9i.als'
## ret = clean.als(dir, f.als, 'test.als')
## lf.new = paste0(dir, 'test.als')
## als = read.table(lf.new, skip = 3)
## plot(als[,2], als[,6], col=rgb(0,0,0,0.1))
