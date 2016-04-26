set.seed(101)

lc.dir = '~/Work/m33_phaseII/m33_v4_lcs/'
out.dir = '~/Work/m33_phaseII/sc_ext_ftrs/'

fs = list.files(lc.dir)
nfs = length(fs)
idx = sample(1:nfs)
fs = fs[idx]

if (F) {
f.tmp = 'mjd.tmp'
write('#',f.tmp)
for (i in 1:900) {
    f.lc = fs[i]
    lf.lc = paste0(lc.dir, f.lc)
    lc = read.table(lf.lc)
    write.table(lc[,1], f.tmp, col.names=F, row.names=F, append=T)
}

tmp = read.table(f.tmp)
hist(tmp[,1], breaks=500, xlim=c(0,4000))
seps = c(0,500,800,1200,2000,2800,3100)
abline(v=seps,col=2)
system(paste0('rm -f ',f.tmp))
}

plot.lc = function(lc) {
    x = lc[,1]
    y = lc[,2]
    e = lc[,3]
    plot(x, y, pch=19, ylim=c(max(y)+0.2,min(y)-0.2), xlab='MJD', ylab='I (mag)', main=f.lc)
    arrows(x, y+e, x, y-e, code=3, length=0, angle=90)
    return(x)
}

qdr.fit.res = function(a) {
    x = a[,1]
    mean.x = mean(x)
    x = x - mean.x
    y = a[,2]
    n = nrow(a)
    X = matrix(1, ncol=3, nrow=n)
    X[,1] = x^2
    X[,2] = x
    Y = matrix(y, ncol=1, nrow=n)
    b = solve(t(X) %*% X) %*% (t(X) %*% Y)
    xc = seq(min(x), max(x), length=100)
    yc = b[1]*xc^2 + b[2]*xc + b[3]
    ## lines(xc+mean.x,yc,col=2)
    residuals = y - (b[1]*x^2 + b[2]*x + b[3])
    return(residuals)
}

seps = c(0,500,800,1200,2000,2800,3100,5000)

f.out = paste0(out.dir, 'qdr_fit_features.dat')
write('#               id            mean_grp_sd  qdr_residual_sd  ratio_q2m', f.out)

for (i in 1:nfs) {

    msg = paste0('   >> ',round(100*i/nfs, 2), ' %   \r')
    message(msg, appendLF=F)
    
    grp.sds = rep(NA, 7)
    all.residuals = c()
    no.data = T
    
    f.lc = fs[i]
    lf.lc = paste0(lc.dir, f.lc)
    lc = read.table(lf.lc)
    ## x = plot.lc(lc)
    x = lc[,1]

    for (i.grp in 1:7) {
        idx = x > seps[i.grp] & x < seps[i.grp+1]
        if (sum(idx) > 7) {
            grp = lc[idx,]
            grp.sds[i.grp] = sd(grp[,2])
            all.residuals = c(all.residuals, qdr.fit.res(grp))
            no.data = F
        }
    }

    if (no.data == F) {
        idx = !is.na(grp.sds)
        grp.sds = grp.sds[idx]
        mean.grp.sd = mean(grp.sds)
        qdr.res.sd = sd(all.residuals)
        sd.ratio = qdr.res.sd / mean.grp.sd
    } else {
        grp = lc
        mean.grp.sd = sd(grp[,2])
        all.residuals = qdr.fit.res(grp)
        qdr.res.sd = sd(all.residuals)
        sd.ratio = qdr.res.sd / mean.grp.sd
    }

    id = gsub('.slc','',f.lc)
    id = gsub('.flc','',f.lc)
    ts = sprintf('%30s%12.3f%12.3f%12.3f',id, mean.grp.sd, qdr.res.sd, sd.ratio)
    write(ts, f.out, append=T)
    ## Sys.sleep(2)
}
