outdir = '~/Work/m33_phaseII/draft/v1/tables/'
f.cad = '~/Work/m33_phaseII/mira_cata_sampsize/cadco.csv'
f.rd = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
lcdir = '~/Work/m33_phaseII/m33_ofiles/islcs/'

cad = read.csv(f.cad, stringsAsFactors=F)
ncad = nrow(cad)
## rd = read.table(f.rd, stringsAsFactors=F)

rad2ra = function(ra.d) {
    x = ra.d / 15
    h = floor(x)
    y = (x - h) * 60
    m = floor(y)
    s = (y - m) * 60
    return(c(h, m, s))
}

decd2dec = function(dec.d) {
    x = dec.d
    d = floor(x)
    y = (x - d) * 60
    m = floor(y)
    s = (y - m) * 60
    return(c(d, m, s))
}

PI = 3.1415926536
prior.gamma.cov = diag(5, 3, 3)
prior.gamma.cov.inv = solve(prior.gamma.cov)
n.tpl = 1000

f.out = paste0(outdir, 'pars.dat')
ts = '#  ID    ra  dec   mean.mag  peirod   amp   max.light   spec.type   n.obs'
write(ts, f.out)
for (icad in 1:ncad) {
    print(paste(icad, ncad))
    f.lc = cad[icad, 1]
    id = gsub('.slc','',f.lc)
    idx = which(rd[,1] == id)
    if (length(idx) != 1) stop(id)
    ra.d = rd[idx, 2]
    dec.d = rd[idx, 3]
    ra = rad2ra(ra.d)
    dec = decd2dec(dec.d)
    name = sprintf('%1s%2i%2i%5.2f%1s%2i%2i%4.1f','J',ra[1],ra[2],ra[3],'+',dec[1],dec[2],dec[3])
    name = gsub(' ','0',name)

    f = cad[icad, 3]
    period = 1/f
    n.obs = cad[icad,'n.obs']
    spec = 'C-rich'
    if (cad[icad,'O_prob'] > 0.5) spec = 'O-rich'
    amp = cad[icad, 'A.model']
    lf.lc = paste0(lcdir, f.lc)
    lc = read.table(lf.lc)
    y = lc[,2]
    idx = abs(y - mean(y)) < 3*sd(y)
    lc = lc[idx,]
    t = lc[,1]
    m = lc[,2]
    e = lc[,3]
    mean.mag = mean(m)

    theta.1 = cad[icad, 'theta.1']
    theta.2 = 10^cad[icad, 'log10.theta.2']

    phase = 2 * PI * f * lc[,1]
    prior.gamma = matrix(0, nrow = 3, ncol = 1)
    prior.gamma[1,1] = mean.mag
    H = matrix(1, ncol = 3, nrow = n.obs)
    H[, 2] = cos(phase)
    H[, 3] = sin(phase)
    Kc = matrix(NA, n.obs, n.obs)
    theta.1.sqr = theta.1 * theta.1
    theta.2.sqr = theta.2 * theta.2
    for (ik1 in 1:n.obs) {
        for (ik2 in ik1:n.obs) {
            Kc[ik1, ik2] = theta.1.sqr * exp(-(lc[ik1,1] - lc[ik2,1])^2 / (2*theta.2.sqr))
            if (ik1 == ik2) {
                Kc[ik1, ik2] = Kc[ik1, ik2] + lc[ik1, 3]^2
            } else {
                Kc[ik2, ik1] = Kc[ik1, ik2]
            }
        }
    }
    y = matrix(lc[,2], nrow = n.obs, ncol = 1)
    Kc.inv = matrix(0, n.obs, n.obs)
    try(Kc.inv <- solve(Kc), silent = T)
    tmp.1 = t(H) %*% Kc.inv %*% H + prior.gamma.cov.inv
    tmp.1.inv = matrix(0, 3, 3)
    try(tmp.1.inv <- solve(tmp.1), silent = T)
    tmp.2 = prior.gamma.cov.inv %*% prior.gamma + t(H) %*% Kc.inv %*% y
    posterior.gamma = tmp.1.inv %*% tmp.2
    posterior.mean.mag = posterior.gamma[1]
    
    tpl.mjd = seq(min(lc[,1]), max(lc[,1]), length = n.tpl)
    n.new.tpl = n.tpl
    H.star = matrix(1, ncol = 3, nrow = n.new.tpl)
    phase.star = 2 * PI * f * tpl.mjd
    H.star[, 2] = cos(phase.star)
    H.star[, 3] = sin(phase.star)
    Kc.star = matrix(NA, nrow = n.new.tpl, ncol = n.obs)
    for (ik1 in 1:n.new.tpl) {
        for (ik2 in 1:n.obs) {
            Kc.star[ik1, ik2] = theta.1.sqr * exp(-(tpl.mjd[ik1] - lc[ik2,1])^2 / (2*theta.2.sqr))
        }
    }
    tpl.mag = H.star %*% posterior.gamma + Kc.star %*% Kc.inv %*% (y - H %*% posterior.gamma)
    
    ## plot(lc[,1],lc[,2],pch=19,cex=0.7,xlab='JD - 2450000')
    ## arrows(lc[,1], lc[,2]+lc[,3], lc[,1], lc[,2]-lc[,3], length=0, code=3, angle=90)
    ## lines(tpl.mjd, tpl.mag)

    max.light = min(tpl.mag)

    ts = paste(name, ra.d, dec.d, round(mean.mag,3), round(period,2), round(amp,3), round(max.light,3), spec, n.obs, sep='   ')
    write(ts, f.out, append=T)
}
