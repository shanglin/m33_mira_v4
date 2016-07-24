psign = function(x) {
   ret = sign(x)
   ret[ret==0] = 1
   return(ret)
}

ra2dra = function(h, m, s) {
    dra = 15*(h + m/60 + s/3600)
    return(dra)
}

dec2ddec = function(d, m, s) {
    ddec = psign(d) * (abs(d) + m/60 + s/3600)
    return(ddec)
}


dir = '~/Work/m33_phaseII/lmc_ofiles/'
lcdir = paste0(dir, 'mira.lcs/')
outdir = paste0(dir, 'lmc_gp_pars/')
options(stringsAsFactors = F)

f.dat = paste0(dir, 'lmc_cpp_pars_all.dat')
dat = read.table(f.dat)

f.csv = paste0(dir, 'mira.cat.csv')
csv = read.csv(f.csv)
ncsv = nrow(csv)

f.out = paste0(dir, 'lmc_gp_feature.dat')
ts = '        OGLE_ID            Designation        R.A.       Dec.      <I>      GP_Period  Ampl_P    Imin    Cl   N  <I>_template[Not_used]'
write(ts, f.out)

PI = 3.1415926536
prior.gamma.cov = diag(5, 3, 3)
prior.gamma.cov.inv = solve(prior.gamma.cov)
n.tpl = 1000

for (ic in 1:ncsv) {
    print(ic)
    id = csv[ic,1]
    f.fig = paste0(outdir, id, '_gp_fit.pdf')
    pdf(f.fig, width=7, height=4)
    ra = csv[ic,'RA']
    dec = csv[ic,'Decl']
    cl = csv[ic,'Spectr']
    cl = substr(cl,1,1)
    ra.str = strsplit(ra,':')[[1]]
    ra.flt = as.numeric(ra.str)
    dra = ra2dra(ra.flt[1],ra.flt[2],ra.flt[3])
    dec.str = strsplit(dec,':')[[1]]
    dec.flt = as.numeric(dec.str)
    ddec = dec2ddec(dec.flt[1],dec.flt[2],dec.flt[3])
    ra.fix = gsub(' ','0',ra)
    dec.fix = gsub(' ','0',dec)
    ra.fix = gsub(':','',ra.fix)
    dec.fix = gsub(':','',dec.fix)
    designation = paste0('J',ra.fix,dec.fix)

    idx = dat[,1] == id
    if (sum(idx) != 1) stop(id)
    period = dat[idx,3]
    if (period < 0) period = dat[idx,2]
    theta.1 = exp(dat[idx,4])
    theta.2 = exp(dat[idx,5])
    F.peak = 1/period
    
    f.lc = paste0(lcdir, id, '.dat')
    lc = read.table(f.lc)
    n.obs = nrow(lc)
    mean.mag = mean(lc[,2])
    phase = 2 * PI * F.peak * lc[,1]
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
    ## posterior.sigma.a = tmp.1.inv[2,2]
    ## posterior.sigma.b = tmp.1.inv[3,3]
    A.model = sqrt(posterior.gamma[2]^2 + posterior.gamma[3]^2) * 2
    
    tpl.mjd = seq(min(lc[,1]), max(lc[,1]), length = n.tpl)
    n.new.tpl = n.tpl
    H.star = matrix(1, ncol = 3, nrow = n.new.tpl)
    phase.star = 2 * PI * F.peak * tpl.mjd
    H.star[, 2] = cos(phase.star)
    H.star[, 3] = sin(phase.star)
    Kc.star = matrix(NA, nrow = n.new.tpl, ncol = n.obs)
    for (ik1 in 1:n.new.tpl) {
        for (ik2 in 1:n.obs) {
            Kc.star[ik1, ik2] = theta.1.sqr * exp(-(tpl.mjd[ik1] - lc[ik2,1])^2 / (2*theta.2.sqr))
        }
    }
    tpl.mag = H.star %*% posterior.gamma + Kc.star %*% Kc.inv %*% (y - H %*% posterior.gamma)
    
    plot(lc[,1],lc[,2],pch=19,cex=0.7,xlab='JD - 2450000', ylab='I [mag]', ylim=rev(range(lc[,2])+c(-0.3,0.3)), main=id)
    arrows(lc[,1], lc[,2]+lc[,3], lc[,1], lc[,2]-lc[,3], length=0, code=3, angle=90)
    lines(tpl.mjd, tpl.mag)
    max.light = min(tpl.mag)

    ts = sprintf('%20s%22s%10.5f%11.5f%9.3f%12.2f%9.3f%9.3f%5s%5i%9.3f',
        id, designation, dra, ddec, mean.mag, period, A.model, max.light, cl, n.obs, posterior.mean.mag)
    write(ts, f.out, append=T)
    dev.off()
}

  
