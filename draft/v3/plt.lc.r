set.seed(101)

outdir = '~/Work/m33_phaseII/draft/v3.0/figures/'
f.cad = '~/Work/m33_phaseII/mira_cata_sampsize/cadco.csv'
f.rd = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
lcdir = '~/Work/m33_phaseII/m33_ofiles/islcs/'

f.eps = paste0(outdir, 'lc.eps')
setEPS()
postscript(f.eps, width=10, height=6.5)


par(tck=0.04, cex.axis=1.1, cex.lab=1.3)
y1 = 0.4165
y2 = 0.6745
fig1 = c(0, 1, 0, y1)
fig2 = c(0, 1, y1, y2)
fig3 = c(0, 1, y2, 1)
mar1 = c(5,5,0,3)
mar2 = c(0,5,0,3)
mar3 = c(0,5,2,3)
xlim = c(300, 3600)
plt.amp = 2.9

cad = read.csv(f.cad, stringsAsFactors=F)
ncad = nrow(cad)
rd = read.table(f.rd, stringsAsFactors=F)
idx = cad[,'n.obs'] > 55
cad = cad[idx,]
ncad = nrow(cad)
toplot = sample(1:ncad, 3)
subcad = cad[toplot,]
idx = order(subcad[,'Y_prob'])
subcad = subcad[idx,]

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

for (icad in 1:3) {

    new = T
    xaxt = 'n'
    if (icad==1) {
        fig = fig1
        mar = mar1
        new = F
        xaxt = 's'
        at = c(21, 19)
    }
    if (icad==2) {
        fig = fig2
        mar = mar2
        at = c(21.5, 19.5)
    }
    if (icad==3) {
        fig = fig3
        mar = mar3
        at = c(20,18)
    }
    par(fig = fig, new=new, mar=mar)
    
    if (T) {
    f.lc = subcad[icad, 1]
    prob = subcad[icad, 'Y_prob']
    id = gsub('.slc','',f.lc)
    idx = which(rd[,1] == id)
    if (length(idx) != 1) stop(id)
    ra.d = rd[idx, 2]
    dec.d = rd[idx, 3]
    ra = rad2ra(ra.d)
    dec = decd2dec(dec.d)
    name = sprintf('%1s%2i%2i%5.2f%1s%2i%2i%4.1f','J',ra[1],ra[2],ra[3],'+',dec[1],dec[2],dec[3])
    name = gsub(' ','0',name)

    f = subcad[icad, 3]
    period = 1/f
    n.obs = subcad[icad,'n.obs']
    spec = 'C-rich'
    if (subcad[icad,'O_prob'] > 0.5) spec = 'O-rich'
    amp = subcad[icad, 'A.model']
    lf.lc = paste0(lcdir, f.lc)
    lc = read.table(lf.lc)
    y = lc[,2]
    idx = abs(y - mean(y)) < 3*sd(y)
    lc = lc[idx,]
    t = lc[,1]
    m = lc[,2]
    e = lc[,3]
    mean.mag = mean(m)

    theta.1 = subcad[icad, 'theta.1']
    theta.2 = 10^subcad[icad, 'log10.theta.2']

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
    
    tpl.mjd = seq(min(lc[,1])-30, max(lc[,1])+30, length = n.tpl)
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
}


    x = lc[,1]
    y = lc[,2]
    e = lc[,3]
    xc = tpl.mjd
    yc = tpl.mag
    amp = plt.amp
    xlab = 'HJD - 2450000'
    if (icad == 2) 
        ylab = expression(paste(italic(I),' [mag]'))
    else
        ylab = ''
    ylim = c(mean(yc) + amp/2, mean(yc) - amp/2)
    plot(x, y, pch=19, cex=0.7, xlab=xlab, ylab=ylab, ylim=ylim, xaxt=xaxt, xlim=xlim,yaxt='n')

    at = seq(15, 25, 1)
    axis(2, at = at, label=at)
    at = seq(15, 25, 0.1)
    axis(2, at = at, labels=F, tck=0.02)
    arrows(x, y-e, x, y+e, length=0, code=3, angle=90)
    lines(xc, yc)
    xt = 250
    yt1 = mean(yc) - amp*0.37
    yt2 = mean(yc) - amp*0.27
    if (icad==3) {
        yt1 = mean(yc) - amp*0.43
        yt2 = mean(yc) - amp*0.33
    }
    text(xt, yt1, name, cex=1., adj=c(0,0), col=1)
    prob = sprintf('%4.2f',prob)
    t3 = bquote(italic(P)[italic(M)]~' = '~.(prob))
    text(xt, yt2, t3, cex=1., adj=0, col=1)
}

dev.off()
