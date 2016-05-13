PI = 3.1415926536
prior.gamma.cov = diag(5, 3, 3)
prior.gamma.cov.inv = solve(prior.gamma.cov)
n.tpl = 1000

newdir = '~/Work/m33_phaseII/rf_model_sampsize/lc_pngs/'
cpimg = function(f.eps) {
    f.png = gsub(epsdir, newdir, f.eps)
    f.png = gsub('.eps','.png',f.png)
    f.png = gsub('.pdf','.png',f.png)
    cmd = paste0('convert ',f.eps,' ', f.png)
    system(cmd)
}


mkfitfig = function(sf, f, theta.1, theta.2) {
    id = gsub('.slc','',sf)
    f.eps = paste0(epsdir, id,'.eps')
    setEPS()
    postscript(f.eps, width=11, height=3.4)
    lf = paste0(lcdir, sf)
    lc = read.table(lf)
    idx = abs(lc[,2] - mean(lc[,2])) < 3.*sd(lc[,2]) # This step is included in GPmodel (c++ 2.8 version)
    lc = lc[idx,]
    n.obs = nrow(lc)
    mean.mag = mean(lc[,2])
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

    nf = layout(matrix(c(1,2),1,2),width=c(2,1))
    xlim = c(200, 3800)
    ylim = c(1.8,-1.8)
    par(mar=c(3,3.5,1,1), mgp=c(2,1,0))
    if (abs(posterior.mean.mag - mean(lc[,2])) > 0.2) {
        new.mean.mag = mean(lc[,2])
    } else {
        new.mean.mag = posterior.mean.mag
    }
    ylab = expression(paste(Delta, italic('I'),' [mag]'))
    plot(lc[,1],lc[,2]-new.mean.mag,pch=19,cex=0.7,xlab='JD - 2450000',ylab=ylab,xlim=xlim,ylim=ylim,main='')
    arrows(lc[,1], lc[,2]+lc[,3]-new.mean.mag, lc[,1], lc[,2]-lc[,3]-new.mean.mag, length=0, code=3, angle=90)
    lines(tpl.mjd, tpl.mag-new.mean.mag)
    text(250, - 1.5, id, adj=0)



    nKc = Kc - diag(lc[,3]^2)
    pts = H %*% posterior.gamma - posterior.mean.mag
    hts = nKc %*% Kc.inv %*% (y - H %*% posterior.gamma)
    xps = (phase / (2*PI)) %% 1
    xps = c(xps, xps+1)
    mag2 = c(lc[,2],lc[,2]) - posterior.mean.mag - c(hts,hts)
    err2 = c(lc[,3],lc[,3])
    ylab = expression(paste(italic(p[t]),' [mag]'))
    plot(xps, mag2, pch=19, cex=0.7, xlab='Phase',ylab=ylab, ylim=ylim, xlim=c(0,2), xaxt='n')
    axis(1, at=c(0,.5,1,1.5,2), labels=c('0','0.5','1','0.5','1'))
    arrows(xps, mag2+err2, xps, mag2-err2, code=3, length=0, angle=90)
    xcnt = (phase.star / (2*PI)) %% 1
    idx = order(xcnt)
    xcnt = xcnt[idx]
    xcnt = c(xcnt, xcnt + 1)
    ycnt = H.star %*% posterior.gamma - posterior.mean.mag
    ycnt = ycnt[idx]
    ycnt = c(ycnt, ycnt)
    lines(xcnt, ycnt)
    period = round(1/f,2)
    t1 = expression(paste(italic('P'),' = ',period, 'd'))
    t1 = bquote(italic('P') == .(period) ~ 'd')
    text(0.1, - 1.5, t1, adj=0)
    dev.off()
    cpimg(f.eps)
    
    cyc = (max(lc[,1]) - min(lc[,1])) / period
    return(cyc)
}
