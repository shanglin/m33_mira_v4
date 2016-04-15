args = commandArgs(trailingOnly = TRUE)
batch = args[1]
set.seed(batch)

fig.dir = '/fdata/scratch/yuanwenlong/m33_v4/extr_feature/figures/'
lc.dir = '/fdata/scratch/yuanwenlong/m33_v4/m33_v4_lcs/'
lst.dir = '/fdata/scratch/yuanwenlong/m33_v4/gene_spec/lsts/'
spc.dir = '/fdata/scratch/yuanwenlong/m33_v4/gene_spec/gp_spectra/'
out.dir = '/fdata/scratch/yuanwenlong/m33_v4/extr_feature/features/'

f.csv = 'mira.cat.csv'
csv = read.table(f.csv,sep=',',header=T)

f.out = paste0(out.dir, 'feature_',batch,'.dat')
cmd = paste0('rm -f ',f.out)
system(cmd)
if (batch == 1) {
    ts = '#                   ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag'
    write(ts, f.out)
}

PI = 3.1415926536
prior.gamma.cov = diag(5, 3, 3)
prior.gamma.cov.inv = solve(prior.gamma.cov)
n.tpl = 1000
source('llmax.r')

do.extract = function(i) {
    lf.lc = fs.lc[i]
    f.lc = gsub(lc.dir, '', lf.lc)
    f = paste0(f.lc,'.gp.dat')
    if (substr(f,1,4) == 'mira') {
        id = substr(f, 6, 10)
        lid = paste0('OGLE-LMC-LPV-',id)
        csv.idx = csv[,1]==lid
        t.freq = 1./csv[csv.idx, 'P_1']
    } else {
        t.freq = -1
    }
    
    lf = paste0(spc.dir, f)
    dat = read.table(lf)
    idx = dat[,1] < 0.01
    dat = dat[idx,]
    dat = dat[is.finite(dat[,2]),]
    n.dat = nrow(dat)
    maxima.index = localmaxima(dat[,2])
    if (maxima.index[1] == -2) {
        maxima.index = maxima.index[-c(1)]
        if (length(maxima.index) > 0) {
            max.dat = dat[maxima.index,]
            sort.idx = sort(max.dat[,2], index.return=T, decreasing=T)$ix
            max.dat = max.dat[sort.idx,]
            n.maxima = nrow(max.dat)
        } else {
            n.maxima = 0
        }
    } else {
        n.maxima = 0
    }

    if (n.maxima == 0) {
        F.true = t.freq
        F.peak = -1
        Q.peak = -1
        Q.base = -1
        dQ.p1.base = -1
        dQ.p1.p2 = -1
        theta.1 = -1
        theta.2 = -1
        log.theta.2 = -1
        A.model = -1
        sd.error = -1
        posterior.sigma.a = -1
        posterior.sigma.b = -1
        posterior.mean.mag = -1
        
        lc = read.table(lf.lc)
        idx = abs(lc[,2] - mean(lc[,2])) < 3.*sd(lc[,2]) # This step is included in GPmodel (c++ 2.8 version)
        lc = lc[idx,]
        n.obs = nrow(lc)
        A.lc = max(lc[,2]) - min(lc[,2])
        A.lc.9 = quantile(lc[,2], 0.9) - quantile(lc[,2], 0.1)
    } else {
        F.true = t.freq
        F.peak = max.dat[1,1]
        Q.peak = max.dat[1,2]
        Q.base = as.numeric(quantile(dat[,2], 0.1))
        dQ.p1.base =  Q.peak - Q.base
        if (n.maxima > 1) {
            dQ.p1.p2 = Q.peak - max.dat[2,2]
        } else {
            dQ.p1.p2 = dQ.p1.base
        }
        theta.1 = exp(max.dat[1,3])
        theta.2 = exp(max.dat[1,4])
        
        lc = read.table(lf.lc)
        idx = abs(lc[,2] - mean(lc[,2])) < 3.*sd(lc[,2]) # This step is included in GPmodel (c++ 2.8 version)
        lc = lc[idx,]
        n.obs = nrow(lc)
        mean.mag = mean(lc[,2])
        phase = 2 * PI * F.peak * lc[,1]
        prior.gamma = matrix(0, nrow = 3, ncol = 1)
        prior.gamma[1,1] = mean.mag
        H = matrix(1, ncol = 3, nrow = n.obs) ## This is NOT the Hessian/local curvature. We do NOT include Hessian in the analysis any more, since it is not a good class indicator. H is the design matrix for the periodic component
        H[, 2] = cos(phase)
        H[, 3] = sin(phase)
        Kc = matrix(NA, n.obs, n.obs) ## Covariance matrix for the square exponential kernel & observational uncertainty
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
        posterior.sigma.a = tmp.1.inv[2,2]
        posterior.sigma.b = tmp.1.inv[3,3]

        tpl.mjd = seq(min(lc[,1]), max(lc[,1]), length = n.tpl)
        tpl.mjd = c(lc[,1], tpl.mjd)
        n.new.tpl = n.obs + n.tpl
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
        A.model = sqrt(posterior.gamma[2]^2 + posterior.gamma[3]^2) * 2
        sd.error = sd(lc[,2] - tpl.mag[1:n.obs])

        A.lc = max(lc[,2]) - min(lc[,2])
        A.lc.9 = quantile(lc[,2], 0.9) - quantile(lc[,2], 0.1)

        if (sample(1:500,1) == 1) {
            setEPS()
            f.eps = paste0(fig.dir,f.lc,'.eps')
            postscript(f.eps,width=12,height=12)
            par(mfrow=c(2,1))
            plot(dat[,1:2],pch=19,cex=0.2,main=f,xlab='Frequency',ylab='Q')
            lines(dat[,1:2])
            abline(v = F.peak, col=2)
            abline(v = F.true, col=4)
            abline(h = Q.base, col = 'grey')
            plot(lc[,1:2], ylim = c(max(tpl.mag)+0.3, min(tpl.mag)-0.3), xlab='MJD', ylab='I (mag)',
                 pch = 19, cex = 0.5)
            arrows(lc[,1],lc[,2]+lc[,3],lc[,1],lc[,2]-lc[,3],length=0,angle=90,code=3)
            lines(tpl.mjd[(n.obs+1):n.new.tpl], tpl.mag[(n.obs+1):n.new.tpl])
            dev.off()
        }
        
        log.theta.2 = log10(theta.2)
        if (!is.finite(log.theta.2)) log.theta.2 = -9999.99999
        if (theta.1 > 99999999.99999) theta.1 = 99999999.99999
        if (log.theta.2 > 999999999.99999) log.theta.2 = 999999999.99999
    }

    ts = sprintf('%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f',
        f.lc, F.true, F.peak, Q.peak, Q.base, dQ.p1.base, dQ.p1.p2, theta.1, log.theta.2, A.model, A.lc, A.lc.9,
        n.obs, sd.error, posterior.sigma.a, posterior.sigma.b, posterior.mean.mag)
    write(ts,f.out,append=T)
    print(paste(i,f))
}


f.lst = paste0(lst.dir,'pe_',batch,'.lst')
lst = read.table(f.lst)
fs.lc = as.character(lst[,1])
nfs.lc = length(fs.lc)

for (i in 1:nfs.lc) {
    try(do.extract(i), silent=T)
}

