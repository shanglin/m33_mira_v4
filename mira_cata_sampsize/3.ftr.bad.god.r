dir = '~/Work/m33_phaseII/mira_cata_sampsize/'
lcdir = '~/Work/m33_phaseII/m33_v4_lcs/'
rfdir = '~/Work/m33_phaseII/rf_model_sampsize/'
figdir = paste0(dir,'figs_2/')
newdir = '~/Work/meetings/may16_2016/figs_2/'

f.res = paste0(dir, 'vis_result.csv')
f.cad = paste0(rfdir, 'm33_mira_candidate.dat')

PI = 3.1415926536
seps = c(0,500,800,1200,2000) # blending effects only for the smaller telescopes, which observed prior to MJD=2000

res = read.table(f.res, sep=',')
res[,1] = as.character(res[,1])
res[,2] = as.character(res[,2])
res[,1] = gsub('_png','.slc',res[,1])

cad = read.table(f.cad, header=T)
cad[,1] = as.character(cad[,1])

idx = match(res[,1], cad[,1])
cad = cad[idx,]
n.cad = nrow(cad)


cpimg = function(f.eps) {
    f.png = gsub(figdir, newdir, f.eps)
    f.png = gsub('.eps','.png',f.png)
    f.png = gsub('.pdf','.png',f.png)
    cmd = paste0('convert -density 300 ',f.eps,' ', f.png)
    system(cmd)
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

if (F) {
mean.sd.s = qdr.rsd.sd.s = ratio.q2m.s = psa = ncy = phase.dist = rep(NA, n.cad)
for (i in 1:n.cad) {
    f.lc = cad[i,1]
    lf.lc = paste0(lcdir, f.lc)
    lc = read.table(lf.lc)
    idx = abs(lc[,2] - mean(lc[,2])) < 3.*sd(lc[,2]) # This step is included in GPmodel (c++ 2.8 version)
    lc = lc[idx,]
    idx = order(lc[,1])
    lc = lc[idx,]
    x = lc[,1]
    y = lc[,2]
    e = lc[,3]

    grp.sds = rep(NA, 4)
    all.residuals = c()
    no.data = T
    
    for (i.grp in 1:4) {
        idx = x > seps[i.grp] & x < seps[i.grp+1]
        if (sum(idx) > 5) {
            grp = lc[idx,]
            grp.sds[i.grp] = sd(grp[,2])
            all.residuals = c(all.residuals, qdr.fit.res(grp))
            no.data = F
        }
    }

    if (no.data == F) {
        idx = !is.na(grp.sds)
        grp.sds = grp.sds[idx]
        mean.sd.s[i] = mean(grp.sds)
        qdr.rsd.sd.s[i] = sd(all.residuals)
        ratio.q2m.s[i] = qdr.rsd.sd.s[i] / mean.sd.s[i]
    } else {
        mean.sd.s[i] = -1
        qdr.rsd.sd.s[i] = -1
        ratio.q2m.s[i] = -1
    }

    ##
    period = 1/cad[i,3]
    psa[i] = max(cad[i,'sig.a'], cad[i,'sig.b'])
    ##
    ncy[i] = (max(x) - min(x)) / period

    msg = paste0('    >>',round(100*i/n.cad,2),' %     \r')
    message(msg, appendLF=F)
}

print('')
}


prob = cad[,'Scaled_prob']
dat = cbind(res[,1:2], mean.sd.s, qdr.rsd.sd.s, ratio.q2m.s, psa, ncy, prob)

idx = dat[,2] == 'Y'

f.pdf = paste0(figdir,'q2m_sp.pdf')
pdf(f.pdf, width=6, height=6)
xlab = 'Scaled Probability'
ylab = 'Ratio of quadratic fit for smaller telescope only'
plot(dat[idx,'prob'], dat[idx, 'ratio.q2m.s'], pch=19, cex=0.5, col = rgb(1,0,0,0.3), xlab=xlab, ylab=ylab)
points(dat[!idx,'prob'], dat[!idx, 'ratio.q2m.s'], cex=0.5, col = rgb(0,0,0,0.3))
legend(0.1,-0.5,c('Kept Mira candidates','Rejected Mira candidates'), pch=c(19,1), col=c(2,1))
dev.off()
cpimg(f.pdf)


f.pdf = paste0(figdir,'sigmaab_sp.pdf')
pdf(f.pdf, width=6, height=6)
xlab = 'Scaled Probability'
ylab = 'log (GP uncertainty of amplitude)'
plot(dat[idx,'prob'], log10(dat[idx, 'psa']), pch=19, cex=0.5, col = rgb(1,0,0,0.3), xlab=xlab, ylab=ylab, ylim=c(-3,0.9))
points(dat[!idx,'prob'], log10(dat[!idx, 'psa']), cex=0.5, col = rgb(0,0,0,0.3))
## legend(0.7,0.3,c('Kept Mira candidates','Rejected Mira candidates'), pch=c(19,1), col=c(2,1))
dev.off()
cpimg(f.pdf)


f.pdf = paste0(figdir,'ncycle_sp.pdf')
pdf(f.pdf, width=6, height=6)
xlab = 'Scaled Probability'
ylab = '# cycle'
plot(dat[idx,'prob'], dat[idx, 'ncy'], pch=19, cex=0.5, col = rgb(1,0,0,0.3), xlab=xlab, ylab=ylab, xlim=c(0,1))
points(dat[!idx,'prob'], dat[!idx, 'ncy'], cex=0.5, col = rgb(0,0,0,0.3))
legend(0,25.2,c('Kept Mira candidates','Rejected Mira candidates'), pch=c(19,1), col=c(2,1))
dev.off()
cpimg(f.pdf)

