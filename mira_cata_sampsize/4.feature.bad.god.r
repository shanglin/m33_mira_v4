dir = '~/Work/m33_phaseII/mira_cata_sampsize/'
lcdir = '~/Work/m33_phaseII/m33_v4_lcs/'
rfdir = '~/Work/m33_phaseII/rf_model_sampsize/'
figdir = paste0(dir,'figs_2/')
newdir = '~/Work/meetings/may16_2016/figs_2/'

cpimg = function(f.eps) {
    f.png = gsub(figdir, newdir, f.eps)
    f.png = gsub('.eps','.png',f.png)
    f.png = gsub('.pdf','.png',f.png)
    cmd = paste0('convert -density 300 ',f.eps,' ', f.png)
    system(cmd)
}

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

if (F) {
scr = # sign change rate
    ## asd = # averaged standard deviation
    mpg = # maximum phase gap
    psa = # posterior uncertainty of amplitude
    ncy = # number of cycles covered by the observation
    rep(NA, n.cad)

rfsign = function (a, b, ea, eb) {
    if (abs(a - b) < 0.5*(ea + eb)) {
        ret = 0
    } else {
        ret = sign(b-a)
    }
    return(ret)
}

## idx = which(cad[,1] == 'wbi32724.slc')
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
    
    ## compute sign change rate
    ts.scr = 0
    ts.nobs = 0
    for (j in 1:4) {
        idx = which(x > seps[j] & x < seps[j+1])
        n.idx = length(idx)
        if (n.idx > 3) {
            sub = y[idx]
            sube = e[idx]
            current.sign = sign(sub[n.idx] - sub[1])
            for (k in 3:n.idx) {
                new.sign = rfsign(sub[k-1],sub[k],sube[k-1],sube[k])
                if (new.sign != current.sign & new.sign != 0) {
                    ts.scr = ts.scr + 1
                    current.sign = new.sign
                }
                ts.nobs = ts.nobs + 1
            }
        }
    }
    if (ts.nobs > 0) {
        scr[i] = ts.scr/ts.nobs
    } else {
        scr[i] = -1
    }

    ## compute maximum phase gap
    period = 1/cad[i,3]
    phase = (x / period) %% 1
    phase = phase[order(phase)]
    phase = c(phase, phase + 1)
    n.phase = length(phase)
    mpg[i] = max(phase[2:n.phase] - phase[1:(n.phase-1)])

    ##
    psa[i] = max(cad[i,'sig.a'], cad[i,'sig.b'])
    ##
    ncy[i] = (max(x) - min(x)) / period
    msg = paste0('    >>',round(100*i/n.cad,2),' %     \r')
    message(msg, appendLF=F)
    ## idx = x < 2000
    ## x = x[idx]
    ## y = y[idx]
    ## e = lc[idx,3]
    ## plot(x, y,pch=19)
    ## lines(x, y)
    ## arrows(x, y-e, x, y+e, code=3, length=0, angle=90)
    ## print(scr[i])
}
print('')
}


prob = cad[,'Scaled_prob']
dat = cbind(res[,1:2], scr, mpg, prob)
idx = dat[,2] == 'Y'

f.pdf = paste0(figdir,'mpg_sp.pdf')
pdf(f.pdf, width=6, height=6)
xlab = 'Scaled Probability'
ylab = 'Maximum phase gap'
plot(dat[idx,'prob'], dat[idx, 'mpg'], pch=19, cex=0.5, col = rgb(1,0,0,0.3), xlab=xlab, ylab=ylab)
points(dat[!idx,'prob'], dat[!idx, 'mpg'], cex=0.5, col = rgb(0,0,0,0.3))
## legend(0.1,-0.5,c('Kept Mira candidates','Rejected Mira candidates'), pch=c(19,1), col=c(2,1))
dev.off()
cpimg(f.pdf)


f.pdf = paste0(figdir,'scr_sp.pdf')
pdf(f.pdf, width=6, height=6)
ylab = 'magnitude increase/decrease change rate'
plot(dat[idx,'prob'], dat[idx, 'scr'], pch=19, cex=0.5, col = rgb(1,0,0,0.3), xlab=xlab, ylab=ylab)
points(dat[!idx,'prob'], dat[!idx, 'scr'], cex=0.5, col = rgb(0,0,0,0.3))
legend(0.1,-0.5,c('Kept Mira candidates','Rejected Mira candidates'), pch=c(19,1), col=c(2,1))
dev.off()
cpimg(f.pdf)

## f.html = '~/Work/meetings/may16_2016/kept.html'
## ts = '<html><body>'
## write(ts, f.html)
## ids = dat[idx,1]
## ids = gsub('.slc','.png',ids)
## idx = sample(1:length(ids))
## ids = ids[idx]
## for (id in ids) {
##     ts = paste0('<img src="../may12_2016/lc_pngs/',id,'" width=90%><br>')
##     write(ts, f.html, append=T)
## }
## ts = '</body></html>'
## write(ts, f.html, append=T)
ids = dat[idx,1]
ids = gsub('.slc','.png',ids)
for (id in ids) {
    cmd = paste0('cp ~/Work/meetings/may12_2016/lc_pngs/',id,' ~/Work/meetings/may16_2016/kept/')
    system(cmd)
}
     
ids = dat[!idx,1]
ids = gsub('.slc','.png',ids)
for (id in ids) {
    cmd = paste0('cp ~/Work/meetings/may12_2016/lc_pngs/',id,' ~/Work/meetings/may16_2016/rejected/')
    system(cmd)
}
     
