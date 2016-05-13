f.res = '~/Work/m33_phaseII/mira_cata_sampsize/vis_result.csv'
res = read.table(f.res, sep=',')
res[,1] = as.character(res[,1])
res = res[res[,2]=='Y',]
figdir = '~/Work/m33_phaseII/mira_cata_sampsize/figs/'
newdir = '~/Work/meetings/may12_2016/figs_2/'
lcdir = '~/Work/m33_phaseII/m33_v4_lcs/'
outdir = '~/Work/m33_phaseII/mira_cata_sampsize/'

cpimg = function(f.eps) {
    f.png = gsub(figdir, newdir, f.eps)
    f.png = gsub('.eps','.png',f.png)
    f.png = gsub('.pdf','.png',f.png)
    cmd = paste0('convert ',f.eps,' ', f.png)
    system(cmd)
}


## f.html = '~/Work/meetings/may12_2016/htmls/M33_mira_candidates.html'
ids = gsub('_png','.png',res[,1])
## ts = '<html><body>'
## write(ts, f.html)
## for (i in 1:nrow(res)) {
##     id = ids[i]
##     lf.png = paste0('../lc_pngs/',id)
##     ## ts = paste0('<h2> pred = ',trn[i,'Scaled_prob'],'</h2>')
##     ## write(ts, f.html, append=T)
##     ts = paste0('<img src="',lf.png,'" width=90%><br><hr>')
##     write(ts, f.html, append=T)
## }
## ts = '</body>'
## write(ts, f.html, append=T)



f.cad = '~/Work/m33_phaseII/rf_model_sampsize/m33_mira_candidate.dat'
dat = read.table(f.cad, header=T)
dat[,1] = as.character(dat[,1])
idx = dat[,1] %in% gsub('.png','.slc',ids)
cad = dat[idx,]



col = 'skyblue'
breaks = 100
width = 12
height = 12
cex = 1.3
mar = c(5,5,3,3)
xlim = c(0,2000)
breaks=50

############ period hist
## f.ftr = '~/Work/m33_phaseII/rf_model_nb/nb_features.dat'
## ftr = read.table(f.ftr, header=T)
## ftr[,1] = as.character(ftr[,1])
## idx = substr(ftr[,1],1,4) == 'mira'
## mir = ftr[idx,]
if (F) {
f.eps = paste0(figdir, 'periods_hist_2.eps')
setEPS()
postscript(f.eps, width=width, height=height/2)

par(mfrow=c(1,2))
hist(1/mir[,2], xlab='Period [day]', breaks=breaks/2, cex.lab=1.3, cex.axis=1.3, main='SIM Mira: True Period', xlim=xlim, col=col)
hist(1/cad[,3], xlab='Period [day]', breaks=breaks/2, cex.lab=1.3, cex.axis=1.3, main='M33 Candidate: Peak Period', xlim=xlim, col=col)
t3 = paste0('Total # = ',nrow(cad))
text(1000, 300, t3, col=1, adj=0,cex=1.5)
dev.off()
cpimg(f.eps)
}

########### PL relation (max light, min light, mean light)
if (F) {
n.cad = nrow(cad)
i.max = i.min = n.cyc = p.cover = rep(NA, n.cad)
PI = 3.1415926536
plength = 0.04
for (i in 1:n.cad) {
    f.lc = cad[i,1]
    lf.lc = paste0(lcdir, f.lc)
    lc = read.table(lf.lc)
    idx = abs(lc[,2] - mean(lc[,2])) < 3.*sd(lc[,2]) # This step is included in GPmodel (c++ 2.8 version)
    lc = lc[idx,]
    phase = 2 * PI * cad[i,3] * lc[,1]
    phase = (phase / (2*PI)) %% 1
    i.max[i] = max(lc[,2])
    i.min[i] = min(lc[,2])
    period = 1/cad[i,3]
    n.cyc[i] = round((max(lc[,1])-min(lc[,1]))/period, 2)
    phase = phase[order(phase)]
    ts.cover = min(plength, phase[1] + plength/2)
    for (j in 2:length(phase)) {
        if ((phase[j] - phase[j-1]) > plength) {
            ts.cover = ts.cover + plength
        } else {
            ts.cover = ts.cover + phase[j] - phase[j-1]
        }
    }
    p.cover[i] = round(ts.cover - max(0, phase[j] - 1 + plength/2), 3)
    print(i/n.cad)
}

cadmore = cbind(cad, i.max, i.min, n.cyc, p.cover)
}


# match C/O-rich predictions
if (T) {
f.co = '~/Work/m33_phaseII/mira_cata_nb/co_predictions.dat'
co = read.table(f.co, header=T)
idx = match(cadmore[,1], as.character(co[,1]))
O_prob = co[idx,'O_prob']
cadco = cbind(cadmore, O_prob)
}

idx = cadco[,'O_prob'] > 0.5
odat = cadco[idx,]
cdat = cadco[!idx,]

f.eps = paste0(figdir,'m33_pi_pl.eps')
setEPS()
postscript(f.eps, width=9, height=9)

par(mfrow=c(3,1), mar=c(5,5,1,1))
plot(log10(1/odat[,3]), odat[,'i.min'], pch=1, col=4, xlab='log P', ylab='I (max light)', ylim=c(21, 17),xlim=c(1.9,3.3), cex.lab=1.5, cex.axis=1.5, cex=0.5)
points(log10(1/cdat[,3]), cdat[,'i.min'], pch=1, col=2, cex=0.1)

plot(log10(1/odat[,3]), odat[,'i.mag'], pch=1, col=4, xlab='log P', ylab='I (mean light)', ylim=c(22, 18),xlim=c(1.9,3.3), cex.lab=1.5, cex.axis=1.5, cex=0.5)
points(log10(1/cdat[,3]), cdat[,'i.mag'], pch=1, col=2, cex=0.1)

plot(log10(1/odat[,3]), odat[,'i.max'], pch=1, col=4, xlab='log P', ylab='I (min light)', ylim=c(24, 20),xlim=c(1.9,3.3), cex.lab=1.5, cex.axis=1.5, cex=0.5)
points(log10(1/cdat[,3]), cdat[,'i.max'], pch=1, col=2, cex=0.1)

dev.off()
cpimg(f.eps)


########### Write the table
f.tbl = paste0(outdir,'cadco.csv')
write.table(cadco, f.tbl, sep=',', row.names=F, col.names=T, quote=F)


#### plt phase cover
f.eps = paste0(figdir,'phase_cover.eps')
setEPS()
postscript(f.eps, width=12, height=6)

par(mfrow=c(1,2), mar=c(5,5,1,1))
hist(cadco[,'p.cover'], main='', xlab='Phase coverage', col=col, cex.lab=1.5, cex.axis=1.5)
hist(cadco[,'n.cyc'], main='', xlab='# cycles', col=col, cex.lab=1.5, cex.axis=1.5)
dev.off()
cpimg(f.eps)
