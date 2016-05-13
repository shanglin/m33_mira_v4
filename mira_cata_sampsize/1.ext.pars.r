set.seed(101)

f.cad = '~/Work/m33_phaseII/rf_model_sampsize/m33_mira_candidate.dat'
outdir = '~/Work/m33_phaseII/mira_cata_sampsize/'
pngdir = '~/Work/m33_phaseII/rf_model_sampsize/lc_pngs/'
htldir = '~/Work/meetings/may12_2016/htmls/'
lcdir = '~/Work/m33_phaseII/m33_v4_lcs/'
newdir = '~/Work/meetings/may12_2016/figs_2/'
figdir = '~/Work/m33_phaseII/mira_cata_sampsize/figs/'
fig.dir = figdir
dat = read.table(f.cad, header=T)
dat[,1] = as.character(dat[,1])
idx = rev(order(dat[,'Scaled_prob']))
dat = dat[idx,]
n.dat = nrow(dat)

cpimg = function(f.eps) {
    f.png = gsub(figdir, newdir, f.eps)
    f.png = gsub('.eps','.png',f.png)
    f.png = gsub('.pdf','.png',f.png)
    cmd = paste0('convert ',f.eps,' ', f.png)
    system(cmd)
}
col = 'skyblue'
breaks = 100
width = 12
height = 12
cex = 1.3
mar = c(5,5,3,3)

f.eps = paste0(fig.dir, 'sigasigb.eps')
setEPS()
postscript(f.eps, width=width, height=height/2)
par(mfrow = c(1,2))
plot(dat[,'Scaled_prob'], dat[,'sig.a'], pch=19, cex=0.5, xlab='Scaled P(Mira)', ylab='Sigma a')
sig.a.cut = 0.25
sig.b.cut = 0.25
abline(h=sig.a.cut, col=2)
plot(dat[,'Scaled_prob'], dat[,'sig.b'], pch=19, cex=0.5, xlab='Scaled P(Mira)', ylab='Sigma b')
abline(h=sig.b.cut, col=2)
dev.off()
cpimg(f.eps)

f.html = paste0(htldir, 'sig_a_order.html')
ts = '<html><body>'
write(ts, f.html)
idx = rev(order(dat[,'sig.a']))
sub = dat[idx[1:300],]
for (i in 1:nrow(sub)) {
    id = sub[i,1]
    f.png = gsub('.slc','.png',id)
    lf.png = paste0('../lc_pngs/',f.png)
    ts = paste0('<h2> Sigma a = ',sub[i,'sig.a'],' , Sigma b = ',sub[i,'sig.b'],'</h2>')
    write(ts, f.html, append=T)
    ts = paste0('<img src="',lf.png,'" width=90%><br>')
    write(ts, f.html, append=T)
}
ts = '</body>'
write(ts, f.html, append=T)


f.html = paste0(htldir, 'sig_b_order.html')
ts = '<html><body>'
write(ts, f.html)
idx = rev(order(dat[,'sig.b']))
sub = dat[idx[1:300],]
for (i in 1:nrow(sub)) {
    id = sub[i,1]
    f.png = gsub('.slc','.png',id)
    lf.png = paste0('../lc_pngs/',f.png)
    ts = paste0('<h2> Sigma b = ',sub[i,'sig.b'],' , Sigma a = ',sub[i,'sig.a'],'</h2>')
    write(ts, f.html, append=T)
    ts = paste0('<img src="',lf.png,'" width=90%><br>')
    write(ts, f.html, append=T)
}
ts = '</body>'
write(ts, f.html, append=T)

## idx = dat[,'sig.a'] < sig.a.cut & dat[,'sig.b'] < sig.b.cut
## dat = dat[idx,]



############ Run random forest on M33 data
n.dat = nrow(dat)
idx = sample(1:n.dat)
dat = dat[idx,]
trn = dat

library(randomForest)
ntree = 10
rf.model = randomForest(Scaled_prob ~ . - ID - F.true - i.mag - Y_prob - n.obs,
    data = trn, do.trace=T, ntree=ntree)
pred = predict(rf.model, trn)
pred = pred - min(pred)
pred = pred / max(pred)
plot(trn[,'Scaled_prob'],pred)

for (foo in 1:20) {
    trn[,'Scaled_prob'] = pred
    rf.model = randomForest(Scaled_prob ~ . - ID - F.true - i.mag - Y_prob - n.obs,
    data = trn, do.trace=F, ntree=ntree)
    pred = predict(rf.model, trn)
    pred = pred - min(pred)
    pred = pred / max(pred)
    plot(trn[,'Scaled_prob'],pred)
    print(foo)
}

pred = pred - min(pred)
pred = pred / max(pred)
trn[,'Scaled_prob'] = round(pred,4)
idx = rev(order(pred))
trn = trn[idx,]
## sub = trn[1:500,]
sub = trn
f.html = paste0(htldir, 'rf_regression_order.html')
ts = '<html><body>'
write(ts, f.html)
for (i in 1:nrow(sub)) {
    id = sub[i,1]
    f.png = gsub('.slc','.png',id)
    lf.png = paste0('../lc_pngs/',f.png)
    ts = paste0('<h2> pred = ',trn[i,'Scaled_prob'],'</h2>')
    write(ts, f.html, append=T)
    ts = paste0('<img src="',lf.png,'" width=90%><br>')
    write(ts, f.html, append=T)
}
ts = '</body>'
write(ts, f.html, append=T)

ids = trn[,1]
f.ids = '~/Site/m33_v4/inst_mira/cadids.dat'
write.table(ids, f.ids, quote=F, col.names=F, row.names=F)


## hist(pred)
## idx = trn[,'Scaled_prob'] < 0.4
## bad.ids = trn[idx,1]
## idx = trn[,'Scaled_prob'] > 0.8
## god.ids = trn[idx,1]
## idx = trn[,'Scaled_prob'] <= 0.8 & trn[,'Scaled_prob'] >= 0.4
## mid.ids = trn[idx,1]


## sitdir = paste0('~/Site/m33_v4/inst_mira/','god/')
## cpdir = paste0(sitdir,'/pngs/')
## cmd = paste0('mkdir -p ',sitdir)
## system(cmd)
## cmd = paste0('mkdir -p ',cpdir)
## system(cmd)
## for (i in 1:length(god.ids)) {
##     id = god.ids[i]
##     f.png = gsub('.slc','.png', id)
##     lf.png = paste0(pngdir, f.png)
##     of.png = paste0(cpdir, f.png)
##     cmd = paste0('cp ',lf.png,' ',of.png)
##     system(cmd)
## }


## sitdir = paste0('~/Site/m33_v4/inst_mira/','bad/')
## cpdir = paste0(sitdir,'/pngs/')
## cmd = paste0('mkdir -p ',sitdir)
## system(cmd)
## cmd = paste0('mkdir -p ',cpdir)
## system(cmd)
## for (i in 1:length(bad.ids)) {
##     id = bad.ids[i]
##     f.png = gsub('.slc','.png', id)
##     lf.png = paste0(pngdir, f.png)
##     of.png = paste0(cpdir, f.png)
##     cmd = paste0('cp ',lf.png,' ',of.png)
##     system(cmd)
## }


## sitdir = paste0('~/Site/m33_v4/inst_mira/','mid/')
## cpdir = paste0(sitdir,'/pngs/')
## cmd = paste0('mkdir -p ',sitdir)
## system(cmd)
## cmd = paste0('mkdir -p ',cpdir)
## system(cmd)
## for (i in 1:length(mid.ids)) {
##     id = mid.ids[i]
##     f.png = gsub('.slc','.png', id)
##     lf.png = paste0(pngdir, f.png)
##     of.png = paste0(cpdir, f.png)
##     cmd = paste0('cp ',lf.png,' ',of.png)
##     system(cmd)
## }

