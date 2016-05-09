set.seed(101)
library(randomForest)

outdir = '~/Work/m33_phaseII/mira_cata_nb/'
figdir = paste0(outdir,'co_figs/')

if (T) {
f.ftr = '~/Work/m33_phaseII/rf_model_nb/nb_features.dat'
ftr = read.table(f.ftr, header = T)
f.csv = '~/Work/m33_phaseII/lmc_ofiles/mira.cat.csv'
csv = read.csv(f.csv)
ftr[,1] = as.character(ftr[,1])
csv[,1] = as.character(csv[,1])
csv[,'Spectr'] = as.character(csv[,'Spectr'])

idx = substr(ftr[,1],1,3) != 'mir' & substr(ftr[,1],1,3) != 'con' & substr(ftr[,1],1,3) != 'srv'
dat = ftr[idx,]

idx = substr(ftr[,1],1,4) == 'mira'
sim = ftr[idx,]
n.sim = nrow(sim)
sim.ids = substr(sim[,1],6,10)

sim.class = rep(NA, n.sim)
idx = csv[,'Spectr'] == 'C-rich'
c.ids = gsub('OGLE-LMC-LPV-','',csv[idx,1])
idx = sim.ids %in% c.ids
sim.class[idx] = 'C'
idx = csv[,'Spectr'] == 'O-rich'
o.ids = gsub('OGLE-LMC-LPV-','',csv[idx,1])
idx = sim.ids %in% o.ids
sim.class[idx] = 'O'
class = sim.class
sim = cbind(sim, class)
}

ntree = 500
n.fold = 5 ## Five-fold cross validation
cut.pos = round(n.sim/n.fold) * (1:n.fold)
cut.pos[n.fold] = n.sim
cut.pos = c(0, cut.pos)
for (i.fold in 1:n.fold) {
    print(paste('  RF for fold',i.fold))
    test.idx = (cut.pos[i.fold]+1):cut.pos[i.fold+1]
    test.set = sim[test.idx,]
    train.set = sim[-test.idx,]
    rf.model = randomForest(class ~ . - ID - F.true - i.mag,
        data = train.set, importance=T, ntree=ntree, do.trace=T)
    pred.test = predict(rf.model, test.set, type='prob')
    pred.data = cbind(test.set, pred.test)
    if (i.fold == 1) {
        out = pred.data
    } else {
        out = rbind(out, pred.data)
    }
    plot(rf.model)
}

f.out = paste0(outdir,'co_cross_validation.dat')
ts = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m  C_prob   O_prob'
write(ts, f.out)
fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f%10.3f%10.3f'
out2 = out[,-c(22)]
out2 = do.call('sprintf', c(fmt, out2))
write(out2, f.out, append=T)


true.cls = out[,'class']
pred.cls = rep(0, nrow(out))
by = 0.005
prob.cuts = seq(0, 1-by, by=by)
n.cuts = length(prob.cuts)
rec.rate = purity = rep(NA, n.cuts)
for (i in 1:n.cuts) {
    msg = paste0('   >> computing recovery rate: ',round(100*i/n.cuts), ' %     \r')
    message(msg, appendLF=F)
    prob.cut = prob.cuts[i]
    idx = which(out[,'O'] > prob.cut)
    pred.cls = rep('C', nrow(out))
    pred.cls[idx] = 'O'
    rec.rate[i] = sum(true.cls == 'O' & pred.cls == 'O') / sum(true.cls == 'O')
    purity[i] = sum(true.cls == 'O' & pred.cls == 'O') / sum(pred.cls == 'O')
}
print('')


f.rec = paste0(outdir,'co_rec_rate.csv')
rdat = cbind(prob.cuts, rec.rate, purity)
write.table(round(rdat,4), f.rec, row.names=F, quote=T, sep=',')

setEPS()
f.eps = paste0(figdir,'co_rec_rate.eps')
postscript(f.eps, width=8, height=8*0.618)
plot(rec.rate, purity, type='l', xlab='Recovery Rate', ylab='Purity')
idx = which(prob.cuts == 0.5)
points(rec.rate[idx], purity[idx], col=2, pch=19)
text(rec.rate[idx], purity[idx]+0.03, 'Probability Cut = 0.5', col=2)
dev.off()

setEPS()
f.eps = paste0(figdir,'co_var_import.eps')
postscript(f.eps, width=12, height=12*0.618)
train.set = sim
rf.model = randomForest(class ~ . - ID - F.true - i.mag,
    data = train.set, importance=T, ntree=ntree, do.trace=T)
varImpPlot(rf.model)
dev.off()

f.imp = paste0(outdir, 'co_importance.dat')
write.table(round(importance(rf.model),4), f.imp, quote=F, sep='   ')

n.dat = nrow(dat)
class = rep('N', n.dat)
m33 = cbind(dat, class)
pred = predict(rf.model, m33, type='prob')
out = cbind(dat, pred)

f.m33 = paste0(outdir, 'co_predictions.dat')
ts = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m  C_prob   O_prob'
write(ts, f.m33)
fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f%10.3f%10.3f'
out2 = do.call('sprintf', c(fmt, out))
write(out2, f.m33, append=T)
