set.seed(101)

outdir = '~/Work/m33_phaseII/rf_model_trackID/'
oodir = '~/Work/m33_phaseII/rf_model_nb/'
figdir = paste0(outdir, 'figs/')
system(paste0('mkdir -p ',figdir))

if (T) {
library(randomForest)
f.ftr = paste0(oodir, 'nb_features.dat')
ftr = read.table(f.ftr, header=T)
ftr[,1] = as.character(ftr[,1])
n.ftr = nrow(ftr)
idx = sample(1:n.ftr)
ftr = ftr[idx,]

class = rep('U', n.ftr)
idx = which(substr(ftr[,1],1,4) == 'con_')
class[idx] = 'N'
idx = which(substr(ftr[,1],1,4) == 'srv_')
class[idx] = 'N'
idx = which(substr(ftr[,1],1,5) == 'mira_')
class[idx] = 'M'

sim = ftr[class!='U',]
dat = ftr[class=='U',]
n.sim = nrow(sim)
idx = sample(1:n.sim, replace=F)
sim = sim[idx,]

sim.class = rep('N', n.sim)
idx = which(substr(sim[,1],1,5) == 'mira_')
sim.class[idx] = 'Y'
sim = cbind(sim, sim.class)

idx = sim.class == 'Y'
mir = sim[idx,]
non = sim[!idx,]
tids = substr(mir[,1],6,10)
tids = as.numeric(tids)
idx = order(tids)
mir = mir[idx,]

}

n.mir = nrow(mir)
n.non = nrow(non)
sim = rbind(mir[1:(n.mir/2),], non[1:(n.non/2),],mir[(n.mir/2+1):n.mir,], non[(n.non/2+1):n.non,])

n.fold = 2 ## Five-fold cross validation
cut.pos = round(n.sim/n.fold) * (1:n.fold)
cut.pos[n.fold] = n.sim
cut.pos = c(0, cut.pos)

ntree = 200
for (i.fold in 1:n.fold) {
    print(paste('  RF for fold',i.fold))
    test.idx = (cut.pos[i.fold]+1):cut.pos[i.fold+1]
    test.set = sim[test.idx,]
    train.set = sim[-test.idx,]
    rf.model = randomForest(sim.class ~ . - ID - F.true - i.mag,
        data = train.set, importance=T, ntree=ntree, do.trace=T, sampsize=c(60000,1250))
    pred.test = predict(rf.model, test.set, type='prob')
    pred.data = cbind(test.set, pred.test)
    if (i.fold == 1) {
        out = pred.data
    } else {
        out = rbind(out, pred.data)
    }
    ## plot(rf.model)
}


## f.out = paste0(outdir,'cross_validation.dat')
## ts = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m  N_prob   Y_prob'
## write(ts, f.out)
## fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f%10.3f%10.3f'
## out2 = out[,-c(22)]
## out2 = do.call('sprintf', c(fmt, out2))
## write(out2, f.out, append=T)


true.cls = out[,'sim.class']
pred.cls = rep(0, nrow(out))
by = 0.005
prob.cuts = seq(0, 1-by, by=by)
n.cuts = length(prob.cuts)
rec.rate = purity = rep(NA, n.cuts)
for (i in 1:n.cuts) {
    msg = paste0('   >> computing recovery rate: ',round(100*i/n.cuts), ' %     \r')
    message(msg, appendLF=F)
    prob.cut = prob.cuts[i]
    idx = which(out[,'Y'] > prob.cut)
    pred.cls = rep('N', nrow(out))
    pred.cls[idx] = 'Y'
    rec.rate[i] = sum(true.cls == 'Y' & pred.cls == 'Y') / sum(true.cls == 'Y')
    purity[i] = sum(true.cls == 'Y' & pred.cls == 'Y') / sum(pred.cls == 'Y')
}
print('')

f.rec = paste0(outdir,'rf_rec_rate_tid.csv')
rdat = cbind(prob.cuts, rec.rate, purity)
write.table(round(rdat,4), f.rec, row.names=F, quote=T, sep=',')

setEPS()
f.eps = paste0(figdir,'rec_rate_tid.eps')
postscript(f.eps, width=8, height=8*0.618)
plot(rec.rate, purity, type='l', xlab='Recovery Rate', ylab='Purity')
idx = which(prob.cuts == 0.5)
points(rec.rate[idx], purity[idx], col=2, pch=19)
text(rec.rate[idx], purity[idx]+0.03, 'Probability Cut = 0.5', col=2)
dev.off()

setEPS()
f.eps = paste0(figdir,'rf_var_import_tid.eps')
postscript(f.eps, width=12, height=12*0.618)
train.set = sim
rf.model = randomForest(sim.class ~ . - ID - F.true - i.mag,
    data = train.set, importance=T, ntree=ntree, do.trace=T, sampsize=c(120000,2500))
varImpPlot(rf.model)
dev.off()

## if (F) {
## print('   >> Predicting classes for all the stars')
## sim.class = rep('A', nrow(ftr))
## idx = which(substr(ftr[,1],1,4) == 'con_')
## sim.class[idx] = 'N'
## idx = which(substr(ftr[,1],1,4) == 'srv_')
## sim.class[idx] = 'N'
## idx = which(substr(ftr[,1],1,5) == 'mira_')
## sim.class[idx] = 'Y'
## adat = cbind(ftr, sim.class)
## pred = predict(rf.model, adat, type='prob')

## print('   >> Write the results to files')
## out = cbind(ftr, pred)
## f.out = paste0(outdir,'all_predictions.dat')
## ts = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m  N_prob   Y_prob'
## write(ts, f.out)
## fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f%10.3f%10.3f'
## out2 = do.call('sprintf', c(fmt, out))
## write(out2, f.out, append=T)


## f.imp = paste0(outdir, 'importance.dat')
## write.table(round(importance(rf.model),4), f.imp, quote=F, sep='   ')

## idx = sim.class == 'A'
## m33 = out[idx,]
## f.m33 = paste0(outdir, 'm33_predictions.dat')
## ts = '                    ID                F.true    F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.model  A.lc   A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m  N_prob   Y_prob'
## write(ts, f.m33)
## fmt = '%35s%11.6f%10.6f%10.3f%9.3f%9.3f%8.3f%15.5f%16.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f%10.3f%10.3f'
## m332 = do.call('sprintf', c(fmt, m33))
## write(m332, f.m33, append=T)
## }
