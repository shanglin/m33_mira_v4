set.seed(101)

outdir = '~/Work/m33_phaseII/rf_model_nc/'
figdir = paste0(outdir, 'figs/')

if (T) {
library(randomForest)
f.ftr = paste0(outdir, 'mnm_features.dat')
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
}

n.fold = 5 ## Five-fold cross validation
cut.pos = round(n.sim/n.fold) * (1:n.fold)
cut.pos[n.fold] = n.sim
cut.pos = c(0, cut.pos)

ntree = 200
ncore = 5
library(doParallel)
cl = makeCluster(ncore)
registerDoParallel(cl)
for (i.fold in 1:n.fold) {
    print(paste('  RF for fold',i.fold))
    test.idx = (cut.pos[i.fold]+1):cut.pos[i.fold+1]
    test.set = sim[test.idx,]
    train.set = sim[-test.idx,]
    rf.model = foreach(sub_ntree = rep(round(ntree/ncore), ncore), .combine=combine, .multicombine=TRUE,
        .packages='randomForest') %dopar% {
            randomForest(sim.class ~ . - ID - t.test - F.true,
                         data = train.set, importance=T, ntree=sub_ntree)
        }
    pred.test = predict(rf.model, test.set, type='prob')
    pred.data = cbind(test.set, pred.test)
    if (i.fold == 1) {
        out = pred.data
    } else {
        out = rbind(out, pred.data)
    }
}

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

f.rec = paste0(outdir,'rf_rec_rate.csv')
rdat = cbind(prob.cuts, rec.rate, purity)
write.table(round(rdat,4), f.rec, row.names=F, quote=T, sep=',')

setEPS()
f.eps = paste0(figdir,'rec_rate.eps')
postscript(f.eps, width=8, height=8*0.618)
plot(rec.rate, purity, type='l', xlab='Recovery Rate', ylab='Purity')
idx = which(prob.cuts == 0.5)
points(rec.rate[idx], purity[idx], col=2, pch=19)
text(rec.rate[idx], purity[idx]+0.03, 'Probability Cut = 0.5', col=2)
dev.off()

setEPS()
f.eps = paste0(figdir,'rf_var_import.eps')
postscript(f.eps, width=12, height=12*0.618)
train.set = sim
rf.model = foreach(sub_ntree = rep(round(ntree/ncore), ncore), .combine=combine, .multicombine=TRUE,
    .packages='randomForest') %dopar% {
        randomForest(sim.class ~ . - ID - t.test - F.true,
                     data = train.set, importance=T, ntree=sub_ntree)
    }
varImpPlot(rf.model)
dev.off()

sim.class = rep('A', nrow(ftr))
idx = which(substr(ftr[,1],1,4) == 'con_')
sim.class[idx] = 'N'
idx = which(substr(ftr[,1],1,4) == 'srv_')
sim.class[idx] = 'N'
idx = which(substr(ftr[,1],1,5) == 'mira_')
sim.class[idx] = 'Y'
adat = cbind(ftr, sim.class)
pred = predict(rf.model, adat, type='prob')
out = cbind(ftr, pred)
f.out = paste0(outdir,'all_predictions.dat')
ts = '                    ID                t.test    F.true   dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.lc   A.lc.9 n.obs sd.error  i.mag     bfM2S      bfS2C     bfGP2P     bfM2C     f.p1     l.p1    A.p1     f.p2     l.p2    A.p2  mean.sd qdr.rsd.sd ratio.q2m  N_prob   Y_prob'
write(ts, f.out)
fmt = '%35s%9.3f%11.6f%11.3f%9.3f%15.5f%16.5f%8.3f%8.3f%5i%8.3f%9.3f%11.4f%10.4f%11.4f%10.4f%10.6f%8.4f%8.3f%10.6f%8.4f%8.3f%8.3f%9.3f%10.3f%10.4f%10.4f'
out2 = do.call('sprintf', c(fmt, out))
write(out2, f.out, append=T)

stopCluster(cl)

f.imp = paste0(outdir, 'importance.dat')
write.table(round(importance(rf.model),4), f.imp, quote=F, sep='   ')

idx = sim.class == 'A'
m33 = out[idx,]
f.m33 = paste0(outdir, 'm33_predictions.dat')
ts = '                    ID                t.test    F.true   dQ.p1.base dQ.p1.p2      theta.1   log10.theta.2   A.lc   A.lc.9 n.obs sd.error  i.mag     bfM2S      bfS2C     bfGP2P     bfM2C     f.p1     l.p1    A.p1     f.p2     l.p2    A.p2  mean.sd qdr.rsd.sd ratio.q2m  N_prob   Y_prob'
write(ts, f.m33)
fmt = '%35s%9.3f%11.6f%11.3f%9.3f%15.5f%16.5f%8.3f%8.3f%5i%8.3f%9.3f%11.4f%10.4f%11.4f%10.4f%10.6f%8.4f%8.3f%10.6f%8.4f%8.3f%8.3f%9.3f%10.3f%10.4f%10.4f'
m332 = do.call('sprintf', c(fmt, m33))
write(m332, f.m33, append=T)
