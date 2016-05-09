f.dat = '~/Work/m33_phaseII/MasseyVI/m33_cad_vi.dat'
dat = read.table(f.dat, header=T)
dat[,1] = as.character(dat[,1])

pcuts = c(0.9, 0.95, 0.999)

figdir = '~/Work/m33_phaseII/MasseyVI/figs/'
f.eps = paste0(figdir,'pw_pl.eps')
setEPS()
postscript(f.eps, width=9, height=12)

par(mfrow=c(3,1), mar=c(5,5,1,1))
for (pcut in pcuts) {
idx = dat[,'Scaled_prob'] > pcut & dat[,'Ms_V'] > 0 & dat[,'F.peak'] != -1
sub = dat[idx,]

if (F) {
f.co = '~/Work/m33_phaseII/mira_cata_nb/co_predictions.dat'
co = read.table(f.co, header=T)
co[,1] = as.character(co[,1])
co[,1] = gsub('.slc','',co[,1])

idx = co[,'C_prob'] > 0.5
c.ids = co[idx,1]

idx = co[,'O_prob'] > 0.5
o.ids = co[idx,1]
}

idx = sub[,1] %in% c.ids
cdat = sub[idx,]

idx = sub[,1] %in% o.ids
odat = sub[idx,]

red = 1.55
ps = 1/odat[,2]
ws = odat[,'lc.mag'] - red*(odat[,'Ms_V'] - odat[,'lc.mag'])
## ws = odat[,'Ms_I'] - 1.55*(odat[,'Ms_V'] - odat[,'Ms_I'])
## ws = odat[,'i.mag'] - 1.55*(odat[,'Ms_V'] - odat[,'i.mag'])
plot(log10(ps), ws, pch=19, col=4, xlab='log P', ylab='W (mag)', ylim=c(22, 13.5),xlim=c(1.9,3.3), cex.lab=1.5, cex.axis=1.5)
tx = paste0('Prob cut = ',pcut)
text(1.9, 14, tx, adj=0, cex=2)

ps = 1/cdat[,2]
ws = cdat[,'lc.mag'] - red*(cdat[,'Ms_V'] - cdat[,'lc.mag'])
## ws = cdat[,'Ms_I'] - 1.55*(cdat[,'Ms_V'] - cdat[,'Ms_I'])
## ws = cdat[,'i.mag'] - 1.55*(cdat[,'Ms_V'] - cdat[,'i.mag'])
points(log10(ps), ws, pch=19, col=2)
}
dev.off()





f.eps = paste0(figdir,'pi_pl.eps')
setEPS()
postscript(f.eps, width=9, height=12)

par(mfrow=c(3,1), mar=c(5,5,1,1))
for (pcut in pcuts) {
idx = dat[,'Scaled_prob'] > pcut & dat[,'F.peak'] != -1
sub = dat[idx,]

idx = sub[,1] %in% c.ids
cdat = sub[idx,]

idx = sub[,1] %in% o.ids
odat = sub[idx,]

red = 1.55
ps = 1/odat[,2]
ws = odat[,'lc.mag'] #- red*(odat[,'Ms_V'] - odat[,'lc.mag'])
## ws = odat[,'Ms_I'] - 1.55*(odat[,'Ms_V'] - odat[,'Ms_I'])
## ws = odat[,'i.mag'] - 1.55*(odat[,'Ms_V'] - odat[,'i.mag'])
plot(log10(ps), ws, pch=1, col=4, xlab='log P', ylab='I (mag)', ylim=c(22, 18),xlim=c(1.9,3.3), cex.lab=1.5, cex.axis=1.5)
tx = paste0('Prob cut = ',pcut)
text(1.9, 18.2, tx, adj=0, cex=2)

ps = 1/cdat[,2]
ws = cdat[,'lc.mag'] #- red*(cdat[,'Ms_V'] - cdat[,'lc.mag'])
## ws = cdat[,'Ms_I'] - 1.55*(cdat[,'Ms_V'] - cdat[,'Ms_I'])
## ws = cdat[,'i.mag'] - 1.55*(cdat[,'Ms_V'] - cdat[,'i.mag'])
points(log10(ps), ws, pch=1, col=2)
}
dev.off()



f.eps = paste0(figdir,'pi_pl_less.eps')
setEPS()
postscript(f.eps, width=9, height=12)

par(mfrow=c(3,1), mar=c(5,5,1,1))
for (pcut in pcuts) {
idx = dat[,'Scaled_prob'] > pcut & dat[,'F.peak'] != -1 & dat[,'Ms_V'] > 0 
sub = dat[idx,]

idx = sub[,1] %in% c.ids
cdat = sub[idx,]

idx = sub[,1] %in% o.ids
odat = sub[idx,]

red = 1.55
ps = 1/odat[,2]
ws = odat[,'lc.mag'] #- red*(odat[,'Ms_V'] - odat[,'lc.mag'])
## ws = odat[,'Ms_I'] - 1.55*(odat[,'Ms_V'] - odat[,'Ms_I'])
## ws = odat[,'i.mag'] - 1.55*(odat[,'Ms_V'] - odat[,'i.mag'])
plot(log10(ps), ws, pch=19, col=4, xlab='log P', ylab='I (mag)', ylim=c(22, 18),xlim=c(1.9,3.3), cex.lab=1.5, cex.axis=1.5)
tx = paste0('Prob cut = ',pcut)
text(1.9, 18.2, tx, adj=0, cex=2)

ps = 1/cdat[,2]
ws = cdat[,'lc.mag'] #- red*(cdat[,'Ms_V'] - cdat[,'lc.mag'])
## ws = cdat[,'Ms_I'] - 1.55*(cdat[,'Ms_V'] - cdat[,'Ms_I'])
## ws = cdat[,'i.mag'] - 1.55*(cdat[,'Ms_V'] - cdat[,'i.mag'])
points(log10(ps), ws, pch=19, col=2)
}
dev.off()


f.eps = paste0(figdir,'pi_pl_post_i.eps')
setEPS()
postscript(f.eps, width=9, height=12)

par(mfrow=c(3,1), mar=c(5,5,1,1))
for (pcut in pcuts) {
idx = dat[,'Scaled_prob'] > pcut & dat[,'F.peak'] != -1
sub = dat[idx,]

idx = sub[,1] %in% c.ids
cdat = sub[idx,]

idx = sub[,1] %in% o.ids
odat = sub[idx,]

red = 1.55
ps = 1/odat[,2]
## ws = odat[,'lc.mag'] - red*(odat[,'Ms_V'] - odat[,'lc.mag'])
## ws = odat[,'Ms_I'] - 1.55*(odat[,'Ms_V'] - odat[,'Ms_I'])
ws = odat[,'i.mag'] # - 1.55*(odat[,'Ms_V'] - odat[,'i.mag'])
plot(log10(ps), ws, pch=1, col=4, xlab='log P', ylab='Posterior I (mag)', ylim=c(22, 18),xlim=c(1.9,3.3), cex.lab=1.5, cex.axis=1.5)
tx = paste0('Prob cut = ',pcut)
text(1.9, 18.2, tx, adj=0, cex=2)

ps = 1/cdat[,2]
## ws = cdat[,'lc.mag'] - red*(cdat[,'Ms_V'] - cdat[,'lc.mag'])
## ws = cdat[,'Ms_I'] - 1.55*(cdat[,'Ms_V'] - cdat[,'Ms_I'])
ws = cdat[,'i.mag']# - 1.55*(cdat[,'Ms_V'] - cdat[,'i.mag'])
points(log10(ps), ws, pch=1, col=2)
}
dev.off()
