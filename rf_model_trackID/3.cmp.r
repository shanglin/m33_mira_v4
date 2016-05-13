dir = '~/Work/m33_phaseII/rf_model_trackID/'
f.trk = paste0(dir, 'rf_rec_rate_tid.csv')
trk = read.csv(f.trk)
f.rdm = paste0(dir, 'rf_rec_rate_rid.csv')
rdm = read.csv(f.rdm)

f.eps = paste0(dir, 'cmp.eps')
setEPS()
postscript(f.eps, width=8, height=8)
mar = c(5,5,3,3)
par(mfrow=c(2,1), mar=mar)
xlab = 'Recovery Rate'
ylab = 'Purity'
plot(trk[,2:3], type='l', col=2, lwd=2, xlab=xlab, ylab=ylab)
lines(rdm[,2:3], col=4, lty=2, lwd=2)
legend(0.1,0.8, c('Different OGLE ID for training and test','Random OGLE ID'), col=c(2,4), lty=c(1,2),lwd=2)
xlab = expression(paste(Delta, ' Recovery Rate'))
ylab = expression(paste(Delta, ' Purity'))
plot(trk[,2]-rdm[,2], trk[,3]-rdm[,3], pch=19, xlab=xlab, ylab=ylab)
dev.off()
