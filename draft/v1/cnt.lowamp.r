f.dat = '~/Work/m33_phaseII/rf_model_sampsize/m33_predictions.dat'
dat = read.table(f.dat, header=T, stringsAsFactors=F)
idx = dat[,'Y_prob'] > 0.5
sub = dat[idx,]
ts = paste0('All prob > 0.5: ', nrow(sub))
print(ts)

idx = sub[,'A.model'] > 0.6
ts = paste0('Model Amp > 0.6 mag: ', sum(idx))
print(ts)

sub = sub[idx,]
idx = sub[,'Y_prob'] < 1
pea = sub[idx,]
p = 1/pea[,'F.peak']
hist(p, breaks=300)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

peak = Mode(round(p,1))
abline(v=peak, col=2)
ts = paste0('Peaks at ', peak, ' days')
print(ts)

f.co = '~/Work/m33_phaseII/mira_cata_sampsize/cadco.csv'
co = read.csv(f.co)
ts = paste0('Number of Mira candidates: ',nrow(co))
print(ts)
idx = co[,'O_prob'] > 0.5
ts = paste0('Number of O-rich Miras: ', sum(idx))
print(ts)
idx = co[,'O_prob'] <= 0.5
ts = paste0('Number of C-rich Miras: ', sum(idx))
print(ts)

