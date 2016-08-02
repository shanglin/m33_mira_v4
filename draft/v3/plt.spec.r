dir = '~/Work/m33_phaseII/sc_get_spec/gp_spectra/'
outdir = '~/Work/m33_phaseII/draft/v3.0/figures/'
lcdir.m = '~/Work/m33_phaseII/sim_ofiles/flc_mira/'
lcdir.s = '~/Work/m33_phaseII/sim_ofiles/flc_srv/'
lcdir.c = '~/Work/m33_phaseII/sim_ofiles/flc_con/'


f.mspec = 'mira_19152_wii12057_73.12.flc.gp.dat'
f.sspec = 'srv_08127_wci29802_519.12.flc.gp.dat'
f.cspec = 'con_wei31784.flc.gp.dat'

f.eps = paste0(outdir, 'spectra.eps')
setEPS()
postscript(f.eps, width=11.5, height=5)
par(tck=0.03, mgp=c(1.5,0.2,0), cex.lab=1.2, cex.axis=1.1)
y1 = 0.4
fig1 = c(0,0.33,y1,1)
fig2 = fig1 + c(0,0,-y1,y1-1)
fig3 = c(0.33,0.66,y1,1)
fig4 = fig3 + c(0,0,-y1,y1-1)
fig5 = c(0.66,0.99,y1,1)
fig6 = fig5 + c(0,0,-y1,y1-1)
mar1 = mar3 = mar5 = c(3,3,0.5,0.3)
mar2 = mar4 = mar6 = c(3,3,0,0.3)

t.x = 0.01
t.cex = 1.3

yamp = 2.2 #mag
ylab1 = expression(italic(Q))
ylab2 = expression(paste(italic(I),' [mag]'))
xlab1 = expression(paste('Frequency [day'^'-1',']'))
xlab2 = 'HJD - 2450000'

par(fig=fig1, mar=mar1)
f.spe = paste0(dir, f.mspec)
spe = read.table(f.spe)
idx = spe[,1] < 0.01
spe = spe[idx,]
plot(spe[,1:2], type='l', xlab=xlab1, ylab=ylab1, lwd=2)
t.y = max(spe[,2]) - 0.1*(max(spe[,2]) - min(spe[,2]))
text(t.x, t.y, 'Mira', adj=1, cex=t.cex)
##************ Features **
y = spe[,2]
y1 = max(y)
y2 = quantile(y, 0.1)
x1 = 0.002
x2 = 0.0055
x3 = x1 + 0.0005
lines(c(x1,x2),c(y1,y1), col=4, lty=2)
abline(h=y2, col=4, lty=2)
arrows(x3, y1, x3, y2, col=4, lty=1, code=3, length=0.1)
text(x1, 0.5*(y1+y2), expression(paste(Delta, italic(Q))), col=4)
text(x1, y1, expression(paste(italic(Q)[1])), col=4, adj=c(1,0.8))
text(0.001, y2+0.2, expression(paste(italic(Q)['b'])), col=4, adj=c(1,0))
sub = spe[spe[,1]>0.004,]
y4 = max(sub[,2])
x4 = 0.0048
x5 = x4 + 0.0014
x6 = x4 + 0.0004
lines(c(x4,x5), c(y4,y4), col=4, lty=2)
arrows(x6, y1, x6, y4, col=4, lty=1, code=3, length=0.1)
text(x6+0.0001, 0.5*(y1+y4), expression(paste(Delta, italic(Q)[12])), col=4, adj=0)
##************ |||||||| **
f.lc = gsub('.gp.dat','',f.spe)
f.lc = gsub(dir, '', f.lc)
f.lc = paste0(lcdir.m, f.lc)
lc = read.table(f.lc)
x = lc[,1]
y = lc[,2]
e = lc[,3]
ylim = c(median(y) + yamp/2, median(y) - yamp/2)
par(fig=fig2, new=T, mar=mar2)
plot(x, y, pch=19, cex=0.5, ylim = ylim, main = '', xlab=xlab2, ylab=ylab2)
arrows(x, y-e, x, y+e, code=3, angle=90, length=0.01)
##************ Features **
meany = mean(y)
abline(h = meany, col=4, lty=2)
y01 = quantile(y,0.1)
y09 = quantile(y,0.9)
A = max(y) - min(y)
A09 = y09 - y01
lines(c(300,900),c(max(y),max(y)), lty=2, col=4)
lines(c(500,900),c(min(y),min(y)), lty=2, col=4)
lines(c(1400,1600),c(y09,y09), lty=2, col=4)
lines(c(1400,1600),c(y01,y01), lty=2, col=4)
arrows(850,max(y),850,min(y),col=4, lty=1, code=3, length=0.1)
arrows(1500,y09,1500,y01,col=4, lty=1, code=3, length=0.07)
text(2000, meany-0.15, expression(italic(bar(m))), col=4, adj=0)
text(1000, meany+0.4, expression(italic(A)), col=4, adj=1)
text(1480, meany-0.2, expression(italic(A)[0.9]), col=4, adj=1)



########
par(fig=fig3,new=T, mar=mar3)
f.spe = paste0(dir, f.sspec)
spe = read.table(f.spe)
idx = spe[,1] < 0.01
spe = spe[idx,]
plot(spe[,1:2], type='l', xlab=xlab1, ylab=ylab1, lwd=2)
t.y = max(spe[,2]) - 0.1*(max(spe[,2]) - min(spe[,2]))
text(t.x, t.y, 'SRV', adj=1, cex=t.cex)
f.lc = gsub('.gp.dat','',f.spe)
f.lc = gsub(dir, '', f.lc)
f.lc = paste0(lcdir.s, f.lc)
lc = read.table(f.lc)
x = lc[,1]
y = lc[,2]
e = lc[,3]
ylim = c(median(y) + yamp/2, median(y) - yamp/2)
par(fig=fig4, new=T, mar=mar4)
plot(x, y, pch=19, cex=0.5, ylim = ylim, main = '', xlab=xlab2, ylab=ylab2)
arrows(x, y-e, x, y+e, code=3, angle=90, length=0.01)



#########
par(fig=fig5, new=T, mar=mar5)
f.spe = paste0(dir, f.cspec)
spe = read.table(f.spe)
idx = spe[,1] < 0.01
spe = spe[idx,]
plot(spe[,1:2], type='l', xlab=xlab1, ylab=ylab1, lwd=1.5)
t.y = max(spe[,2]) - 0.1*(max(spe[,2]) - min(spe[,2]))
text(t.x, t.y, 'Constant\nstar', adj=1, cex=t.cex)
f.lc = gsub('.gp.dat','',f.spe)
f.lc = gsub(dir, '', f.lc)
f.lc = paste0(lcdir.c, f.lc)
lc = read.table(f.lc)
x = lc[,1]
y = lc[,2]
e = lc[,3]
ylim = c(median(y) + yamp/2, median(y) - yamp/2)
par(fig=fig6, new=T, mar=mar6)
plot(x, y, pch=19, cex=0.5, ylim = ylim, main = '', xlab=xlab2, ylab=ylab2)
arrows(x, y-e, x, y+e, code=3, angle=90, length=0.01)
dev.off()
