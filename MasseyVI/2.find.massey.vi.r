if (T) {
f.cad = '~/Work/m33_phaseII/mira_cata_nb/m33_mira_candidate.dat'
cad = read.table(f.cad, header=T)
cad[,1] = as.character(cad[,1])
idx = rev(order(cad[,'Scaled_prob']))
cad = cad[idx,]

f.rd = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
rd = read.table(f.rd)
rd[,1] = as.character(rd[,1])
rd[,1] = paste0(rd[,1], '.slc')

idx = match(cad[,1], rd[,1])
srd = rd[idx,]

cadvi = cbind(cad, srd[,2:3], srd[,4:5], srd[,4:5])
mcol = ncol(cadvi)
for (i in 0:3) cadvi[,mcol-i] = -99.99

f.vi = '~/Work/m33_phaseII/MasseyVI/massey_vi.dat'
dat = read.table(f.vi)
ra = dat[,2]
dec = dat[,3]
}

n.cad = nrow(cad)
## n.cad = 500
dists = rep(NA, n.cad)
dist.cut = 2e-4
for (i in 1:n.cad) {
    tt = cadvi[i,]
    idx = which.min((tt[1,mcol-5] - ra)^2 + (tt[1,mcol-4] - dec)^2)
    ts = dat[idx,]
    dist = sqrt((tt[1,mcol-5] - ra[idx])^2 + (tt[1,mcol-4] - dec[idx])^2)
    dists[i] = dist
    if (dist < dist.cut) {
        cadvi[i, mcol-3] = ts[1,4]
        cadvi[i, mcol-2] = ts[1,5]
        cadvi[i, mcol-1] = ts[1,6]
        cadvi[i, mcol] = ts[1,7]
    }
    msg = paste0('   >> ',round(100*i/n.cad,1),' %    \r')
    message(msg, appendLF=F)
}
print('')

## pixel scale: 1 pixel ~ 1e-4 degree
## let's take 2 pixel ~ 2e-4 as the match criterium
f.eps = '~/Work/m33_phaseII/MasseyVI/match_radius.eps'
setEPS()
postscript(f.eps, width=6, height=6)
hist(dists[dists<10*dist.cut], breaks=50, main='Matching radius \n Between Mira candidates and Massey\'s catalog', xlab='Arc Degrees',cex.lab=1.3, cex.axis=1.3, col='skyblue')
abline(v=dist.cut, col=2)
t1 = '2e-4 arc degree'
t2 = '~ 2 pixels in image of smaller telescope'
text(dist.cut*1.4, 670, t1, adj=0, col=2, cex=1.3)
text(dist.cut*1.4, 600, t2, adj=0, col=2, cex=1.3)
dev.off()


cadvi[,1] = gsub('.slc','',cadvi[,1])
cadvi2 = cadvi[,-c(2)]
f.out = '~/Work/m33_phaseII/MasseyVI/m33_cad_vi.dat'
fmt = '%12s%10.6f%10.3f%9.3f%9.3f%8.3f%10.5f%10.5f%8.3f%8.3f%8.3f%5i%8.3f%8.3f%8.3f%9.3f%9.3f%8.3f%9.3f%10.3f%10.3f%10.3f%11.5f%11.5f%9.3f%9.3f%10.3f%9.3f'
ts = '       ID      F.peak     Q.peak  Q.base dQ.p1.base dQ.p1.p2 theta.1 log10.theta.2 A.model A.lc A.lc.9 n.obs sd.error sig.a   sig.b   i.mag   lc.mag   mean.sd qdr.rsd.sd ratio.q2m Scaled_prob Y_prob   RA         Dec      Ms_V      eV       Ms_I      eI'
write(ts, f.out)
out = do.call('sprintf',c(fmt, cadvi2))
write(out, f.out, append=T)

