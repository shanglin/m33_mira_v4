if (T) {
f.cad = '~/Work/m33_phaseII/mira_cata_sampsize/cadco.csv'
cad = read.csv(f.cad)
cad[,1] = as.character(cad[,1])

f.rd = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
rd = read.table(f.rd)
rd[,1] = as.character(rd[,1])
rd[,1] = paste0(rd[,1], '.slc')

idx = match(cad[,1], rd[,1])
srd = rd[idx,]

## cadvi = cbind(cad, srd[,2:3], srd[,4:5], srd[,4:5])
cadvi = cbind(cad[,1], cad[,1], srd[,2:3], srd[,2:3], srd[,4:5], srd[,4:5])
mcol = ncol(cadvi)
for (i in 0:3) cadvi[,mcol-i] = -99.999
for (i in 5:6) cadvi[,i] = 0
cadvi[,2] = 'xxxxx'

f.vi = '~/Work/m33_phaseII/MasseyVI/massey_vi.dat'
dat = read.table(f.vi)
dat[,1] = as.character(dat[,1])
ra = dat[,2]
dec = dat[,3]
}

n.cad = nrow(cad)
## n.cad = 500
dists = rep(NA, n.cad)
dist.cut = 2e-4
for (i in 1:n.cad) {
    tt = cadvi[i,]
    idx = which.min((tt[1,3] - ra)^2 + (tt[1,4] - dec)^2)
    ts = dat[idx,]
    dist = sqrt((tt[1,3] - ra[idx])^2 + (tt[1,4] - dec[idx])^2)
    dists[i] = dist
    if (dist < dist.cut) {
        cadvi[i, 2] = ts[1,1]
        cadvi[i, 5] = ts[1,2]
        cadvi[i, 6] = ts[1,3]
        cadvi[i, 7] = ts[1,4]
        cadvi[i, 8] = ts[1,5]
        cadvi[i, 9] = ts[1,6]
        cadvi[i, 10] = ts[1,7]
    }
    msg = paste0('   >> ',round(100*i/n.cad,1),' %    \r')
    message(msg, appendLF=F)
}
print('')

## pixel scale: 1 pixel ~ 1e-4 degree
## let's take 2 pixel ~ 2e-4 as the match criterium
f.eps = '~/Work/m33_phaseII/MasseyVI/match_radius_sampsize.eps'
setEPS()
postscript(f.eps, width=6, height=6)
hist(dists[dists<10*dist.cut], breaks=50, main='Matching radius \n Between Mira candidates and Massey\'s catalog', xlab='Arc Degrees',cex.lab=1.3, cex.axis=1.3, col='skyblue')
abline(v=dist.cut, col=2)
t1 = '2e-4 arc degree'
t2 = '~ 2 pixels in image of smaller telescope'
text(dist.cut*1.4, 670, t1, adj=0, col=2, cex=1.3)
text(dist.cut*1.4, 600, t2, adj=0, col=2, cex=1.3)
dev.off()


f.out = '~/Work/m33_phaseII/MasseyVI/m33_cad_vi_sampsize.dat'
fmt = '%12s%21s%11.5f%11.5f%11.5f%11.5f%9.3f%9.3f%10.3f%9.3f'
ts = '       ID        mID      RA      Dec      mRA        mDec      Ms_V      eV       Ms_I      eI'
write(ts, f.out)
out = do.call('sprintf',c(fmt, cadvi))
write(out, f.out, append=T)

