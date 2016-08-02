options(stringsAsFactors=F)

dir = '~/Work/m33_mira/evaluate_phot/'
f.dat = paste0(dir, 'iden_uid.dat')
f.mas = paste0(dir, 'id_merd_mas.dat')
slcdir = '~/Work/m33_mira/cmb_lcs/light_curves/m33_islcs/'
ulcdir = '~/Work/m33_mira/cmb_lcs/light_curves/tmp_lcs/'

dat = read.table(f.dat)
ndat = nrow(dat)
mas = read.table(f.mas)

f.out = paste0(dir,'M33_photometry.dat')
ts = '#  M33SSSid    std_dev mean_mag   lcerr  field  std_dev mean_mag  lcerr     err        ra         dec        x         y           LGGS_id          LGGS_I'
write(ts, f.out)
for (i in 1:ndat) {
    sid = dat[i,1]
    uid = dat[i,2]
    f.slc = paste0(slcdir, sid, '.slc')
    f.ulc = paste0(ulcdir, uid, '.ulc')
    if (file.exists(f.slc) & file.exists(f.ulc)) {
        slc = read.table(f.slc)
        ulc = read.table(f.ulc)
        for (ifoo in 1:10) {
            idx = abs(slc[,2]-mean(slc[,2])) < 3*sd(slc[,2])
            slc = slc[idx,]
            idx = abs(ulc[,2]-mean(ulc[,2])) < 3*sd(ulc[,2])
            ulc = ulc[idx,]
        }
        ssd = round(sd(slc[,2]),3)
        usd = round(sd(ulc[,2]),3)
        smag = round(mean(slc[,2]),3)
        umag = round(mean(ulc[,2]),3)
        sdof = max(1, nrow(slc)-1)
        udof = max(1, nrow(ulc)-1)
        serr = ssd / sqrt(sdof)
        uerr = usd / sqrt(udof)
        
        field = substr(uid,1,3)
        idx = mas[,1] == sid & mas[,2] == field
        if (sum(idx) != 1) {
            print(i)
        } else {
            tt = mas[idx,]
            fmt = '%12s%9.3f%9.3f%9.4f%6s%9.3f%9.3f%9.4f%9.4f%12.5f%12.5f%10.3f%10.3f%21s%8.3f'
            ts = sprintf(fmt,
                sid, ssd, smag, serr, field, usd, umag, uerr, tt[5],  tt[6], tt[7],
                tt[8], tt[9], tt[10], tt[11])
            write(ts, f.out, append=T)
            ## par(mfrow=c(2,1))
            ## xlim = range(slc[,1])
            ## ylim = rev(range(slc[,2]))
            ## plot(slc[,1:2],xlim=xlim, ylim=ylim, xlab = ssd)
            ## plot(ulc[,1:2],xlim=xlim, ylim=ylim, xlab = usd)
            ## a = readline()
        }
    }
}


a = read.table(f.out)
par(mfrow=c(2,1))

plot(a[,2],a[,6],cex=0.3, xlim=c(0,0.4),ylim=c(0,0.4), xlab='sd', ylab='sd_seg')
lines(c(0,3),c(0,3),col=2)

plot(a[,4],a[,8],cex=0.3, xlim=c(0,0.05),ylim=c(0,0.05), xlab='sigma', ylab='sigma_seg')
lines(c(0,3),c(0,3),col=2)
