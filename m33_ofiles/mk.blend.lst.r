dir = '~/Work/m33_phaseII/m33_ofiles/'
lcdir = paste0(dir, 'islcs/')
f.dat = paste0(dir, 'm33i_bright.dat')
f.lst = paste0(dir, 'blended_star.lst')

dat = read.table(f.dat)
objs = as.character(dat[,1])
n.obj = length(objs)
write('# id', f.lst)

pdf('~/Desktop/blend_lcs.pdf',width=8,height=5)
for (i in 1:n.obj) {
    obj = objs[i]
    f.lc = paste0(lcdir, obj, '.slc')
    msg = paste0('    >> ',round(i*100/n.obj,2),' %    \r')
    message(msg, appendLF=F)

    tele = substr(obj,1,1)
    if (tele == 'w') {
        lc = read.table(f.lc)
        idx = lc[,1] < 2000
        if (sum(idx) < 3) next
        g1 = lc[idx,]
        
        idx = lc[,1] > 2000
        if (sum(idx) < 3) next
        g2 = lc[idx,]

        sd.1 = sd(g1[,2])
        sd.2 = sd(g2[,2])
        mean.diff = mean(g1[,2]) - mean(g2[,2])

        if (sd.1 < 0.25 & sd.2 < 0.4 & abs(mean.diff) > 0.6 & mean(g2[,2]) > 20.5) {
            write(obj, f.lst, append=T)
            plot(lc[,1:2], pch=19, cex=0.8, ylim=c(max(lc[,2]+0.3), min(lc[,2])-0.3), main=obj)
            arrows(lc[,1],lc[,2]+lc[,3],lc[,1],lc[,2]-lc[,3], code=3, length=0, angle=90)
            ## Sys.sleep(2)
        }
    } else {
        lc = read.table(f.lc)
        idx = lc[,1] < 1000
        if (sum(idx) < 3) next
        g1 = lc[idx,]
        
        idx = lc[,1] > 1000
        if (sum(idx) < 3) next
        g2 = lc[idx,]

        sd.1 = sd(g1[,2])
        sd.2 = sd(g2[,2])
        mean.diff = mean(g1[,2]) - mean(g2[,2])

        if (sd.1 < 0.25 & sd.2 < 0.2 & abs(mean.diff) > 0.6) {
            write(obj, f.lst, append=T)
            plot(lc[,1:2], pch=19, cex=0.8, ylim=c(max(lc[,2]+0.3), min(lc[,2])-0.3), main=obj)
            arrows(lc[,1],lc[,2]+lc[,3],lc[,1],lc[,2]-lc[,3], code=3, length=0, angle=90)
            ## Sys.sleep(2)
        }
    }
}
dev.off()
