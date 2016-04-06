rm(list=ls())
set.seed(101)

n.sim = 1e5
mag.cut = 21.45
type = 'con'

dir = paste0('~/Work/m33_phaseII/sim_ofiles/flc_',type,'/')
slc.dir = '~/Work/m33_phaseII/m33_ofiles/islcs/'

f.m33.cat = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
m33.cat = read.table(f.m33.cat)
n.m33 = nrow(m33.cat)

fig.dir = paste0('~/Work/m33_phaseII/sim_ofiles/figs_',type,'/')
golden.ratio = 1.61803398875
fig.height = 9 # inches
fig.width = fig.height / golden.ratio

i.sim = 0
while (i.sim < n.sim) {
    idx.tlc = sample(1:n.m33, 1)
    M33ID = m33.cat[idx.tlc, 1]
    lf.tlc = paste0(slc.dir, M33ID, '.slc')
    tlc = read.table(lf.tlc)
    nobs = nrow(tlc)
    
    flc = tlc
    flc[,1] = sample(tlc[,1])
    flc = flc[order(flc[,1]),]

    f.flc = paste0(dir,type,'_',M33ID,'.flc')
    if (file.exists(f.flc)) {
        next
    } else {
        write.table(flc, f.flc, row.names = F, col.names = F, sep = '  ')
        i.sim = i.sim + 1
        msg = paste0('    >> ',round(i.sim * 100 / n.sim, 3),' %      \r')
        message(msg, appendLF = F)
        
        if (F) {
            f.eps = paste0(fig.dir,type,'_',M33ID,'.eps')
            setEPS()
            postscript(f.eps,height = fig.height, width = fig.width)
            par(mfrow=c(2,1))
            x = tlc[,1]
            y = tlc[,2]
            e = tlc[,3]
            plot(x,y,ylim=c(max(y) + 0.5, min(y) - 0.5), pch = 19, cex = 0.5, xlab = 'MJD', ylab = 'I (mag)')
            arrows(x, y + e, x, y - e, code = 3, angle = 90, length = 0.02)
            x = flc[,1]
            y = flc[,2]
            e = flc[,3]
            plot(x,y,ylim=c(max(y) + 0.5, min(y) - 0.5),pch = 19, cex = 0.5, col = 4, xlab = 'MJD', ylab = 'I (mag)')
            arrows(x, y + e, x, y - e, code = 3, angle = 90, length = 0.02, col = 4)
            dev.off()
        }
    }
}
print('')
print('FINISHED')

