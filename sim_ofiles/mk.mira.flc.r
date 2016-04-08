rm(list=ls())
set.seed(101)

n.sim = 1e5
mag.shift = 6.2
mag.cut = 21.45
least.night = 7
type = 'mira'

dir = paste0('~/Work/m33_phaseII/sim_ofiles/flc_',type,'/')
slc.dir = '~/Work/m33_phaseII/m33_ofiles/islcs/'
llc.dir = paste0('~/Work/m33_phaseII/lmc_ofiles/',type,'.lcs/')

f.com = '~/Work/m33_phaseII/m33_ofiles/complete_fun/lum_fun.dat'
com = read.table(f.com)
f.sig = '~/Work/m33_phaseII/m33_ofiles/sig_mag/lc_sig_mag.dat'
sig = read.table(f.sig)
f.m33.cat = '~/Work/m33_phaseII/m33_ofiles/m33i_bright.dat'
m33.cat = read.table(f.m33.cat)
n.m33 = nrow(m33.cat)
f.lmc.cat = paste0('~/Work/m33_phaseII/lmc_ofiles/',type,'.cat.csv')
lmc.cat = read.csv(f.lmc.cat)[,c(1,9,11)]
lmc.cat[,1] = as.character(lmc.cat[,1])
idx = which(lmc.cat[,2]>8.5)
lmc.cat = lmc.cat[idx, ]
lmc.cat[,2] = lmc.cat[,2] + mag.shift
idx = lmc.cat[,2] <= mag.cut
lmc.cat = lmc.cat[idx, ]
idx = order(lmc.cat[,2])
lmc.cat = lmc.cat[idx,]
row.names(lmc.cat) = 1:nrow(lmc.cat)
idx = match(round(lmc.cat[,2],3), com[,1])
freqs = com[idx,2]
lmc.cat = cbind(lmc.cat, freqs)
n.lmc = nrow(lmc.cat)


fig.dir = paste0('~/Work/m33_phaseII/sim_ofiles/figs_',type,'/')
f.para = '~/Work/m33_phaseII/lmc_ofiles/allLMC.txt'
f.basicfun = '~/Work/m33_phaseII/code.phaseii/GPcodes/r_v2/BasicFuns.R'
golden.ratio = 1.61803398875
fig.height = 10 # inches
fig.width = fig.height * golden.ratio

library(VarStar)
source(f.basicfun)
paraAll = read.table(f.para, header=FALSE)
paraIDs = paraAll[,1]
thetasAll = as.matrix(paraAll[,3:9])
thetasAll = abs(thetasAll)
thetasAll[,4] = 100000

i.sim = 0
while (i.sim < n.sim) {
    idx.tlc = sample(1:n.m33, 1)
    M33ID = m33.cat[idx.tlc, 1]
    lf.tlc = paste0(slc.dir, M33ID, '.slc')

    idx.lmc = sample(1:n.lmc, 1, prob = lmc.cat[,4])
    LMCID = lmc.cat[idx.lmc, 1]
    sLMCID = gsub('OGLE-LMC-LPV-','',LMCID)
    period = lmc.cat[idx.lmc, 3]
    lf.dat = paste0(llc.dir, LMCID,'.dat')

    tlc = read.table(lf.tlc)
    nobs = nrow(tlc)
    nights = round(tlc[,1])
    n.night = length(unique(nights))
    dat = read.table(lf.dat)
    lmc.range = max(dat[,1]) - min(dat[,1])
    m33.range = max(tlc[,1]) - min(tlc[,1])
    
    if (n.night >= least.night & lmc.range > m33.range) {
        meanMag = mean(dat[,2])
        x = new(gpModel, dat$V1, dat$V2, dat$V3, meanMag)
        x$set_period(period, 0)
        ThetaIdx = which(paraIDs == paste0(LMCID))
        x$gp_setTheta(thetasAll[ThetaIdx,])
        
        sData = x$get_fake(tlc$V1, 0)
        jd.shift = sData$shift
        flc = sData$fLcurve
        flc[,2] = flc[,2] + mag.shift

        for (i in 1:nobs) {
            mag = flc[i,2]
            sig.idx = which(sig[,1] == tlc[i,1])
            sig.a = sig[sig.idx, 2]
            sig.b = sig[sig.idx, 3]
            sig.c = sig[sig.idx, 4]
            sig.c = max(sig.c, 0.00001)
            flc[i,3] = round(sig.a^(mag - sig.b) + sig.c, 3)
            flc[i,2] = round(rnorm(1, mean = mag, sd = flc[i,3]), 3)
        }
        flc = flc[order(flc[,1]),]

        cflc = flc[flc[,3]<3,]
        
        if ((nrow(flc) - nrow(cflc)) < 3) {
            f.flc = paste0(dir,type,'_',sLMCID,'_',M33ID,'_',round(jd.shift,2),'.flc')
            write.table(cflc, f.flc, row.names = F, col.names = F, sep = '  ')
            i.sim = i.sim + 1
            msg = paste0('    >> ',round(i.sim * 100 / n.sim, 3),' %      \r')
            message(msg, appendLF = F)
        }
        
        if (F) {
            f.eps = paste0(fig.dir,type,'_',sLMCID,'_',M33ID,'_',round(jd.shift,2),'.eps')
            setEPS()
            postscript(f.eps,height = fig.height, width = fig.width)    
            x = dat[,1]
            y = dat[,2] + mag.shift
            e = dat[,3]
            plot(x,y,ylim=c(max(y) + 0.5, min(y) - 0.5), pch = 19, cex = 0.5, xlab = 'MJD', ylab = 'I (mag)')
            x = flc[,1] - min(flc[,1]) + min(dat[,1]) + jd.shift
            y = flc[,2]
            e = flc[,3]
            points(x,y,pch = 19, cex = 0.5, col = 4)
            arrows(x, y + e, x, y - e, code = 3, angle = 90, length = 0.02, col = 4)
            dev.off()
        }
    }
}
print('')
print('FINISHED')

