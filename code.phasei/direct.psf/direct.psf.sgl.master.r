field = 'x1i'
f.as.master = 'ii6308.fits'

dir = paste0('~/Work/m33_mira/phot/i/',field,'/')
dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
code.dao.opt = paste0(dir.fun, 'fun.dao.wrt.opt.r')
source(code.dao.opt)
write.daophot.opt(dir, th = 5)
write.photo.opt(dir, c(5,6,7,8,9,10,11,12,13))
write.allstar.opt(dir)

code.phot = paste0(dir.fun, 'fun.dao.phot.r')
code.get.apl = paste0(dir.fun, 'fun.ap2apl.r')
code.wrt.reg = paste0(dir.fun, 'fun.wrt.ds9reg.r')
code.daomatch = paste0(dir.fun, 'fun.dao.match.r')
source(code.phot)
source(code.get.apl)
source(code.wrt.reg)
source(code.daomatch)

fs.fits = list.files(dir, pattern = 'ii.....fits')
nfs.fits = length(fs.fits)
daophot = '~/Programs/dao_e2/daophot'
daomatch = 'daomatch'
daomaster = 'daomaster'
for (i in 1:nfs.fits) {
    f.fits = fs.fits[i]
    print(paste0(' Finding stars in ',f.fits,' ...'))
    dao.find.stars(dir, f.fits, daophot = daophot)
    f.ap = gsub('.fits','.ap',f.fits)
    lf.ap = paste0(dir,f.ap)
    apl = ap2apl(lf.ap,3) ## aper = 7
    lf.reg = gsub('.ap','.reg',lf.ap)
    write.ds9reg.image(lf.reg, apl[,2], apl[,3])
    lf.apl = gsub('.ap','.apl',lf.ap)
    ts = ' NL    NX    NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
    write(ts,lf.apl)
    ts = '  1  2035  2045   668.1 20000.0  176.36    7.00   4.000  10.000    7.47'
    write(ts,lf.apl,append=T)
    write('',lf.apl,append=T)
    n.apl = nrow(apl)
    for (i in 1:n.apl) {
        tt = apl[i,]
        ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.2f%9.0f%9.2f%9.3f',
            tt[1],tt[2],tt[3],tt[4],tt[5],tt[6],5.,0.,0.)
        write(ts,lf.apl,append=T)
    }
}


f.master = paste0(field,'.fits')
lf.master = paste0(dir, f.master)
lf.as.master = paste0(dir, f.as.master)
cmd = paste0('cp ',lf.as.master,' ',lf.master)
system(cmd)

print(' PSF photometry on the master frame...')
bad.x0s = c()
bad.y0s = c()
bad.r0s = c()
if (f.master == 'x1i.fits') {
    bad.x0s = c(712, 1688)
    bad.y0s = c(1067, 1337)
    bad.r0s = c(80, 40)
}
write.daophot.opt(dir, th = 3)
write.photo.opt(dir, c(5,6,7,8,9,10,11,12,13))
write.allstar.opt(dir)
n.frames = length(list.files(dir, pattern = 'ii.....fits'))
dao.find.stars(dir, f.master, daophot = daophot, n.frame.average = n.frames, n.frame.sum = 1)
ret = dao.get.psf(dir, f.master, n.star = 30, mag.lim = 19, daophot = daophot)
allstar = '~/Programs/dao_e2/allstar'
ret = dao.allstar(dir, f.master, allstar = allstar)

f.als = gsub('.fits', '.als', f.master)
lf.als = paste0(dir, f.als)
als = read.table(lf.als, skip = 3)
als.all = als
x = als[,2]
y = als[,3]
edge.pix = 50
idx = x > edge.pix & x < max(x) - edge.pix & y > edge.pix & y < max(y) - edge.pix
als = als[idx,]
if (length(bad.x0s) > 0) {
    for (i.bad in 1:length(bad.x0s)) {
        x = als[,2]
        y = als[,3]
        idx = (x - bad.x0s[i.bad])^2 + (y - bad.y0s[i.bad])^2 > bad.r0s[i.bad]^2
        als = als[idx,]
    }
}
lf.als.header = paste0(dir,'als.header')
cmd = paste0('head -3 ', lf.als, ' > ', lf.als.header)
system(cmd)
lf.reg = gsub('.als', '.reg', lf.als)
write.ds9reg.image(lf.reg, als[,2], als[,3], r = 3, color = 'green')
cmd = paste0('head -3 ', lf.als.header, ' > ', lf.als)
system(cmd)
n.als = nrow(als)
for (i.als in 1:n.als) {
    tt = als[i.als,]
    ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.3f%9s%9.3f%9.3f',
        tt[1,1],tt[1,2],tt[1,3],tt[1,4],tt[1,5],tt[1,6],paste0(tt[1,7],'.'),tt[1,8],tt[1,9])
    write(ts, lf.als, append = T)
}

print(' Selecting secondary standards...')
mag.lim.bri = quantile(als[,4],0.0001)
mag.lim.fai = quantile(als[,4],0.01)
err.lim = 0.02
idx = als[,4] > mag.lim.bri & als[,4] < mag.lim.fai & als[,5] < err.lim
std.condidates = als[idx,]
n.std.cond = nrow(std.condidates)
near.dist = rep(NA, n.std.cond)
for (i.con in 1:n.std.cond) {
    als.all = als.all[-which(als.all[,1]==std.condidates[i.con, 1]),]
    near.dist[i.con] = min((als.all[,2] - std.condidates[i.con,2])^2 + (als.all[,3] - std.condidates[i.con,3])^2)
}
idx = rev(order(near.dist))
std.condidates = std.condidates[idx,]
n.std = min(50, n.std.cond)
std = std.condidates[1:n.std,]
lf.reg = paste0(dir,'test.reg')
write.ds9reg.image(lf.reg,std[,2],std[,3],r=7,color='magenta')
f.std = paste0(f.als, '.std')
lf.std = paste0(dir, f.std)
cmd = paste0('rm -f ',lf.std)
system(cmd)
for (i.std in 1:n.std) {
    tt = std[i.std,]
    ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.3f%9s%9.3f%9.3f',
        tt[1,1],tt[1,2],tt[1,3],tt[1,4],tt[1,5],tt[1,6],paste0(tt[1,7],'.'),tt[1,8],tt[1,9])
    write(ts, lf.std, append = T)
}

## Update the PSF for master frame.
print(' Updating the PSF by fitting the secondary standards...')
f.lst = gsub('.als', '.lst', f.als)
lf.lst = paste0(dir, f.lst)
cmd = paste0('head -3 ',lf.als.header,' > ',lf.lst)
system(cmd)
for (i.std in 1:n.std) {
    tt = std[i.std,]
    ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f',
        tt[1,1],tt[1,2],tt[1,3],tt[1,4],tt[1,5])
     write(ts, lf.lst, append = T)
}
ret = dao.update.psf(dir, f.master, daophot = daophot)

## Rerun ALLSTAR use updated PSF
print(' PSF photometry for master frame using new PSF...')
write.daophot.opt(dir, th = 3)
write.allstar.opt(dir)
ret = dao.allstar(dir, f.master, allstar = allstar)
f.als = gsub('.fits', '.als', f.master)
lf.als = paste0(dir, f.als)
als = read.table(lf.als, skip = 3)
x = als[,2]
y = als[,3]
edge.pix = 50
idx = x > edge.pix & x < max(x) - edge.pix & y > edge.pix & y < max(y) - edge.pix
als = als[idx,]
if (length(bad.x0s) > 0) {
    for (i.bad in 1:length(bad.x0s)) {
        x = als[,2]
        y = als[,3]
        idx = (x - bad.x0s[i.bad])^2 + (y - bad.y0s[i.bad])^2 > bad.r0s[i.bad]^2
        als = als[idx,]
    }
}
lf.als.header = paste0(dir,'als.header')
cmd = paste0('head -3 ', lf.als, ' > ', lf.als.header)
system(cmd)
lf.reg = gsub('.als', '.reg', lf.als)
write.ds9reg.image(lf.reg, als[,2], als[,3], r = 3, color = 'green')
cmd = paste0('head -3 ', lf.als.header, ' > ', lf.als)
system(cmd)
n.als = nrow(als)
for (i.als in 1:n.als) {
    tt = als[i.als,]
    ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9.3f%9s%9.3f%9.3f',
        tt[1,1],tt[1,2],tt[1,3],tt[1,4],tt[1,5],tt[1,6],paste0(tt[1,7],'.'),tt[1,8],tt[1,9])
    write(ts, lf.als, append = T)
}


## Update the psf for each frame
fs.apl = list.files(dir, pattern = '^ii.*.apl$')
nfs.apl = length(fs.apl)
code.std2psf = paste0(dir.fun, 'fun.std2psf.r')
source(code.std2psf)
for (i.apl in 1:nfs.apl) {
    f.apl = fs.apl[i.apl]
    f.fits = gsub('.apl', '.fits', f.apl)
    print(paste0(' Updating PSF for ',f.fits,'...'))
    std2psf(dir, f.als, f.apl, f.std, daomatch = daomatch, daomaster = daomaster, daophot = daophot)
}
f.as.psf = gsub('.fits', '.psf', f.as.master)
f.psf = gsub('.als', '.psf', f.als)
lf.as.psf = paste0(dir, f.as.psf)
lf.psf = paste0(dir, f.psf)
cmd = paste0('cp ',lf.psf,' ',lf.as.psf)
system(cmd)

## Match all the frames to the master frame use apl files
print(' Matching all the frames to the master frame...')
fs.apl = list.files(dir, pattern = '^ii.*.apl$')
fs.all.frame = c(f.als, fs.apl)
f.mch = 'all.frame.mch'
dao.match.fast(dir, fs.all.frame, f.mch, daomatch = daomatch)
dao.master(dir, f.mch, daomaster = daomaster)
lf.mch = paste0(dir, f.mch)
f.new = paste0(field, '.mch')
lf.new = paste0(dir, f.new)
cmd = paste0('awk \'NR>1\' ',lf.mch,' > ',lf.new)
system(cmd)
write.allframe.opt(dir)
print('Running ALLFRAME...')
allframe = '~/Programs/dao_e2/allframe'
ret = dao.allframe(dir, f.new, allframe = allframe) 
