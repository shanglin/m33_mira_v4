field = 'x1i'

fields = c('x1i','x2i','x3i','x4i',
    'y1i','y2i','y3i','y4i',
    'zai','zbi','zci')
cat(' Please input the field (i.e. x1i): ')
f.ask = file('stdin')
field = readLines(f.ask,1)
close(f.ask)
idx = fields == field
if (sum(idx) != 1) stop(' No such field.')


vfield = paste0(substr(field,1,2),'v')
dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/')
mdir = '~/Work/m33_mira/astrometry/massey_reduced/'
odir = paste0('~/Work/m33_mira/astrometry/',field,'/')
dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
code.dao.match = paste0(dir.fun, 'fun.dao.match.r')
code.radec.tools = paste0(dir.fun, 'fun.radec.tools.r')
source(code.dao.match)
source(code.radec.tools)
daomatch = '~/Programs/dao_e2/daomatch'
daomaster = '~/Programs/dao_e2/daomaster'
imwcs = '~/Programs/wcstools-3.9.2/bin/imwcs'


f.idmap = paste0(mdir,'m33massey.cat')
idmap = read.table(f.idmap, skip=2)

cmd = paste0('mkdir -p ',odir)
system(cmd)

f.als = list.files(mdir, pattern=paste0('^M33.*.',vfield,'.als'))
if (length(f.als) != 1) stop(' Fake .als file not found.')
lf.als = paste0(mdir, f.als)
cmd = paste0('cp ',lf.als,' ',odir)
system(cmd)

f.nmg = paste0(field,'.nmg')
lf.nmg = paste0(dir, f.nmg)
cmd = paste0('cp ',lf.nmg,' ',odir)
system(cmd)

## (1) prepare id, ra, dec, massey_id file
fs.mag = c(f.als, f.nmg)
f.mch = 'tmp.mch'
ret = dao.match.fast(odir, fs.mag, f.mch, daomatch=daomatch, clean=T)
ret = dao.master.mtr(odir, f.mch, daomaster=daomaster, clean=T)
f.mtr = paste0(field,'.mtr')
lf.mtr = paste0(odir, f.mtr)
if (!file.exists(lf.mtr)) stop(paste0(f.mtr, 'not found.'))
mtr = read.table(lf.mtr, skip=3)
f.out = paste0(field,'_rd.dat')
lf.out = paste0(odir, f.out)
ts = '#ID   X   Y   RA   Dec   Massey_ID  RA.h RA.m. RA.s Dec.d Dec.m Dec.s'
write(ts, lf.out)
n.mtr = nrow(mtr)
for (i.mtr in 1:n.mtr) {
    msg = paste0(' >> [',field,'] ',round(100*i.mtr/n.mtr,2),' %    \r')
    message(msg, appendLF=F)
    id = mtr[i.mtr, 10]
    mid.als = mtr[i.mtr, 1]
    x = mtr[i.mtr, 2]
    y = mtr[i.mtr, 3]
    idx = idmap[,1] == mid.als
    if (sum(idx) != 1) stop(paste0(mid.als, 'not found in ',f.idmap))
    tt = idmap[idx,]
    mid = tt[1, 10]
    ra = ra2dra(tt[1,2], tt[1,3], tt[1,4])
    dec = dec2ddec(tt[1,5], tt[1,6], tt[1,7])
    ts = paste(id, x, y, round(ra,6), round(dec,6), mid, tt[1,2], tt[1,3], tt[1,4], tt[1,5], tt[1,6], tt[1,7], sep='   ')
    write(ts, lf.out, append=T)
}
print('                      ')



## (2) update WCS for all the images with photometry
tbl = read.table(lf.out)
 ## (a) take care of the master frame first
f.fits = paste0(field, '.fits')
lf.fits = paste0(dir, f.fits)
cmd = paste0('cp ',lf.fits,' ',odir)
system(cmd)
f.xyrd = paste0(field,'_xyrd.dat')
lf.xyrd = paste0(odir, f.xyrd)
cmd = paste0('rm -f ',lf.xyrd)
system(cmd)
for (i in 1:nrow(tbl)) {
    ts = paste(tbl[i,2], tbl[i,3], tbl[i,7], tbl[i,8], tbl[i,9], tbl[i,10], tbl[i,11], tbl[i,12],  sep='  ')
    write(ts, lf.xyrd, append=T)
}

dir.current = getwd()
setwd(odir)
cmd = paste0(imwcs, ' -n 8 -q p -u ',f.xyrd,' -w ',f.fits)
system(cmd)
setwd(dir.current)
lf.mch = paste0(odir, f.mch)
f.tfr = gsub('.mch', '.ftr',f.mch)
lf.tfr = paste0(odir, f.tfr)
lf.fits = paste0(odir, f.fits)
cmd = paste0('rm -f ',lf.fits,' ',lf.mch,' ',lf.tfr)
system(cmd)

 ## (b) update all the images with ALF photometry
fs.alf = list.files(dir, pattern='.*.alf$')
nfs = length(fs.alf)
for (i.alf in 1:nfs) {
    f.alf = fs.alf[i.alf]
    f.fits = gsub('.alf','.fits',f.alf)
    print(paste0(' >> [',field,'] Updating WCS for image ', f.fits, '  [',i.alf,'/',nfs,']'))
    lf.alf = paste0(dir, f.alf)
    lf.fits = paste0(dir, f.fits)
    alf = read.table(lf.alf, skip=3)
    root = gsub('.alf','',f.alf)
    f.xyrd = paste0(root,'_xyrd.dat')
    lf.xyrd = paste0(odir, f.xyrd)
    cmd = paste0('rm -f ',lf.xyrd)
    system(cmd)
    for (i in 1:nrow(tbl)) {
        idx = tbl[i,1] == alf[,1]
        if (sum(idx) == 1) {
            ts = paste(alf[idx,2], alf[idx,3], tbl[i,7], tbl[i,8], tbl[i,9], tbl[i,10], tbl[i,11], tbl[i,12],  sep='  ')
            write(ts, lf.xyrd, append=T)
        }
    }
    cmd = paste0('cp ',lf.fits,' ',odir)
    system(cmd)
    dir.current = getwd()
    setwd(odir)
    cmd = paste0(imwcs, ' -n 8 -q p -u ',f.xyrd,' -w ',f.fits)
    system(cmd)
    setwd(dir.current)
    lf.fits = paste0(odir, f.fits)
    cmd = paste0('rm -f ',lf.fits)
    system(cmd)
}

