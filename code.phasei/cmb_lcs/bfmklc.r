
dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
code.wrt.reg = paste0(dir.fun, 'fun.wrt.ds9reg.r')
source(code.wrt.reg)
xy2sky = '~/Programs/wcstools-3.9.2/bin/xy2sky'

dir = '~/Work/m33_mira/cmb_lcs/m33_lcs/'
phot.dir = '~/Work/m33_mira/phot/i/'
zer.dir = '~/Work/m33_mira/cmb_lcs/mag_off/'
f.zer = 'fnl_zero_point.dat'
lf.zer = paste0(zer.dir, f.zer)
zer = read.table(lf.zer)
zer[,1] = as.character(zer[,1])

f.cat = 'bf_cmb_lc.cat'
lf.cat = paste0(dir, f.cat)
ts = '#  id       ra        dec        m        e'
write(ts, lf.cat)

fields = list.files(phot.dir, pattern='....allframe')
for (lfield in fields) {
    field = substr(lfield,1,3)
    print(field)
    idx = zer[,1] == field
    a = zer[idx, 2]
    b = zer[idx, 3]
    fnl.dir = paste0(phot.dir, field, '_allframe/trialp_all_', field, '/')
    f.fnl = paste0(field, '.fnl')
    of.fnl = paste0(fnl.dir, f.fnl)
    lf.fnl = paste0(dir, f.fnl)
    cmd = paste0('cp ',of.fnl,' ',lf.fnl)
    system(cmd)
    ast.dir = paste0('~/Work/m33_mira/astrometry/',field,'/')
    f.fits = paste0(field, 'w.fits')
    wf.fits = paste0(ast.dir, f.fits)
    lf.fits = paste0(dir, f.fits)
    cmd = paste0('cp ',wf.fits,' ',lf.fits)
    system(cmd)
    f.xy = paste0(field,'.xy')
    lf.xy = paste0(dir, f.xy)
    cmd = paste0('awk \'NR>1 {print $2,$3}\' ',lf.fnl,' > ',lf.xy)
    system(cmd)
    ## xy = read.table(lf.xy)
    ## f.reg = paste0(field, '.reg')
    ## lf.reg = paste0(dir, f.reg)
    ## write.ds9reg.image(lf.reg,xy[,1],xy[,2],r=3,color='magenta')
    f.rdxy = paste0(field, '.rdxy')
    lf.rdxy = paste0(dir, f.rdxy)
    dir.current = getwd()
    setwd(dir)
    cmd = paste0(xy2sky,' -d ',f.fits,' @',f.xy,' > ',f.rdxy)
    system(cmd)
    setwd(dir.current)
    fnl = read.table(lf.fnl, skip=1)
    rdxy = read.table(lf.rdxy)
    uid = paste0(field, fnl[,1])
    ra = rdxy[,1]
    dec = rdxy[,2]
    m = fnl[,4] * a + b
    e = fnl[,5]
    m = round(m, 3)
    cat = cbind(uid, ra, dec, m, e)
    write.table(cat, lf.cat, quote=F, col.names=F, row.names=F, append=T, sep='   ')
    
}
