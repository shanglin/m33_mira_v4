dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
code.wrt.reg = paste0(dir.fun, 'fun.wrt.ds9reg.r')
source(code.wrt.reg)

phot.dir = '~/Work/m33_mira/phot/i/'
fields = list.files(phot.dir, pattern='....allframe')
fields = substr(fields, 1, 3)

dir = '~/Work/m33_mira/cmb_lcs/m33_lcs/'

ref = 'w0i'
nei = 'x3i'
f.map.ref = paste0(ref,'_uid_map.dat')
lf.map.ref = paste0(dir, f.map.ref)
map = read.table(lf.map.ref, sep=',')

## f.map.nei = paste0(nei,'_uid_map.dat')
## lf.map.nei = paste0(dir, f.map.nei)

n.col = which(fields == nei)

f.fnl.1 = paste0(dir,ref,'.fnl')
f.fnl.2 = paste0(dir,nei,'.fnl')
fnl.1 = read.table(f.fnl.1, skip=1)
fnl.2 = read.table(f.fnl.2, skip=1)

f.reg = paste0(dir, 'ref.reg')
write.ds9reg.image(f.reg,fnl.1[,2], fnl.1[,3], r=3,color='green')

f.reg = paste0(dir, 'nei.reg')
write.ds9reg.image(f.reg,fnl.2[,2], fnl.2[,3], r=3,color='red')

uids = map[, n.col]
uids = uids[!is.na(uids)]
nids = substr(uids, 4, 100)
nids = as.numeric(nids)
idx = match(nids, fnl.2[,1])
sub.fnl = fnl.2[idx,]
f.reg = paste0(dir, 'mch.reg')
write.ds9reg.image(f.reg,sub.fnl[,2], sub.fnl[,3], r=5,color='magenta')

