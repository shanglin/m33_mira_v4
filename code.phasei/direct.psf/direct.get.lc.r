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

dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/')
dir.fun = '~/Work/m33_mira/codes/M33MiraRlib/'
code.alf2lc = paste0(dir.fun, 'fun.alf2lc.r')
source(code.alf2lc)
dir.info = '~/Work/m33_mira/phot/info_files/'
cmd = paste0('cp ',dir.info,field,'.inf ', dir)
system(cmd)
cmd = paste0('cp ',dir.info,field,'.prs ', dir)
system(cmd)
cmd = paste0('cp ',dir.info,field,'.clb ', dir)
system(cmd)

f.fits = paste0(field, '.fits')
lf.fits = paste0(dir, f.fits)
cmd = paste0('imhead ',lf.fits, ' | grep EXPTIME > foo.tmp')
system(cmd)
foo = read.table('foo.tmp')
exptime = foo[1,3]
system('rm -f foo.tmp')
## make .lib & .fet file
f.lib = paste0(field,'.lib')
f.fet = paste0(field,'.fet')
lf.lib = paste0(dir, f.lib)
lf.fet = paste0(dir, f.fet)

f.nmg = paste0(field,'.nmg')
lf.nmg = paste0(dir, f.nmg)
f.std = paste0(field,'.als.std')
lf.std = paste0(dir, f.std)

std = read.table(lf.std)
nmg = read.table(lf.nmg, skip = 3)

write(' 1 MAGNITUDE:            V', lf.lib)
cmd = paste0('head -3 ', lf.nmg, ' > ', lf.fet)
system(cmd)

n.std = nrow(std)
for (i.std in 1:n.std) {
    id = std[i.std, 1]
    idx = nmg[,1] == id
    if (sum(idx) == 1) {
        if (nmg[idx,6] < 50) {
            sid = as.character(id)
            while (nchar(sid) < 7) sid = paste0('0',sid)
            lid = paste0(field,'-',sid)
            llid = paste0('0',lid)
            ts = sprintf('%12s%9.3f%8.3f%7.4f%4i%5i',
                lid, nmg[idx,5], nmg[idx,4]+2.5*log10(exptime),
                nmg[idx,5] / sqrt(nmg[idx,7]), nmg[idx,7], nmg[idx,7])
            write(ts, lf.lib, append = T)
            write(llid, lf.fet, append = T)
            tt = nmg[idx,]
            ts = sprintf('%7i%9.3f%9.3f%9.3f%9.4f%9s%9s%9.3f%9.3f',
                id, tt[1,2], tt[1,3], tt[1,4], tt[1,5],
                paste0(tt[1,6],'.'), paste0(tt[1,7],'.'),
                tt[1,8], tt[1,9])
            write(ts, lf.fet, append = T)
        }
    }
}

trialp = '~/Programs/dao_e2/trialp'
root = field
ret = dao.alf2lc(root, dir, trialp = trialp)
