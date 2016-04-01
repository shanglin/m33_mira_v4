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
code.wrt.reg = paste0(dir.fun, 'fun.wrt.ds9reg.r')
source(code.wrt.reg)

fs.alf = list.files(dir, pattern = '.*.alf$')
nfs.alf = length(fs.alf)
for (i in 1:nfs.alf) {
    f.alf = fs.alf[i]
    lf.alf = paste0(dir, f.alf)
    lf.reg = paste0(lf.alf, '.reg')
    alf = read.table(lf.alf, skip = 3)
    write.ds9reg.image(lf.reg, alf[,2], alf[,3])
    print(f.alf)
}

