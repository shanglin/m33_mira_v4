fields = c('x1i','x2i','x3i','x4i',
    'y1i','y2i','y3i','y4i',
    'zai','zbi','zci')
cat(' Please input the field (i.e. x1i): ')
f.ask = file('stdin')
field = readLines(f.ask,1)
close(f.ask)
idx = fields == field
if (sum(idx) != 1) stop(' No such field.')

dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/trialp_all_',field,'/')
f.fnl = paste0(dir, field, '.fnl')
fnl = read.table(f.fnl, skip = 1)
idx = abs(fnl[,8]) < 3 & fnl[,4] < 21
fnl = fnl[idx,]
idx = order(fnl[,1])
fnl = fnl[idx,]

n.fnl = nrow(fnl)
for (i in 1:n.fnl) {
    id = fnl[i,1]
    f = paste0(dir,'lcV.',id)
    lc = read.table(f)
    lc = lc[lc[,3]<0.3,]
    if (nrow(lc) > 5) {
        x = lc[,1] - 2450000
        y = lc[,2]
        e = lc[,3]
        xlim = c(0, 1900)
        ylim = c(fnl[i,4]-0.7, fnl[i,4]+0.7)
        plot(x,y,pch=19,cex=0.6,xlim = xlim, ylim = ylim, main=fnl[i,4])
        arrows(x,y-e,x,y+e,code=3,angle=90,length=0)
        Sys.sleep(0.1)
    }
}
