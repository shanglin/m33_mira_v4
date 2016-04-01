field = 'w0i'
sfields = c(0:9, letters[1:19])
fields = paste0('w', sfields, 'i')

for (field in fields) {

## cat(' Please input the field (i.e. x1i): ')
## f.ask = file('stdin')
## field = readLines(f.ask,1)
## close(f.ask)
    
if (sum(fields == field) != 1) stop(' No such field.')
band = substr(field,3,3)
sfield = substr(field,2,2)

dir = paste0('~/Work/m33_mira/phot/i/',field,'_allframe/trialp_all_',field,'/')
f.fnl = paste0(dir, field, '.fnl')
fnl = read.table(f.fnl, skip = 1)
fnl = fnl[fnl[,6]>3,]
mag = fnl[,4]
quant = quantile(mag, 0.5)
idx = abs(fnl[,8]) < 3 & mag < quant
print(paste(field, quant))
fnl = fnl[idx,]
idx = order(fnl[,1])
fnl = fnl[idx,]
n.fnl = nrow(fnl)
n.fnl = min(n.fnl, 100)
idx = sample(1:n.fnl)
fnl = fnl[idx,]

for (i in 1:n.fnl) {
    id = fnl[i,1]
    f = paste0(dir,'lcV.',id)
    lc = read.table(f)
    lc = lc[lc[,3]<0.3,]
    if (nrow(lc) > 1) {
        x = lc[,1] - 2450000
        y = lc[,2]
        e = lc[,3]
        xlim = c(1900, 4000)
        ylim = c(fnl[i,4]-0.7, fnl[i,4]+0.7)
        main = paste(field, fnl[i,4], sep='   ')
        plot(x,y,pch=19,cex=0.6,xlim = xlim, ylim = ylim, main=main)
        arrows(x,y-e,x,y+e,code=3,angle=90,length=0)
        Sys.sleep(0.02)
    }
}
}
