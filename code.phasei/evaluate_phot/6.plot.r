options(stringsAsFactors=F)
f.dat = '~/Work/m33_mira/evaluate_phot/id_merd_mas.dat'
dat = read.table(f.dat)

## 1. w0i and x3i

all = unique(dat[,2])
f.pdf = '~/Work/m33_mira/evaluate_phot/dm.pdf'
pdf(f.pdf, width=7, height=4)
par(mar=c(5,5,2,2))
for (f1 in all) {
    for (f2 in all) {
        if (f1 < f2) {
        fields = c(f1,f2)
        idx = dat[,2] %in% fields[1]
        sub1 = dat[idx,]
        idx = dat[,2] %in% fields[2]
        sub2 = dat[idx,]

        idx = match(sub1[,1], sub2[,1])
        if (sum(!is.na(idx)) > 20) {
            sub2 = sub2[idx,]
            idx = !is.na(sub2[,1])
            sub2 = sub2[idx,]
            sub1 = sub1[idx,]
            
            x = sub1[,3]
            y = sub2[,3] - sub1[,3]
            e = sqrt(sub1[,5]^2+sub2[,5]^2)
xt = range(x)[1]
            main = paste(f2,'-',f1)
            xlab = bquote(italic(I)~' [mag]')
            ylab = bquote(italic(I)[.(f2)]~' - '~italic(I)[.(f1)]~' [mag]')
            ylim = c(-0.3,0.3)
            plot(x,y, xlab=xlab, ylab=ylab, main=main, ylim=ylim, pch=1, col='grey')
            arrows(x, y-e, x, y+e, code=3, angle=90, length=0.001, col='grey')
            for (ifoo in 1:15) {
                my = mean(y)
                idx = abs(y-my) < 3*sd(y)
                y = y[idx]
                x = x[idx]
                e = e[idx]
            }
            points(x,y, pch=19)
            arrows(x, y-e, x, y+e, code=3, angle=90, length=0.001)
            my = mean(y)
            sd = sd(y)
            abline(h=my, lty=1, col=4)
            abline(h=my+sd, lty=2, col=4)
            abline(h=my-sd, lty=2, col=4)
            t1 = paste0('Mean = ',round(my,3))
            t2 = bquote(sigma~' = '~.(round(sd,3)))
            
            text(xt, 0.28, t1, col=4, adj=0)
            text(xt, 0.23, t2, col=4, adj=0)
            ## a = readline()
        }
    }
    }
}
dev.off()
