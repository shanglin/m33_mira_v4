dir = '~/Work/m33_mira/phot/i/x1i/trialp_all_x1i/'
f.fnl = paste0(dir,'x1i.fnl')
fnl = read.table(f.fnl, skip=1)

x = fnl[,2]
y = fnl[,3]
sharp = fnl[,8]

idx = x < 50 | x > 1920 | y < 50 | y > 2020 | sharp < -1.2 | sharp > 0.6 | sharp < -0.4
fnl = fnl[!idx,]


idx = order(fnl[,4])
fnl = fnl[idx,]
nfs = nrow(fnl)


for (i in 1:nfs) {
    f = paste0('lcV.',fnl[i,1])
    lf = paste0(dir,f)
    dat = read.table(lf)
    if (nrow(dat) > 10) {
        x = dat[,1] - 2450000
        y = dat[,2]
        e = dat[,3]
        y1 = fnl[i,4]+0.5
        y2 = fnl[i,4]-0.5
        y1 = max(y1,max(y))
        y2 = min(y2,min(y))
        ylim = c(y1,y2)
        xlim = c(0,4100)
        xpos = fnl[i,2]
        ypos = fnl[i,3]
        main = paste(xpos,ypos)
        main = fnl[i,8]
        plot(x,y,pch=19,cex=0.5,xlim=xlim,main=main,ylim=ylim,xlab='MJD',ylab='I (mag)')
        arrows(x,y+e,x,y-e,code=3,angle=90,length=0.01)
        Sys.sleep(0.9)
    }
}


x = fnl[,2]
y = fnl[,3]
e = fnl[,5]
plot(x,e,col=rgb(0,0,0,0.1))
abline(v=50,col=2)
abline(v=1920,col=2)

idx = x < 50 | x > 1920 | y < 50 | y > 2020
fnl2 = fnl[!idx,]
x = fnl2[,2]
y = fnl2[,3]
e = fnl2[,5]

plot(x,e,col=rgb(0,0,0,0.1))
abline(v=50,col=2)
abline(v=2020,col=2)

e = fnl[,5]
s = fnl[,8]
plot(s,e,col=rgb(0,0,0,0.1))
abline(v=-1.2,col=2)
abline(v=1.5,col=2)


c = fnl[,7]
plot(c,e,col=rgb(0,0,0,0.1),xlim=c(-0.1,5))
