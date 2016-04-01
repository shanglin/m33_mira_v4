w = function(ts, f.out, append = T) {
    write(ts, f.out, append = append)
}


write.daophot.opt = function(dir, fw = 3, wa = -1,
    re = 10, ga = 4, va = 0, hi = 20000, lo = 20,
    th = 10, psf = 15, fi = 2, use = 0, an = -3) {
    f.out = paste0(dir,'daophot.opt')
    w(paste0('FW=',fw), f.out, append = F)
    w(paste0('WA=',wa), f.out)
    w(paste0('RE=',re), f.out)
    w(paste0('GA=',ga), f.out)
    w(paste0('VA=',va), f.out)
    w(paste0('HI=',hi), f.out)
    w(paste0('LO=',lo), f.out)
    w(paste0('TH=',th), f.out)
    w(paste0('PSF=',psf), f.out)
    w(paste0('FI=',fi), f.out)
    w(paste0('USE=',use), f.out)
    w(paste0('AN=',an), f.out)
}

write.photo.opt = function(dir, apers,
    is = 15, os = 20) {
    n.aper = length(apers)
    if (n.aper > 10) stop(' Please use less than 10 apertures.')
    f.out = paste0(dir,'photo.opt')
    system(paste0('rm -f ',f.out))
    for (i in 1:n.aper) {
        w(paste0('A',i-1,'=',apers[i]), f.out)
    }
    w(paste0('A',i,'=0'), f.out)
    w(paste0('IS=',is), f.out)
    w(paste0('OS=',os), f.out)
}

write.allstar.opt = function(dir, fi = 2,
    ce = 4, is = 15, os = 20, re = 1) {
    f.out = paste0(dir,'allstar.opt')
    w(paste0('fi=',fi), f.out, append = F)
    w(paste0('ce=',ce), f.out)
    w(paste0('IS=',is), f.out)
    w(paste0('OS=',os), f.out)
    w(paste0('re=',re), f.out)
}

write.allframe.opt = function(dir, ce = 2, cr = 2.5,
    ge = 20, wa = 1, mi = 5, ma = 50, pe = 0, pr = 0,
    is = 15, os = 20) {
    f.out = paste0(dir,'allframe.opt')
    w(paste0('CE=',ce), f.out, append = F)
    w(paste0('CR=',cr), f.out)
    w(paste0('GE=',ge), f.out)
    w(paste0('WA=',wa), f.out)
    w(paste0('MI=',mi), f.out)
    w(paste0('MA=',ma), f.out)
    w(paste0('PE=',pe), f.out)
    w(paste0('PR=',pr), f.out)
    w(paste0('IS=',is), f.out)
    w(paste0('OS=',os), f.out)
}
