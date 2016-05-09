f.tbl = '~/Work/m33_mira/astrometry/massey/new_tab13.txt'

txt = read.table(f.tbl)
txt[,1] = as.character(txt[,1])
rah = txt[,2]
ram = txt[,3]
ras = txt[,4]
dcd = txt[,5]
dcm = txt[,6]
dcs = txt[,7]
rmag = txt[,8] - txt[,14]
imag = rmag - txt[,16]
vmag = txt[,8]
ev = txt[,9]
ei = txt[,17]

ra2dra = function(h, m, s) {
    dra = 15*(h + m/60 + s/3600)
    return(dra)
}

dec2ddec = function(d, m, s) {
    ddec = sign(d) * (abs(d) + m/60 + s/3600)
    return(ddec)
}

f.dat = '~/Work/m33_phaseII/MasseyVI/massey_vi.dat'
RA = ra2dra(rah, ram, ras)
Dec = dec2ddec(dcd, dcm, dcs)
RA = round(RA,5)
Dec = round(Dec,5)
ID = as.data.frame(txt[,1])
idx = abs(imag) > 30
imag[idx] = -99.999

dat = cbind(ID, RA, Dec, vmag, ev, imag, ei)
ts = '#             ID                RA         Dec     vmag      ev      imag      ei'
write(ts, f.dat)
fmt = '%25s%11.5f%11.5f%9.3f%8.3f%10.3f%9.3f'
out = do.call('sprintf',c(fmt, dat))
write(out, f.dat, append=T)

