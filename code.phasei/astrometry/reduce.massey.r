dir = '~/Work/m33_mira/astrometry/massey/'
f.txt = paste0(dir, 'tab13.txt')
f.new = paste0(dir, 'new_tab13.txt')
cmd = paste0('awk \'NR>46 {print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17}\' ',f.txt,' > ',f.new)
system(cmd)

txt = read.table(f.new)
txt[,1] = as.character(txt[,1])
rah = txt[,2]
ram = txt[,3]
ras = txt[,4]
dcd = txt[,5]
dcm = txt[,6]
dcs = txt[,7]
rmag = txt[,8] - txt[,14]
imag = rmag - txt[,16]

ra2dra = function(h, m, s) {
    dra = 15*(h + m/60 + s/3600)
    return(dra)
}

dec2ddec = function(d, m, s) {
    ddec = sign(d) * (abs(d) + m/60 + s/3600)
    return(ddec)
}

RA = ra2dra(rah, ram, ras)
Dec = dec2ddec(dcd, dcm, dcs)
idx = abs(txt[,17]) < 90
RA = round(RA[idx],5)
Dec = round(Dec[idx],5)
ID = txt[idx,1]
Imag = imag[idx]

f.dat = paste0(dir,'massey_i.dat')
dat = cbind(ID, RA, Dec, Imag)
write.table(dat, f.dat, row.names=F, quote=F, sep = '   ')
