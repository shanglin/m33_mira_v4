f = '~/Work/m33_phaseII/m33_ofiles/sig_mag/lc_sig_mag.dat'
d = read.table(f)
a = d[,2]
b = d[,3]
c = d[,4]

hist(a, breaks=50)
