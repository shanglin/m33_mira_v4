dir = '~/Work/m33_phaseII/lmc_ofiles/'
lcdir = paste0(dir, 'mira.lcs/')

options(stringsAsFactors = F)

f.csv = paste0(dir, 'mira.cat.csv')
csv = read.csv(f.csv)
ncsv = nrow(csv)

gp = '~/Work/m33_phaseII/code.phaseii/GPcodes/cpp_fixp/gpmodelfp'
for (ic in 1:ncsv) {
    id = csv[ic,1]
    period = csv[ic, 'P_1']
    ra = csv[ic,'RA']
    dec = csv[ic,'Decl']
    dra = 0
    ddec = 0
    f.lc = paste0(lcdir, id, '.dat')
    cmd = paste(gp, f.lc, period)
    print(i)
    system(cmd)
}

