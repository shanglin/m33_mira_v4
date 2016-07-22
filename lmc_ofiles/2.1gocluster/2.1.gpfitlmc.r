args = commandArgs(trailingOnly = TRUE)
start = args[1]
end = args[2]

dir = '/fdata/scratch/yuanwenlong/m33_v4/gp_lmc/'
lcdir = paste0(dir, 'mira.lcs/')
gpdir = './gp_spectra/'
outdir = paste0(dir, 'lmc_gp_pars/')

options(stringsAsFactors = F)

f.csv = paste0(dir, 'mira.cat.csv')
csv = read.csv(f.csv)
ncsv = nrow(csv)

gp = '/fdata/scratch/yuanwenlong/m33_v4/gp_lmc/cpp_fixp/gpmodelfp'
f.out = paste0(outdir, 'lmc_cpp_pars',start,'.dat')
ts = '#     id      OGLE_P     newP     theta1      theta2'
write(ts, f.out)

for (ic in start:end) {
    id = csv[ic,1]
    f.fig = paste0(outdir, id, 'outfig.pdf')
    pdf(f.fig)
    period = csv[ic, 'P_1']
    ra = csv[ic,'RA']
    dec = csv[ic,'Decl']
    dra = 0
    ddec = 0
    f.lc = paste0(lcdir, id, '.dat')
    cmd = paste(gp, f.lc, period, 2)
    print(ic)
    system(cmd)

    par(mfrow=c(2,1))
    f.gp = paste0(gpdir,id,'.dat.gp.dat')
    resolutions = c(0.3)
    for (resolution in resolutions) {
        spc = read.table(f.gp)
        idx = spc[,1] < 1/80
        spc = spc[idx,]
        F = spc[which.max(spc[,2]),1]
        newP = 1 / F
        plot(spc[,1:2], pch=19)
        abline(v = F, col=2)
        cmd = paste(gp, f.lc, newP, resolution)
        system(cmd)
    }
    spc = read.table(f.gp)
    idx = spc[,1] < 1/80
    spc = spc[idx,]
    idx = which.max(spc[,2])
    F = spc[idx,1]
    plot(spc[,1:2], pch=19)
    abline(v = F, col=2)
    newP = 1 / F
    if (F == spc[1,1] | F == spc[nrow(spc),1]) newP = -99
    t1 = spc[idx,3]
    t2 = spc[idx,4]
    ts = sprintf('%30s%12.2f%13.3f%12.6f%12.6f',
        id, period, newP, t1, t2)
    write(ts, f.out, append=T)
    dev.off()
}

