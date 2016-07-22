options(stringsAsFactors = F)
f.csv = '~/Work/m33_phaseII/lmc_ofiles/mira.cat.csv'
scr.dir = './slrms/'

csv = read.csv(f.csv)
ncsv = nrow(csv)

seps = seq(1, ncsv, 5)
seps = c(seps, ncsv+1)
n = length(seps)

w = function(ts, f = lf.slr, append=T) {
    write(ts, f, append=append)
}
f.sub = paste0('dosubmit.sh')
write('#',f.sub)


for (i in 1:(n-1)) {
    start = seps[i]
    end = seps[i+1]-1
    j.num = i
    f.slr = paste0('gplmc_',i,'.slrm')
    lf.slr = paste0(scr.dir, f.slr)
    w('#!/bin/bash', append=F)
    w(paste0('#SBATCH -J l4-',j.num))
    w('#SBATCH -p stakeholder-4g')
    w('#SBATCH --time=45:59:59')
    w('#SBATCH -n1')
    w('#SBATCH --mem-per-cpu=4095')
    w('#SBATCH -o outs/l4-%j.out')
    w('#SBATCH -e errs/l4-%j.err')
    w('echo "starting at `date` on `hostname`"')
    w('module load gcc')
    w('module load openblas')
    w('module load lapack')
    w(paste('Rscript 2.1.gpfitlmc.r', start, end))
    w('echo "ended at `date` on `hostname`"')
    w('exit 0')
    w(paste0('sbatch slrms/',f.slr), f=f.sub)
    w('sleep 0.5', f=f.sub)
}

