
dir = '~/Work/m33_phaseII/sc_ext_ftrs/'
scr.dir = paste0(dir,'slrms/')

n.batch = 2990
w = function(ts, f = lf.slr, append=T) {
    write(ts, f, append=append)
}
f.sub = paste0(dir,'dosubmit.sh')
write('#',f.sub)

for (i in 1:n.batch) {
    j.num = i
    f.slr = paste0('ex_',i,'.slrm')
    lf.slr = paste0(scr.dir, f.slr)
    w('#!/bin/bash', append=F)
    w(paste0('#SBATCH -J ex-',j.num))
    w('#SBATCH -p stakeholder-4g')
    w('#SBATCH --time=95:59:59')
    w('#SBATCH -n1')
    w('#SBATCH --mem-per-cpu=4095')
    w('#SBATCH -o outs/ex-%j.out')
    w('#SBATCH -e errs/ex-%j.err')
    w('echo "starting at `date` on `hostname`"')
    w('module load intel/2013_sp1.3')
    w('module load R/3.1.1')
    w('MKL_NUM_THREADS=1')
    w(paste0('Rscript ext.feature.r ',i))
    w('echo "ended at `date` on `hostname`"')
    w('exit 0')
    
    w(paste0('echo "submit ',f.slr,'"'), f=f.sub)
    w(paste0('sbatch slrms/',f.slr), f=f.sub)
    w('sleep 0.5', f=f.sub)
}
print('')
