f.unf = '~/Work/m33_phaseII/sc_get_spec/part1.unfinished.dat'
unf = read.table(f.unf)
unf = as.character(unf[,1])

lst.dir = '~/Work/m33_phaseII/sc_get_spec/lsts_part2/'
slr.dir = '~/Work/m33_phaseII/sc_get_spec/slrms_part2/'


nfs.lc = length(unf)
n.batch = 20
n.less = floor(nfs.lc / n.batch)
n.more = ceiling(nfs.lc / n.batch)

switch.pos = nfs.lc - n.less * n.batch

idx.start = 1
for (i in 1:n.batch) {
    msg = paste0('   >> [lst] ',round(100*i/n.batch,2),' %     \r')
    message(msg, appendLF=F)
    if (i <= switch.pos) {
        idx.end = idx.start + n.more - 1
        sub = unf[idx.start:idx.end]
        write.table(sub, paste0(lst.dir,'p2_',i,'.lst'), quote=F, row.names=F, col.names=F)
        idx.start = idx.start + n.more
    } else {
        idx.end = idx.start + n.less - 1
        sub = unf[idx.start:idx.end]
        write.table(sub, paste0(lst.dir,'p2_',i,'.lst'), quote=F, row.names=F, col.names=F)
        idx.start = idx.start + n.less
    }
}
print('')

dir = '~/Work/m33_phaseII/sc_get_spec/'
w = function(ts, f = lf.slr, append=T) {
    write(ts, f, append=append)
}
f.sub = paste0(dir,'dosubmit_part2.sh')
write('#',f.sub)
for (i in 1:n.batch) {
    msg = paste0('   >> [slrm_part2] ',round(100*i/n.batch,2),' %     \r')
    message(msg, appendLF=F)
    f.lst = paste0('p2_',i,'.lst')
    j.num = gsub('.lst','',f.lst)
    j.num = gsub('p2_','',j.num)
    f.slr = gsub('.lst','.slrm',f.lst)
    lf.slr = paste0(slr.dir, f.slr)
    w('#!/bin/bash', append=F)
    w(paste0('#SBATCH -J v4-',j.num))
    w('#SBATCH -p stakeholder')
    w('#SBATCH --time=95:59:59')
    w('#SBATCH -n1')
    w('#SBATCH --mem-per-cpu=2000')
    w('#SBATCH -o outs/p2-v4-%j.out')
    w('#SBATCH -e errs/p2-v4-%j.err')
    w('echo "starting at `date` on `hostname`"')
    w('module load gcc')
    w('module load openblas')
    w('module load lapack')
    w(paste0('/fdata/scratch/yuanwenlong/m33_v4/codes/cpp/gpmodel -l lsts_part2/',f.lst))
    w('echo "ended at `date` on `hostname`"')
    w('exit 0')
    w(paste0('sbatch slrms_part2/',f.slr), f=f.sub)
    w('sleep 0.5', f=f.sub)
}
print('')
