
## 1. file name list
## 2. slrm scripts
## 3. dosubmit.sh
## 4. compress results

set.seed(101)
dir = '~/Work/m33_phaseII/sc_get_spec/'
lc.dir = '~/Work/m33_phaseII/m33_v4_lcs/'
lst.dir = paste0(dir,'lsts/')
scr.dir = paste0(dir,'slrms/')

## 1
fs.lc = list.files(lc.dir)
nfs.lc = length(fs.lc)
idx = sample(1:nfs.lc, replace=F)
fs.lc = fs.lc[idx]
fs.lc = paste0('/fdata/scratch/yuanwenlong/m33_v4/m33_v4_lcs/',fs.lc)

n.batch = 2990
n.less = floor(nfs.lc / n.batch)
n.more = ceiling(nfs.lc / n.batch)

switch.pos = nfs.lc - n.less * n.batch

idx.start = 1
for (i in 1:n.batch) {
    msg = paste0('   >> [lst] ',round(100*i/n.batch,2),' %     \r')
    message(msg, appendLF=F)
    if (i <= switch.pos) {
        idx.end = idx.start + n.more - 1
        sub = fs.lc[idx.start:idx.end]
        write.table(sub, paste0(lst.dir,'pe_',i,'.lst'), quote=F, row.names=F, col.names=F)
        idx.start = idx.start + n.more
    } else {
        idx.end = idx.start + n.less - 1
        sub = fs.lc[idx.start:idx.end]
        write.table(sub, paste0(lst.dir,'pe_',i,'.lst'), quote=F, row.names=F, col.names=F)
        idx.start = idx.start + n.less
    }
}
print('')


## 2, 3
w = function(ts, f = lf.slr, append=T) {
    write(ts, f, append=append)
}
f.sub = paste0(dir,'dosubmit.sh')
write('#',f.sub)
for (i in 1:n.batch) {
    msg = paste0('   >> [slrm] ',round(100*i/n.batch,2),' %     \r')
    message(msg, appendLF=F)
    f.lst = paste0('pe_',i,'.lst')
    j.num = gsub('.lst','',f.lst)
    j.num = gsub('pe_','',j.num)
    f.slr = gsub('.lst','.slrm',f.lst)
    lf.slr = paste0(scr.dir, f.slr)
    w('#!/bin/bash', append=F)
    w(paste0('#SBATCH -J v4-',j.num))
    w('#SBATCH -p background-4g')
    w('#SBATCH --time=95:59:59')
    w('#SBATCH -n1')
    w('#SBATCH --mem-per-cpu=4095')
    w('#SBATCH -o outs/v4-%j.out')
    w('#SBATCH -e errs/v4-%j.err')
    w('echo "starting at `date` on `hostname`"')
    w('module load gcc')
    w('module load openblas')
    w('module load lapack')
    w(paste0('/fdata/scratch/yuanwenlong/m33_v4/codes/cpp/gpmodel -l lsts/',f.lst))
    w('echo "ended at `date` on `hostname`"')
    w('exit 0')
    w(paste0('sbatch slrms/',f.slr), f=f.sub)
    w('sleep 0.5', f=f.sub)
}
print('')
