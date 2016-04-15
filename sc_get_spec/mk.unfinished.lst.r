lst.dir = '~/Work/m33_phaseII/sc_get_spec/lsts/'
stt.dir = '~/Work/m33_phaseII/sc_get_spec/part1.res/status/'

fs.lst = list.files(lst.dir)
nfs = length(fs.lst)

f.unf = '~/Work/m33_phaseII/sc_get_spec/part1.unfinished.dat'
write('# ids',f.unf)
for (i.lst in 1:nfs) {
    f.lst = fs.lst[i.lst]
    f.txt = paste0(f.lst,'.status.txt')
    lf.lst = paste0(lst.dir, f.lst)
    lf.txt = paste0(stt.dir, f.txt)

    lst = read.table(lf.lst)[,1]
    lids = as.character(lst)
    ids = gsub('/fdata/scratch/yuanwenlong/m33_v4/m33_v4_lcs/','',lids)

    txt = read.table(lf.txt)
    id = as.character(txt[1,2])
    
    if (which(id == ids) == length(ids)) {
        next
    } else {
        uids = lids[(which(id==ids)+1):length(ids)]
        for (i in 1:length(uids)) write(uids[i], f.unf, append=T)
    }
    print(i.lst/nfs*100)
}
