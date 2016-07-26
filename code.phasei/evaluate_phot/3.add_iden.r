options(stringsAsFactors=F)

dir = '~/Work/m33_mira/evaluate_phot/'
f.dat = paste0(dir, 'f_id_m_e_rd.dat')
dat = read.table(f.dat)
ndat = nrow(dat)

fields = unique(dat[,1])

f.map = '~/Work/m33_mira/cmb_lcs/m33_lcs/uid_map.dat'
## map = read.table(f.map, sep=',')

iden = rep(NA, ndat)
alluids = paste0(dat[,1],dat[,2])

for (i in 1:ndat) {
    ## print(paste(i,ndat))
    id = dat[i,2]
    field = dat[i,1]
    uid = paste0(field, id)
    col = which(fields == field)
    idx = map[,col] == uid
    if (sum(idx, na.rm=T) == 1) {
        ts = map[which(idx),]
        uids = ts[!is.na(ts)]
        ix = alluids %in% uids
        iden[ix] = uid
    }
}

new = as.data.frame(matrix(NA, ncol=2, nrow=1e5))
iden = iden[!is.na(iden)]
uiden = unique(iden)
niden = length(uiden)
end = 0
for (i in 1:niden) {
    uid = uiden[i]
    field = substr(uid,1,3)
    col = which(fields == field)
    idx = map[,col] == uid
    if (sum(idx, na.rm=T) != 1) stop(uid)
    ts = map[which(idx),]
    uids = ts[!is.na(ts)]
    nids = length(uids)
    if (nids > 1) {
        start = end + 1
        end = start + nids - 1
        new[start:end,1] = uids[1]
        new[start:end,2] = uids
    }
}
idx = !is.na(new[,1])
new = new[idx,]
nnew = nrow(new)

fmt = '%12s%12s'
out = do.call('sprintf',c(new, fmt))
f.out = paste0(dir,'iden_uid.dat')
write('# M33SSS_iden    sid', f.out)
write(out, f.out, append=T)

      
