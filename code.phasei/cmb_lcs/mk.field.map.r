dir = '~/Work/m33_mira/cmb_lcs/m33_lcs/'
f.cat = 'bf_cmb_lc.cat'
lf.cat = paste0(dir, f.cat)

phot.dir = '~/Work/m33_mira/phot/i/'
fields = list.files(phot.dir, pattern='....allframe')
fields = substr(fields, 1, 3)
n.fields = length(fields)
cat = read.table(lf.cat)
cat[,1] = as.character(cat[,1])
cat.fields = substr(cat[,1], 1, 3)

rsqr = 0.0003^2
dmag = 0.5
map = matrix('NA', nrow=1e6, ncol=n.fields)
n.start = 0
n.end = 0

for (field in fields) {
    col.idx = which(fields == field)
    rest.fields = fields[(col.idx+1):n.fields]
    sub = cat[cat.fields == field,]

    row.idx = !(sub[,1] %in% map[, col.idx])
    sub = sub[row.idx,]
    
    n.start = n.end + 1
    n.end = n.start + nrow(sub) - 1
    map[n.start:n.end, col.idx] = sub[,1]
    
    for (rest.field in rest.fields) {
        n.col = which(fields == rest.field)
        if (length(n.col) == 1) {
            print(paste0(' > Matching ',field,'--',rest.field))
            tmh = cat[cat.fields == rest.field,]
            sub.ra.rge = range(sub[,2])
            sub.dec.rge = range(sub[,3])
            tmh.ra.rge = range(tmh[,2])
            tmh.dec.rge = range(tmh[,3])
            ra.olp.1 = sub.ra.rge[1] > tmh.ra.rge[1] & sub.ra.rge[1] < tmh.ra.rge[2]
            ra.olp.2 = sub.ra.rge[2] > tmh.ra.rge[1] & sub.ra.rge[2] < tmh.ra.rge[2]
            ra.olp = ra.olp.1 | ra.olp.2
            dec.olp.1 = sub.dec.rge[1] > tmh.dec.rge[1] & sub.dec.rge[1] < tmh.dec.rge[2]
            dec.olp.2 = sub.dec.rge[2] > tmh.dec.rge[1] & sub.dec.rge[2] < tmh.dec.rge[2]
            dec.olp = dec.olp.1 | dec.olp.2
            if (ra.olp & dec.olp) {
                n.sub = nrow(sub)
                n.tmh = nrow(tmh)
                for (i in 1:n.sub) {
                    ra = sub[i, 2]
                    dec = sub[i, 3]
                    idx = ((tmh[,2] - ra)^2 + (tmh[,3] - dec)^2) < rsqr
                    if (sum(idx) == 0) {
                        map[n.start+i-1, n.col] = 'NA'
                    } else if (sum(idx) == 1) {
                        map[n.start+i-1, n.col] = tmh[idx, 1]
                    } else {
                        idices = which(((tmh[,2] - ra)^2 + (tmh[,3] - dec)^2) < rsqr & abs(tmh[,4] - sub[i,4]) < dmag)
                        n.idices = length(idices)
                        if (n.idices == 0) {
                            idices = which(((tmh[,2] - ra)^2 + (tmh[,3] - dec)^2) < rsqr)
                            n.idices = length(idices)
                        }
                        dists = (tmh[idices, 2] - ra)^2 + (tmh[idices, 3] - dec)^2
                        idx = idices[which.min(dists)]
                        map[n.start+i-1, n.col] = tmh[idx, 1]
                    }
                }
            } 
        }
        ## well, there is also a possibility two objects matched to a single object in the new frame
        all.uids = map[n.start:n.end, n.col]
        uids = all.uids[all.uids != 'NA']
        tbl = table(uids)
        dup.uids = names(tbl[tbl>1])
        for (uid in dup.uids) {
            idices = which(uid == all.uids)
            pre.uids = sub[idices, col.idx]
            ts.idx = tmh[,1] == uid
            ra = tmh[ts.idx, 2]
            dec = tmh[ts.idx, 3]
            god.idx = which.min( (sub[idices,2]-ra)^2 + (sub[idices,3]-dec)^2 )
            bad.idx = idices[-god.idx]
            map[n.start + bad.idx - 1, n.col] = 'NA'
        }
    }
    
    sub.map = map[n.start:n.end,]
    f.map = paste0(field, '_uid_map.dat')
    lf.map = paste0(dir, f.map)
    write.table(sub.map, lf.map, quote=F, col.names=F, row.names=F, sep=',')
}

map = map[1:n.end,]
f.map = 'uid_map.dat'
lf.map = paste0(dir, f.map)
write.table(map, lf.map, quote=F, col.names=F, row.names=F, sep=',')

