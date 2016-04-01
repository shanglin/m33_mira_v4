cal.err = function(x, a, b, c) {
    return(a^(x-b) + c)
}


dir = '~/Work/m33_mira/cmb_lcs/light_curves/'
slc.dir = '~/Work/m33_mira/cmb_lcs/light_curves/m33_islcs/'
ulc.dir = '~/Work/m33_mira/cmb_lcs/light_curves/tmp_lcs/'
cmd = paste0('mkdir -p ',slc.dir,' ',ulc.dir)
system(cmd)
base.dir = '~/Work/m33_mira/cmb_lcs/'
zer.dir = '~/Work/m33_mira/cmb_lcs/mag_off/'
map.dir = '~/Work/m33_mira/cmb_lcs/m33_lcs/'
fnl.dir = map.dir
rd.dir = map.dir
sig.dir = '~/Work/m33_mira/cmb_lcs/sig_mag_fit/'
phot.dir = '~/Work/m33_mira/phot/i/'

fields = list.files(phot.dir, pattern='....allframe')
fields = substr(fields, 1, 3)
n.fields = length(fields)

f.zer = 'fnl_zero_point.dat'
lf.zer = paste0(zer.dir, f.zer)
f.sig = 'sig_mag_fit.dat'
lf.sig = paste0(sig.dir, f.sig)
    
zer = read.table(lf.zer)
sig = read.table(lf.sig)

f.urd = 'uid_rd.dat'
lf.urd = paste0(dir, f.urd)

ts = '#  uid    ra    dec'
write(ts, lf.urd)
for (field in fields) {
    idx = which(zer[,1] == field)
    a = zer[idx, 2]
    b = zer[idx, 3]   
    inf.dir = paste0(phot.dir, field, '_allframe/')
    lc.dir = paste0(inf.dir, 'trialp_all_', field, '/')
    f.inf = paste0(field, '.inf')
    lf.inf = paste0(inf.dir, f.inf)
    con = file(lf.inf, 'r')
    inf = readLines(con)
    close(con)
    inf.roots = substr(inf,1,20)
    inf.roots = gsub(' ','',inf.roots)
    inf.mjds = as.numeric(substr(inf, 65, 78))
    
    f.fnl = paste0(field, '.fnl')
    lf.fnl = paste0(fnl.dir, f.fnl)
    fnl = read.table(lf.fnl, skip=1)
    n.fnl = nrow(fnl)
    
    f.rd = paste0(field, '.rdxy')
    lf.rd = paste0(rd.dir, f.rd)
    rd = read.table(lf.rd)

    fs.alf = list.files(inf.dir, pattern='.*.alf$')
    n.frame = length(fs.alf)
    sig.labels = matrix(1, nrow = n.fnl, ncol = n.frame+1)
    sig.labels = as.data.frame(sig.labels)
    colnames(sig.labels) = c('id', fs.alf)
    sig.labels[,1] = fnl[,1]
    for (f.alf in fs.alf) {
        lf.alf = paste0(inf.dir, f.alf)
        alf = read.table(lf.alf, skip=3)
        root = gsub('.alf', '', f.alf)
        root.idx = which(sig[,2] == root & sig[,1] == field)
        A = sig[root.idx, 3]
        B = sig[root.idx, 4]
        C = sig[root.idx, 5]
        x.shift = sig[root.idx, 6]
        y.shift = sig[root.idx, 7]
        ## plot(alf[,4], alf[,5], pch=19, cex=0.1)
        bad.idx = which(alf[,5] > (cal.err(alf[,4]+x.shift, A, B, C) + y.shift))
        ## points(alf[bad.idx, 4:5], col=4)
        bad.ids = alf[bad.idx, 1]
        idx.0 = sig.labels[,1] %in% bad.ids
        sig.labels[idx.0, f.alf] = 0
    }

    for (i.fnl in 1:n.fnl) {
        nid = fnl[i.fnl, 1]
        f.lc = paste0('lcV.',nid)
        lf.lc = paste0(lc.dir, f.lc)
        if (file.info(lf.lc)$size > 0) {
            lc = read.table(lf.lc)[,1:4]
            lc[,4] = 1
            n.lc = nrow(lc)
            for (i in 1:n.lc) {
                mjd = lc[i,1]
                root = inf.roots[which.min(abs(inf.mjds - mjd))]
                col.lab = paste0(root, '.alf')
                row.idx = which(sig.labels[,1] == nid)
                lc[i, 4] = sig.labels[row.idx, col.lab]
            }
            lc = lc[lc[,4] == 1,]
            if (nrow(lc) > 0) {
                ulc = lc[,1:3]
                ulc[,1] = round(ulc[,1] - 2450000, 4)
                ulc[,2] = round(ulc[,2] * a + b, 3)
                uid = paste0(field, nid)
                ra = rd[i.fnl, 1]
                dec = rd[i.fnl, 2]
                ## m = fnl[i.fnl, 4] * a + b
                ## e = fnl[i.fnl, 5]
                f.ulc = paste0(uid,'.ulc')
                lf.ulc = paste0(ulc.dir, f.ulc)
                write.table(ulc, lf.ulc, quote=F, row.names=F, col.names=F, sep='   ')
                
                ts = paste(uid, ra, dec, sep='   ')
                write(ts, lf.urd, append=T)
            }
        }
        msg = paste0('   >> preparing [',field,']: ',round(i.fnl*100/n.fnl,1),' %    \r')
        message(msg, appendLF = F)
    }
    print('')
}


f.out = 'm33_i_catalog.dat'
lf.out = paste0(dir, f.out)
ts = '#   sid          ra        dec        m        e   n.obs n.field'
write(ts, lf.out)

urd = read.table(lf.urd)
f.map = 'uid_map.dat'
lf.map = paste0(map.dir, f.map)
map = read.table(lf.map, sep=',')
n.map = nrow(map)
n.col = ncol(map)
for (i in 1:n.col) map[,i] = as.character(map[,i])
for (i.map in 1:n.map) {
    ts = map[i.map,]
    uids = as.character(ts[1, !is.na(ts)])
    n.uids = length(uids)
    n.field = 0
    slc.flag = F
    for (i in 1:n.uids) {
        uid = uids[i]
        f.ulc = paste0(uid, '.ulc')
        lf.ulc =  paste0(ulc.dir, f.ulc)
        if (file.exists(lf.ulc)) {
            lc = read.table(lf.ulc)
            if (!slc.flag) {
                slc = lc
                slc.flag = T
            } else {
                slc = rbind(slc, lc)
            }
            n.field = n.field + 1
        }
    }
    if (slc.flag) {
        n.obs = nrow(slc)
        mean.mag = mean(slc[,2])
        if (n.obs > 1) {
            err = sd(slc[,2]) / sqrt(n.obs)
        } else {
            err = slc[1,3]
        }
        uid = uids[1]
        idx = which(urd[,1] == uid)
        i = 2
        while (length(idx) == 0 & i <= n.uids) {
            uid = uids[i]
            idx = which(urd[,1] == uid)
            i = i + 1
        }
        ra = urd[idx, 2]
        dec = urd[idx, 3]
        sid = uid
        f.slc = paste0(sid, '.slc')
        lf.slc = paste0(slc.dir, f.slc)
        write.table(slc, lf.slc, quote=F, row.names=F, col.names=F, sep='   ')

        ts = sprintf('%11s%11.5f%11.5f%9.3f%8.3f%5i%6i',
            sid, ra, dec, mean.mag, err, n.obs, n.field)
        write(ts, lf.out, append=T)
    }
    msg = paste0(' >> combining: ',round(i.map*100/n.map,2),' %    \r')
    message(msg, appendLF = F)
}
print('                            ')
