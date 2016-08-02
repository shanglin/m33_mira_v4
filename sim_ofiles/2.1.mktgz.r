lcdir = '~/Work/m33_phaseII/sim_ofiles/flc_mira/'
outdir = '~/Work/m33_phaseII/sim_ofiles/simulated_light_curves/'

f.lst = '~/Work/m33_phaseII/sim_ofiles/fakepl.lst'
lst = read.table(f.lst)
nlst = nrow(lst)


fmt = '%11.4f%9.3f%9.3f'
for (i in 1:nlst) {
    oid = lst[i,2]
    lid = lst[i,1]
    iid = lst[i,3]
    oid = paste0(oid)
    while(nchar(oid)<5) oid = paste0('0',oid)
    f = paste0('mira_',oid,'_',iid,'.flc')
    
    sid = paste0(lid)
    while (nchar(sid)<6) sid = paste0('0',sid)
    
    lf = paste0(lcdir, f)
    lc = read.table(lf)
    out = do.call('sprintf',c(fmt,lc))
    f.out = paste0('lc',sid,'.dat')
    lf.out = paste0(outdir, f.out)
    write(out, lf.out)
    msg = paste0('  >> ',round(i*100/nfs, 3),' %           \r')
    message(msg, appendLF=F)
}
print('')
