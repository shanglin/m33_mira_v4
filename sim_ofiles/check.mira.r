dir = '~/Work/m33_phaseII/sim_ofiles/flc_mira/'
ids = list.files('~/Work/m33_phaseII/lmc_ofiles/mira.lcs/')
ids = gsub('OGLE-LMC-LPV-','',ids)
ids = gsub('.dat','',ids)
ids = as.numeric(ids)
ids = ids[ids>9999]
n = length(ids)
for (i in 1:n) {
    id = ids[i]
    msg = paste0('  >> ',round(i*100/n,2),' %   \r')
    message(msg, appendLF=F)
    cmd = paste0('awk \'$2>99\' ',dir,'mira_',id,'*.flc >> check.mira.res.dat')
    system(cmd)
    ## stop()
}
