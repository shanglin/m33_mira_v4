phot.dir = '~/Work/m33_mira/phot/i/'
fields = list.files(phot.dir, pattern='....allframe')
for (lfield in fields) {
    field = substr(lfield,1,3)
    print(field)
    inf.dir = paste0(phot.dir, field, '_allframe/')
    f.inf = paste0(field, '.inf')
    lf.inf = paste0(inf.dir, f.inf)
    con = file(lf.inf, 'r')
    inf = readLines(con)
    close(con)
    jds = substr(inf,64,78)
    jds = as.numeric(jds)
    jds = jds[order(jds)]
    n = length(jds)
    d.jds = jds[2:n] - jds[1:(n-1)]
    print(min(d.jds))
    if (min(d.jds) < 0.0005) stop()
}
