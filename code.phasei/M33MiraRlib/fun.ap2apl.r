ap2apl = function(f.ap, idx.ap,
    x0s = c(), y0s = c(), r0s = c(), edge.pix = 30) {
    n.bad = length(x0s)
    if (length(y0s) != n.bad | length(r0s) != n.bad)
        stop(' x0s, y0s, r0s must have the same dimension.')
    f.apl = gsub('.ap','.tmpl',f.ap)
    f.mag = gsub('.ap','.tmp1',f.ap)
    f.err = gsub('.ap','.tmp2',f.ap)
    cmd = paste0("awk 'NR>3&&NR%3==2' ",f.ap,' > ',f.mag)
    system(cmd)
    cmd = paste0("awk 'NR>3&&NR%3==0' ",f.ap,' > ',f.err)
    system(cmd)
    mag = read.table(f.mag)
    err = read.table(f.err)
    cmd = paste0('rm -f ',f.mag)
    system(cmd)
    cmd = paste0('rm -f ',f.err)
    system(cmd)
    id = mag[,1]
    x = mag[,2]
    y = mag[,3]
    m = mag[,3 + idx.ap]
    e = err[,3 + idx.ap]
    sky = err[,1]
    apl = cbind(id,x,y,m,e,sky)
    idx = m < 90 & m > -90 & e < 9 & x > edge.pix & x < max(x) - edge.pix & y > edge.pix & y < max(y) - edge.pix
    apl = apl[idx,]
    if (n.bad > 0) {
        for (i in 1:n.bad) {
            x0 = x0s[i]
            y0 = y0s[i]
            r0 = r0s[i]
            idx = (apl[,2] - x0)^2 + (apl[,3] - y0)^2 > r0^2
            apl = apl[idx,]
        }
    }
    return(apl)
}
