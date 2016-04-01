psign = function(x) {
    if (x == 0) {
        return(1)
    } else {
        return(sign(x))
    }
}

ra2dra = function(h, m, s) {
    dra = 15*(h + m/60 + s/3600)
    return(dra)
}

dec2ddec = function(d, m, s) {
    ddec = psign(d) * (abs(d) + m/60 + s/3600)
    return(ddec)
}
