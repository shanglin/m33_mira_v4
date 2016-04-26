psign = function(x) {
   ret = sign(x)
   ret[ret==0] = 1
   return(ret)
}

ra2dra = function(h, m, s) {
    dra = 15*(h + m/60 + s/3600)
    return(dra)
}

dec2ddec = function(d, m, s) {
    ddec = psign(d) * (abs(d) + m/60 + s/3600)
    return(ddec)
}


############################### add on April 26, 2016
degree2rad = function(d) {
    return(pi * d / 180)
}
rad2degree = function(r) {
    return(180 * r / pi)
}

rd2galactic = function(r, d, epoch = 'J2000', unit = 'degree') {
    
    if (epoch == 'B1950' | epoch == '1950' | epoch == 'B1950.0') {
        r0 = degree2rad(192.25)
        d0 = degree2rad(27.4)
        l0 = degree2rad(303)
    } else if (epoch == 'J2000' | epoch == '2000' | epoch == 'J2000.0') {
        r0 = degree2rad(192.8595)
        d0 = degree2rad(27.1284)
        l0 = degree2rad(302.9320)
    } else {
        stop('epoch = J2000 or B1950 ?')
    }

    if (unit != 'degree') stop('Not support radian input!')

    r = degree2rad(r)
    d = degree2rad(d)

    l = l0 - atan(cos(d)*sin(r-r0)/(sin(d)*cos(d0)-cos(d)*sin(d0)*cos(r-r0)))
    b = asin(sin(d)*sin(d0)+cos(d)*cos(d0)*cos(r-r0))

    ret = cbind(rad2degree(l), rad2degree(b))
    colnames(ret) = c('l','b')
    return(ret)
}

## examples:
## r = c(269.9, 272.3)
## d = c(-42.68, -40.815)
## ret = rd2galactic(r, d)
