search.overdense = function(apl, r0 = 70, step = 10) {
    n.star.max = 0
    x0 = 0
    y0 = 0
    x0s = seq(r0, max(apl[,2]) - r0, step)
    y0s = seq(r0, max(apl[,3]) - r0, step)
    for (x in x0s) {
        for (y in y0s) {
            idx = (apl[,2] - x)^2 + (apl[,3] - y)^2 < r0^2
            n.star = sum(idx)
            if (n.star > n.star.max) {
                n.star.max = n.star
                x0 = x
                y0 = y
            }
        }
    }
    x0s = seq(x0 - 2*step, x0 + 2*step, 1)
    y0s = seq(y0 - 2*step, y0 + 2*step, 1)
    for (x in x0s) {
        for (y in y0s) {
            idx = (apl[,2] - x)^2 + (apl[,3] - y)^2 < r0^2
            n.star = sum(idx)
            if (n.star > n.star.max) {
                n.star.max = n.star
                x0 = x
                y0 = y
            }
        }
    }
    return(c(x0,y0))
}

