write.ds9reg.image = function(f.reg, xs, ys, r = 5, color = 'green', append = F) {
    n = length(xs)
    if (n > 0) {
        ts = paste0('image;circle(',xs[1],',',ys[1],',',r,')#color=',color)
        write(ts, f.reg, append = append)
    }
    if (n > 1) {
        for (i in 2:n) {
            ts = paste0('image;circle(',xs[i],',',ys[i],',',r,')#color=',color)
            write(ts, f.reg, append = T)
        }
    }
}
