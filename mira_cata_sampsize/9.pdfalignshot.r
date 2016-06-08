
dir = '~/Work/m33_phaseII/mira_cata_sampsize/snapshots/'
fs = list.files(dir)
idx = order(fs)
fs = fs[idx]
nfs = length(fs)


idx = gregexpr(pattern ='_',fs)
strpos = rep(NA, nfs)
for (i in 1:nfs) {
    strpos[i] = idx[[i]][1]
}

roots = substr(fs,rep(1,nfs),strpos-1)
uroots = unique(roots)
nurt = length(uroots)

lens = as.data.frame(matrix(NA, ncol=2, nrow=nurt))
for (i in 1:nurt) {
    idx = roots == uroots[i]
    sub = fs[idx]
    lens[i,1] = length(sub)
    ret = gregexpr(pattern = 'J', sub[1])
    if (ret[[1]][1] > 1)
        lens[i,2] = 1
    else
        lens[i,2] = 0
}

idx = lens[,2] == 1
jroots = uroots[idx]
droots = uroots[!idx]
jlens = lens[idx,]
dlens = lens[!idx,]
idx = rev(order(jlens[,1]))
jroots = jroots[idx]
idx = rev(order(dlens[,1]))
droots = droots[idx]


findtype = function(f) {
    s = gsub(uroots[i],'',f)
    s = gsub('_','',s)
    return(substr(s,1,1))
}

uroots = c(jroots,droots)



starts = seq(1, nurt, 88)
ends = seq(88, nurt, 88)
ends = c(ends, nurt)
for (istart in 1:length(starts)) {
f.out = paste0('~/Work/m33_phaseII/mira_cata_sampsize/snapshots_pdf/batch_',istart,'.tex')
f.tex = paste0('~/Work/m33_phaseII/mira_cata_sampsize/snapshots_pdf/file_',istart,'.tex')
    write('',f.out)
for (i in starts[istart]:ends[istart]) {
    idx = roots == uroots[i]
    sub = fs[idx]
    nsub = length(sub)
    ts = '\\bgroup
\\def\\arraystretch{0}
\\setlength{\\tabcolsep}{0pt}
\\begin{tabular}{@{}|p{\\w} | p{\\w} | p{\\w} | p{\\w} | p{\\w} | p{\\w} | p{\\w}|@{}}
\\hline'
    write(ts, f.out, append=T)
    f.eps = paste0(uroots[i],'_img.eps')
    ts = paste0('\\includegraphics[width=\\fw, height=\\fw] {',
        f.eps,'} &')
    write(ts, f.out, append=T)
    sub = sub[sub != f.eps]
    nsub = length(sub)
    if (nsub > 0) {
        types = rep(NA, nsub)
        for (k in 1:nsub) types[k] = findtype(sub[k])
        idx = which(types == 'w')
        nidx = length(idx)
        if (nidx > 0) {
            ssub = sub[idx]
            if (nidx > 2) nidx = 2
            for (foo in 1:nidx) {
                ts = paste0('\\includegraphics[width=\\fw, height=\\fw] {',
                    ssub[foo],'} &')
                write(ts, f.out, append=T)
            }
            if (nidx < 2) {
                ts = paste0('&')
                write(ts, f.out, append=T)
            }
        } else {
            for (foo in 1:2) {
                ts = paste0('&')
                write(ts, f.out, append=T)
            }
        }

        idx = which(types == 'x' | types == 'y' | types == 'z' )
        nidx = length(idx)
        if (nidx > 0) {
            ssub = sub[idx]
            for (foo in 1:nidx) {
                ts = paste0('\\includegraphics[width=\\fw, height=\\fw] {',
                    ssub[foo],'} &')
                write(ts, f.out, append=T)
            }
            if (nidx < 3) {
                for (foo in 1:(3-nidx)) {
                    ts = paste0('&')
                    write(ts, f.out, append=T)
                }
            }
        } else {
            for (foo in 1:3) {
                ts = paste0('&')
                write(ts, f.out, append=T)
            }
        }
        

        idx = which(types == 'J')
        nidx = length(idx)
        if (nidx > 0) {
            ssub = sub[idx]
            for (foo in 1:nidx) {
                ts = paste0('\\includegraphics[width=\\fw, height=\\fw] {',
                    ssub[foo],'} \\\\')
                write(ts, f.out, append=T)
            }
        } else {
            ts = paste0('\\\\')
            write(ts, f.out, append=T)
        }
    } else {
        for (foo in 1:5) {
            ts = paste0('&')
            write(ts, f.out, append=T)
        }
        ts = paste0('\\\\')
        write(ts, f.out, append=T)
    }
    ts = '\\hline
\\end{tabular}
\\egroup


'
    write(ts, f.out, append=T)
    ## ts = paste0('! ',i)
    ## write(ts, f.out, append=T)
}

ts = '\\documentclass[12pt]{article}
\\usepackage{graphicx}
\\graphicspath{ {../snapshots/} }
\\usepackage[margin=0.05in]{geometry}

\\newcommand{\\w}{2.8cm}
\\newcommand{\\fw}{2.8cm}

\\title{Snapshots of M33 Mira Candidates}
\\date{May 17 2016}

\\begin{document}
\\maketitle
'
write(ts, f.tex)
sf.out = paste0('batch_',istart,'.tex')
ts = paste0('\\input{',sf.out,'}
\\end{document}')
write(ts, f.tex, append=T)

}
