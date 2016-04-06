mediaSmooth = function(measure){
    nL = length(measure)
    measure2 = measure
    for(j in 2:(nL-1)){
        aa = max(1,j-2)
        bb = min(j+2,nL)
        mV = median(measure[aa:bb])
        measure2[j] = mV
    }
    return(measure2)
}


plot_fake=function(starObs,x,p0,sData){
    shift = sData$shift
    sData = sData$fLcurve
    minT = min(starObs$V1)
    maxT = max(starObs$V1)
    deltaMJD = maxT-minT
    deltaMJD = ceiling(deltaMJD/p0) *p0
    colDet = (sData[,1]-min(sData[,1])+shift) %/% (deltaMJD ) + 1
    fTime = sData[,1] -min(sData[,1]) +shift  + min(starObs$V1)
    plot(fTime,sData[,2],col=c("blue","red","blue","red","blue","red")[colDet],
         pch=20,cex=2,xlab="MJD",ylab="Mag")
    abline(v=min(starObs$V1)+p0*1:100,lty=2)
    nCyc = (maxT - minT) %/% p0 +1
    newX = seq(minT, minT+nCyc*p0, by=2)
    points(starObs[,1:2],pch=20,xlim=range(newX))
    newY = x$gp_predict(newX, 0)
    lines(newX,newY$predy,col="green",lwd=2)
#     lines(2*nCyc*p0 - (newX-minT)+minT,
#           newY$predy,col="green",lwd=2)
#     points(fTime,sData[,2],col=c("blue","red")[colDet],
#          pch=20,cex=2,xlab="MJD",ylab="Mag")
}

smryStat=function(data){
    tmp = c(mean(data),median(data),sd(data),
            quantile(data,probs=c(0.05,0.95)))
    names(tmp) = c("mean","median","sd","q05","q95")
    return(tmp)
}

#generate fake lightcurves
#which night to have observations?
getNights = function(){
fdNgt = "~/M33Project/inputfiles/"
fields=c(0:9,letters[1:19])
oneField=sample(fields,1)
ngtMJD=read.table(paste0(fdNgt,"m33i-",oneField,"-ngt.dat"))
while(dim(ngtMJD)[1]<40){
    oneField=sample(fields,1)
    ngtMJD=read.table(paste0(fdNgt,"m33i-",oneField,"-ngt.dat"))    
}
nObsWeight=read.table(paste0(fdNgt, "m0",oneField,"-obs-hst.dat"),
                      header=TRUE)
nObs=with(nObsWeight, sample(Fraction,1,prob=frequency))
if(nObs<40) nObs = 40
nObs = min(nObs, dim(ngtMJD)[1])
nights=sample(ngtMJD[,1],nObs,replace=FALSE)
nights=sort(nights)
return(nights)
}

##plot gaussian proecess fit
gp_plot = function(starObs,gp_x, p0,addon=FALSE){
    minT = min(starObs$V1)
    maxT = max(starObs$V1)
    nCyc = (maxT - minT) %/% p0 +1
    newX = seq(minT, minT+nCyc*p0*1.01, by=2)
    if(addon) 
        points(starObs[,1:2],pch=20,xlim=range(newX))
    else
        plot(starObs[,1:2],pch=20,xlim=range(newX))
    newY = gp_x$gp_predict(newX, 0)
    lines(newX,newY$predy,pch=20,col="red",lwd=2)
    newY = gp_x$gp_predict(newX, 1)
    newY2 = gp_x$gp_predict(newX, 3)$predy-mean(starObs$V2)
    lines(newX,newY$predy+newY2,
          col="blue",lwd = 2)
    newY = gp_x$gp_predict(newX, 2)
    lines(newX,newY$predy,col="black",lty=2)
}

period_output=function(freqC,smry,p0){
    #freqC = residSeq[,1]
    freqC2 = c(freqC, rev(freqC))
    perc = c(smry[,4], rev(smry[,5]))
    plot(freqC, smry[,2], 
         xlab = "Frequency", ylab = "RSS", ylim=range(perc)*0.9,
         type="l",main=length(starModel2$MJD))#
    polygon(freqC2,perc,col="grey",border=NA)
    lines(freqC, smry[,2],col="red",lwd=2)
    #lines(residSeq[,1],diag(cov(t(residSeq[,-1]))))
    residSig = mean(smry[,2]) - smry[,2]
    residSig = residSig /smry[,3]
    abline(v=1/p0,col="blue",lwd = 3)
    pHat = 1/freqC[which.max(residSig)]
    abline(v=1/pHat,col="blue",lwd=3,lty=3)

    return(pHat)
}