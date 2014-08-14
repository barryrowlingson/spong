
makeLineStyle <- function(linestyle=list()){
    defaults = list(
        col="black",
        lwd=2,
        lty=1,
        lend=1,
        ljoin=0,
        lmitre=10
        )
    lapply(names(linestyle),function(x){defaults[[x]]<<-linestyle[[x]]})
    defaults
        
}

makeFillStyle <- function(fillstyle=list()){
    defaults = list(
        density=NULL,
        col="#FFFFFF80"
        )
    lapply(names(fillstyle),function(x){defaults[[x]]<<-fillstyle[[x]]})
    defaults        
}

makePointStyle <- function(pointstyle=list()){
    defaults = list(
        col="black",
        pch=1,
        cex=1
        )
    lapply(names(pointstyle),function(x){defaults[[x]]<<-pointstyle[[x]]})
    defaults        
}

