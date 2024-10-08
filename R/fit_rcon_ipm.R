##
## 2021 Revision: Baseret på ren R-kode (bortset fra beregningen af
## nogle traces)
##

##
## Dette er for iterativ fitting og for user defined fitting...
##

rconIPM_r <- function(object, K0, control=object$control, trace=object$trace){
  
  vccN  <- getSlot(object, "vcc")
  eccN  <- getSlot(object, "ecc")
  iR    <- getSlot(object, "intRep")
  vccI  <- getSlot(iR, "vccI")
  eccI  <- getSlot(iR, "eccI")

  idxb    <- sapply(vccN, length) == 1;  ##idxb    <- sapply(vccN, nrow)==1
  vccN.at <- vccN[idxb]
  vccI.at <- vccI[idxb]
  vccN.co <- vccN[!idxb]
  vccI.co <- vccI[!idxb]
  
  idxb    <- sapply(eccN, length) == 1;  ##idxb    <- sapply(eccN, nrow)==1
  eccN.at <- eccN[idxb]
  eccI.at <- eccI[idxb]
  eccN.co <- eccN[!idxb]
  eccI.co <- eccI[!idxb]
  
  S  <- dataRep(object, "S")
  n  <- dataRep(object, "n")

  maxit       <- control$maxouter
  logL0       <- prevlogL <- ellK(K0, S, n - 1)
  logLeps     <- control$logLeps #* abs(logL0)

  logL.vec    <- rep(NA, maxit)
  itcount     <- 1
  converged   <- FALSE
  
  Kwork <- K0

  eccfit <- control$eccfit
  vccfit <- control$vccfit

  while(!converged){

    if (vccfit){
      ##Kwork <- fitIPSset(vccI.at, Kwork, S, n, control=control)$K
      Kwork <- fitNR2    (vccI.at, Kwork, S, n, type="vcc", control=control, trace=trace)$K
      Kwork <- fitNR2    (vccI.co, Kwork, S, n, type="vcc", control=control, trace=trace)$K
    }
    if (eccfit){
      ##Kwork <- fitIPSedge(eccI.at, Kwork, S, n, type='ecc',control=control)$K
      Kwork <- fitNR2     (eccI.at, Kwork, S, n, type='ecc', control=control, trace=trace)$K
      Kwork <- fitNR2     (eccI.co, Kwork, S, n, type="ecc", control=control, trace=trace)$K
    }
    
    logL    <- ellK(Kwork, S, n-1);
    dlogL   <- logL - prevlogL
    if (trace >= 3)
      cat("...rconIPM_r iteration", itcount, "logL:", logL, "dlogL:", dlogL, "\n")
    logL.vec[itcount]  <- logL
    if ((logL - prevlogL) < logLeps || itcount>=maxit)
      converged <- TRUE
    else {
      prevlogL <- logL;
      itcount  <- itcount + 1
    }
  }
  
  coef <- K2theta(object,Kwork, scale='original')
  vn   <- unlist(lapply(getcc(object),names))
  names(coef) <- vn

  if (!is.null(control$vcov)){
    J  <- getScore(object, K=Kwork)$J
    dimnames(J) <- list(vn, vn)
  } else {
    J <- NULL
  }
  
  ans <- list(K=Kwork, logL=logL, coef=coef, J=J, logL.vec=logL.vec[1:itcount])
  return(ans)
}


##
## MATRIX VERSION; reduced amount of copying
##
fitNR2 <- function(x, K, S, n, varIndex=1:nrow(K), type="ecc", control, trace){

  f         <-  n-1;
  itmax     <-  control$maxinner
  eps       <-  control$deltaeps

  if (length(x)){
    ##cat("fitNR(start): type:", type, "logL:", ellK(K,S,n-1), "\n")

    for (ii in 1:length(x)){
      ## Current generator
      gen    <- x[[ii]] ##print(gen)

      ## Make gen have two columns if it does not already have so
      if (ncol(gen)==1)
        genmat <- cbind(gen,gen)
      else
        genmat <- rbind(gen,gen[,2:1])
      
      idx   <- sort(unique(as.numeric(gen)))
      cidx  <- setdiff(varIndex, idx);       
      
      ## gen2: Version of gen which matches the lower dimensional matrices used later
      gen2           <- apply(gen, 2, match, idx)
      dim(gen2)      <- dim(gen)
      ##storage.mode(gen2) <- "double" ## Why???

      subt <- 0
      if (length(cidx) > 0)
        subt <- K[idx, cidx, drop=FALSE] %*%
          solve.default(K[cidx, cidx, drop=FALSE], K[cidx, idx, drop=FALSE])

      S2 <- S[idx,idx,drop=FALSE]
      ####trSS2 <- .Call("trAW", gen2, S2, PACKAGE="gRc")
      trSS2 <- trAW(gen2, S2)

      itcount   <-  0
      prev.adj2 <-  0

      repeat{
        currparm  <- unique(K[genmat])
        Sigma2    <-  solve.default(K[idx,idx]-subt)
        ####trIS      <-  .Call("trAW", gen2, Sigma2, PACKAGE="gRc")
        ####trISIS    <-  .Call("trAWBW", gen2, Sigma2, gen2, PACKAGE="gRc")
        trIS      <-  trAW(gen2, Sigma2)
        trISIS    <-  trAWBW(gen2, Sigma2, gen2)

        Delta2    <-  trIS - trSS2
        adj2      <-  Delta2 /(trISIS + Delta2^2/(2) )
        K[genmat] <-  K[genmat]+adj2
        dadj2     <-  (adj2 - prev.adj2)
        prev.adj2 <-  adj2
        itcount   <-  itcount + 1
                                        #logL <- ellK(K,S,n-1)
                                        #dlogL <- logL-prevlogL
                                        #print(c(itcount, logL, dlogL, abs(dadj2), min(eigen(K)$values)))
        
        if (trace>=40){
          ##print(gen)
          cat("trIS", trIS, "trISIS", trISIS, "trSS2", trSS2, "\n")
          cat("....Modified Newton iteration", itcount,
            "currparm:", currparm, 
            "Delta2:", Delta2,
            "parm change", dadj2,"\n")
        }
        if ( (abs(dadj2)< eps) | (itcount>=itmax) )
          break() 
                                        # prevlogL <- logL
      }
    }
  }
  logL <- ifelse (control[["logL"]], ellK(K,S,n-1), -9999)
  ans  <- list(K=K, logL=logL)
  return(ans)
}





##
## 2021 revision : Baseret på kald af rconipm (c kode); al c-code til
## model fitting er taget ud.
##

## rconIPM_c <- function(object, K0,
##                      control=object$control, trace=object$trace){

##   if (trace>=2)
##     cat("..fitting with rconIPM_c (C version):\n");

##   t0 <- proc.time()
##   S       <- object$dataRep$S
##   nobs    <- object$dataRep$n
##   Kstart  <- object$Kstart

##   ctrl <- control
##   logL      = 0
##   converged = 1

##   deltaeps  = ctrl$deltaeps
##   maxouter  = ctrl$maxouter
##   maxinner  = ctrl$maxinner

##   eccfit <- control$eccfit
##   vccfit <- control$vccfit

##   if (vccfit)
##     vcc <- object$intRep$vccI
##   else
##     vcc <- NULL

##   if (eccfit)
##     ecc <- object$intRep$eccI
##   else
##     ecc <- NULL

##   glist   <- c(vcc, ecc)

##   logL0       <- prevlogL <- ellK(K0,S,nobs-1)
##   logLeps     <- ctrl$logLeps * abs(logL0)

##   t00 <- proc.time()
##   ##cat("maxouter:", maxouter,"\n")
##   tmp <- .Call("rconipm", S=S, nobs=nobs-1, K=K0, Glist=glist,
##                maxouter=maxouter, maxinner=maxinner,
##                logLeps=logLeps, deltaeps=deltaeps,
##                converged=converged, debug=trace,
##                PACKAGE="gRc")
##   Kwork <- tmp[[1L]]; logL <- tmp[[4L]]
##   ##Kwork <- ansC[[1]]
##   ##cat("C-call:", proc.time()-t00, "\n")
##   #cat("maxouter:", maxouter, "\n")

##   coef <- K2theta(object,Kwork, scale='original')
##   vn   <- unlist(lapply(getcc(object),names))
##   names(coef) <- vn

##   if (object$method == "ipms"){ ## IPM without finalizing with finding score
##     J <- NULL
##   } else {
##     if (!is.null(control$vcov)){
##       J  <- getScore(object,K=Kwork)$J
##       dimnames(J) <- list(vn, vn)
##     } else {
##       J <- NULL
##     }
##   }

##   if (trace>=2)
##     cat("..ipm, logL:", logL, "Time:", proc.time()-t0, "\n")

##   ##cat("exit rconIPM_c: Kwork: \n"); print(Kwork)
##   ans <- list(K=Kwork, logL=logL, coef=coef, J=J)
##   return(ans)
## }























#######################################################################################
##
## 2021: Alt nedenfor er kommenteret ud, da det slet ikke kaldes nogen steder
##
#######################################################################################


## ## 2021 revision: fitNR bruges slet ikke

## ## MATRIX VERSION
## fitNR <- function(x, K, S, n, varIndex=1:nrow(K),type="ecc",control,trace){
##   ##cat("--fitNR", type, "\n")
##   ##print(x)
##   if (length(x)){

##     ##cat("fitNR(start): type:", type, "logL:", ellK(K,S,n-1), "\n")
##     for (i in 1:length(x)){
##       cl    <- x[[i]];
##       ##print(cl)

##       # Make cl have two columns if it does not already have so
##       if (ncol(cl) == 1)
##         clmat <- cbind(cl,cl)
##       else
##         clmat <- rbind(cl, cl[, 2:1])

##       idx   <- sort(unique(as.numeric(cl)))
##       cidx  <- setdiff(varIndex,idx);       
      
##       ## cl2: Version of cl which matches the lower dimensional matrices used later
##       cl2 <- apply(cl, 2, match, idx)
##       dim(cl2) <- dim(cl)
##       storage.mode(cl2) <- "double"
##       #print(cl2)
      
##       if (length(cidx) > 0)
##         subt <- K[idx, cidx, drop=FALSE] %*% solve.default(K[cidx, cidx, drop=FALSE], K[cidx, idx, drop=FALSE])
##         ##subt <- K[idx,cidx,drop=F]%*%cholSolve(K[cidx,cidx,drop=F])%*%K[cidx,idx,drop=F]
##       else
##         subt <- 0
      
##       val<-modNewt(K, S, n, idx, cl, cl2, clmat, subt, type, control, trace)
##       K <- val      
##     }
    
##   }
##   logL <- ifelse (control[["logL"]], ellK(K,S,n-1), -9999)
  
##   ##cat("fitNR(loop): type:", type, "logL:", ellK(K,S,n-1), "\n")
##   ##cat("fitNR: type:", type, "logL:", logL, "\n")

##   ans <- list(K=K, logL=logL)
##   return(ans)
## }


## ## 2021 revision: modNewt bruges slet ikke

## ## MATRIX VERSION
## modNewt <- function(K, S, n, idx, cl, cl2, clmat, subt, type, control,trace){
##   f         <-  n-1;
##   itcount   <-  0
##   itmax     <-  control$maxinner
##   eps       <-  control$deltaeps
##   prev.adj2 <-  0
##   ##print(cl)
##   trSS2     <-  trAW(cl, S)

## #   print("modNEWT")
## #   print(cl);
## #   print(cl2)
## #   print(clmat)
## #   print(subt)
  
## #  prevlogL <- ellK(K,S,n-1)
##   repeat{
##     ##Sigma2    <-  cholSolve(K[idx,idx]-subt)
##     Sigma2    <-  solve.default(K[idx,idx]-subt)
##                                         #print(cl2)
##     trIS      <-  trAW(cl2,Sigma2)
##     ##trISIS    <-  trAWBW(cl2, Sigma2, cl2)

##     ###trISIS    <-  .Call("trAWBW", cl2, Sigma2, cl2, PACKAGE="gRc")
##     trISIS    <-  trAWBW(cl2, Sigma2, cl2)
    
##     Delta2    <-  trIS - trSS2
##     #adj2     <-  Delta2 /(trISIS + 0.5*f*Delta2^2 )
##     ##adj2     <-  Delta2 /(trISIS + 2*f*Delta2^2 )
##     adj2      <-  Delta2 /(trISIS + 0.5*Delta2^2 )


##     K[clmat]  <- K[clmat]+adj2
##     dadj2     <- (adj2 - prev.adj2)
##     prev.adj2 <- adj2
##     itcount   <- itcount + 1
##     #logL <- ellK(K,S,n-1)
##     #dlogL <- logL-prevlogL
##     #print(c(itcount, logL, dlogL, abs(dadj2), min(eigen(K)$values)))

##     if (trace>=40)
##     cat("....Modified Newton iteration", itcount, "parm change", dadj2,"\n")
##     if ( (abs(dadj2)< eps) | (itcount>=itmax) )
##       break()

##    # prevlogL <- logL
##   }
##   return(K)
## }


## ## 2021 revision: fitIPSedge bruges slet ikke

## ## Fit edge {a,b} without fitting nodes {a} and {b}
## ##
## fitIPSedge <- function(x, K, S, n, varIndex=1:nrow(K),type,control=NULL){

##   if (length(x)){
##     my.complement <- function(cc) return(setdiff(varIndex, cc))
##     x.complements <- lapply(x, my.complement)
    
##     for (i in 1:length(x)){
##       cc  <- x[[i]]; #print(cc)
##       aa  <- x.complements[[i]]; #print(aa)
##       subt <- 0
      
##       if (length(aa) > 0)
##         subt <- K[cc, aa, drop=FALSE] %*% solve.default(K[aa, aa, drop=FALSE], K[aa, cc, drop=FALSE])
      
##       K11   <- K[cc, cc]  
##       KK    <- K11 - subt
##       a12   <- subt[1, 2]
##       s     <- S[cc[1], cc[2]]
##       sqrtD <- sqrt((2 * s * a12 + 1)^2 + 4 * s * (-s * a12^2 - a12 + s * prod(diag(KK))))
##       x1    <- (-(2 * s * a12 + 1) + sqrtD)/(-2 * s)
##       K[cc[1], cc[2]] <-  K[cc[2], cc[1]] <-x1
##     }
##   }
##   logL <- ifelse (control[["logL"]], ellK(K, S, n - 1), -9999)  
##   ans <- list(K=K, logL=logL)
##   return(ans)
## }


## ## 2021 revision: fitIPSset bruges slet ikke

## ## Classical IPS applied to sets {C_1,...,C_p}
## ##
## fitIPSset <- function(x, K, S, n, varIndex=1:nrow(K), control){
##   #print(x)
##   if (length(x)){
##     my.complement <- function(cc) return(setdiff(varIndex, cc))
##     x.complements <- lapply(x, my.complement)
    
##     if (length(x.complements[[1]]) == 0){
##       return(list(K=solve.default(S)))
##     }
    
##     for(j in 1:length(x)){
##       cc  <- x[[j]]
##       aa  <- x.complements[[j]]
##       K[cc,cc] <- solve( S[cc, cc, drop=FALSE] ) +
##         K[cc, aa, drop=FALSE] %*% solve.default(K[aa, aa, drop=FALSE], K[aa, cc, drop=FALSE])
##     }
##   }
  
##   logL <- ifelse (control[["logL"]], ellK(K,S,n-1), -9999)
##   ans <- list(K=K, logL=logL)
##   #print("JJJJJ")
##   return(ans)
## }

























































## ## Fit edge {a,b} without fitting nodes {a} and {b}
## ##
## ## MATRIX VERSION
## fitIPSedge <- function(x, K, S, n, varIndex=1:nrow(K), type, control=NULL){
##   #cat("fitIPSedge\n"); print(x)
##   if (length(x)){
##     my.complement <- function(cc) return(setdiff(varIndex, cc))
##     x.complements <- lapply(x, my.complement)
    
##     for (i in 1:length(x)){
##       cc  <- x[[i]]; #print(cc)
##       aa  <- x.complements[[i]]; #print(aa)
##       subt <- 0
      
##       if (length(aa) > 0)
##         subt <- K[cc, aa, drop=FALSE] %*% solve.default(K[aa, aa, drop=FALSE], %*% K[aa, cc, drop=FALSE])

##       K11   <- K[cc, cc]  
##       KK    <- K11 - subt
##       a12   <- subt[1, 2]
##       s     <- S[cc[1], cc[2]]
##       sqrtD <- sqrt((2 * s * a12 + 1)^2 + 4 * s * (-s * a12^2 - a12 + s * prod(diag(KK))))
##       x1    <- (-(2 * s * a12 + 1) + sqrtD) / (-2 * s)
##       K[cc[1], cc[2]] <-  K[cc[2], cc[1]] <- x1
##     }
##   }
##   logL <- ifelse (control[["logL"]], ellK(K, S, n - 1), -9999)  
##   ans <- list(K=K, logL=logL)
##   return(ans)
## }






##print(unlist(lapply(cl,paste,collapse=":")))
##print(unlist(lapply(cl2,paste,collapse=":")))
##print(idx); print(cidx)


# ## Represent a vcc (a list structure) as a matrix (for fast lookup)
# ##
# .vccMat <- function(x){
#   lapply(x, function(b){
#     b2 <- matrix(rep(unlist(b),2),nc=2)
#     b2})
# }

# ## Represent a ecc (a list structure) as a matrix (for fast lookup)
# .eccMat <- function(x){
#   lapply(x, function(b){
#     b2 <- matrix(unlist(b),nc=2,byrow=TRUE)
#     b2 <- rbind(b2,b2[,2:1])
#     b2})
#   } 


# fitNR <- function(x, K, S, n, varIndex=1:nrow(K),type="ecc",control,trace){

#   if (length(x)){
#     xmat <- switch(type,
#                    "ecc"={.eccMat(x)},
#                    "vcc"={.vccMat(x)}); ##  print(xmat)

#     #cat("fitNR(start): type:", type, "logL:", ellK(K,S,n-1), "\n")
#     for (i in 1:length(x)){
#       cl    <- x[[i]]; 
#       clmat <- xmat[[i]];
#       a     <- sort(unique(unlist(cl)))
#       cl2   <- lapply(cl, match, a); ## cl represented as indices in a compact matrix
      
#       idx   <- sort(unique(unlist(cl))); 
#       cidx  <- setdiff(varIndex,idx); 
      
#       if (length(cidx)>0)
#         subt <- K[idx,cidx,drop=FALSE]%*% solve(K[cidx,cidx,drop=FALSE], K[cidx,idx,drop=FALSE])
#         ##subt <- K[idx,cidx,drop=F]%*%cholSolve(K[cidx,cidx,drop=F])%*%K[cidx,idx,drop=F]
#       else
#         subt <- 0
#       val<-modNewt(K, S, n, idx, cl, cl2, clmat, subt, type, control, trace)
#       K <- val
#       #cat("fitNR(loop): type:", type, "logL:", ellK(K,S,n-1), "\n")
#     }
    
#   }
#   logL <- ifelse (control[["logL"]], ellK(K,S,n-1), -9999)
#   #cat("fitNR: type:", type, "logL:", logL, "\n")
#   ans <- list(K=K, logL=logL)
#   return(ans)
# }




# modNewt <- function(K, S, n, idx, cl, cl2, clmat, subt, type, control,trace){
#   f         <-  n-1;
#   itcount   <-  0
#   itmax     <- 25
#   eps       <-  0.01
#   prev.adj2 <-  0
#   trSS2     <-  trAW(cl, S)

#   #prevlogL <- ellK(K,S,n-1)
#   repeat{
#     ##Sigma2    <-  cholSolve(K[idx,idx]-subt)
#     Sigma2    <-  solve(K[idx,idx]-subt)
#     trIS      <-  trAW(cl2,Sigma2)
#     trISIS    <-  trAWBW(cl2, Sigma2, cl2)
    
#     Delta2    <-  trIS - trSS2
#     #adj2     <-  Delta2 /(trISIS + 0.5*f*Delta2^2 )
#     ##adj2     <-  Delta2 /(trISIS + 2*f*Delta2^2 )
#     adj2      <-  Delta2 /(trISIS + 0.5*Delta2^2 )
    
#     K[clmat]  <- K[clmat]+adj2
#     dadj2     <- (adj2 - prev.adj2)
#     prev.adj2 <- adj2
#     itcount   <- itcount + 1
#     logL <- ellK(K,S,n-1)
#     #dlogL <- logL-prevlogL
#     ## print(c(itcount, logL, dlogL, abs(dadj2), min(eigen(K)$values)), digits=15)
#     if (trace>=4)
#     cat("....Modified Newton iteration", itcount, "parm change", dadj2,"\n")
#     if ( (abs(dadj2)< eps) | (itcount>=itmax) )
#       break()

#    # prevlogL <- logL
#   }
#   return(K)
# }


#     print(c(trISold,trISISold))
#     Kinv <- solve(K)
#     trIS      <-  trAW(cl,Kinv)
#     trISIS    <-  trAWBW(cl, Kinv, cl)
#     print(c(trIS,trISIS))

#     if (abs(trISold-trIS)>0.01){
#       print(idx)
#       print(cl)
#       print(cl2)
      
#       stop()
#     }



# ## MATRIX VERSION
# modNewt2 <- function(K, S, n, idx, cl, cl2, clmat, subt, type, control,trace){
#   f         <-  n-1;
#   itcount   <-  0
#   itmax     <-  control$maxinner
#   eps       <-  control$deltaeps
#   prev.adj2 <-  0
#   ##print(cl)
#   trSS2     <-  trAW(cl, S)

# #   print("modNEWT")
# #   print(cl);
# #   print(cl2)
# #   print(clmat)
# #   print(subt)
  
# #  prevlogL <- ellK(K,S,n-1)
#   repeat{
#     ##Sigma2    <-  cholSolve(K[idx,idx]-subt)
#     Sigma2    <-  solve.default(K[idx,idx]-subt)
#                                         #print(cl2)
#     trIS      <-  trAW(cl2,Sigma2)
#     ##trISIS    <-  trAWBW(cl2, Sigma2, cl2)

#     trISIS    <-  .Call("trAWBW", cl2, Sigma2, cl2, PACKAGE="gRc")
    
#     Delta2    <-  trIS - trSS2
#     #adj2     <-  Delta2 /(trISIS + 0.5*f*Delta2^2 )
#     ##adj2     <-  Delta2 /(trISIS + 2*f*Delta2^2 )
#     adj2      <-  Delta2 /(trISIS + 0.5*Delta2^2 )


#     K[clmat]  <- K[clmat]+adj2
#     dadj2     <- (adj2 - prev.adj2)
#     prev.adj2 <- adj2
#     itcount   <- itcount + 1
#     #logL <- ellK(K,S,n-1)
#     #dlogL <- logL-prevlogL
#     #print(c(itcount, logL, dlogL, abs(dadj2), min(eigen(K)$values)))

#     if (trace>=4)
#     cat("....Modified Newton iteration", itcount, "parm change", dadj2,"\n")
#     if ( (abs(dadj2)< eps) | (itcount>=itmax) )
#       break()

#    # prevlogL <- logL
#   }
#   return(K)
# }
