######################################################################
#' @title Generating class for RCON / RCOR models
#' @description Implementation of generating classes for RCOX
#'   models. These functions are not intended to be called by the
#'   user.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @name generating-class
######################################################################
#'
## ' @aliases as.V is.V as.L is.L as.L2 is.L2 matchVL matchLL2 matchVL2
## '   unionLL unionL2L2 setequalLL setdiffLL addVL2 is.elementVL
## '   is.subsetLL match.containsLL2
## '   toLisp toLisp.default toLisp.list toLisp.cc
## '   
## '   as.cc.default as.cclist print.cc print.cclist print.atom
## '   maximalSetL2
NULL

## as.V <- function(x){
##   as.atom(x)
## }

is.V <- function(x){
  is.numeric(x) | is.character(x) 
}

## as.L <- function(x){
##     if (!is.L(x)) stop("Can not create L\n")
##     as.cc(x)
## }

is.L <- function(x){
  is.list(x) && all(sapply(x, is.V)) 
}

as.L2 <- function(x){
    if (!is.L2(x)) stop("Can not create L2\n")
    as.cclist(x)
}

is.L2 <- function(x){
  is.list(x) && all(sapply(x, is.L)) 
}

## Matching
##
matchVL <- function(x,y){
  z<- which(sapply(y, setequal, x))
  if (length(z)==0)
    z <- NA
  z
}

matchLL2 <- function(x,y){
  z <- which(sapply(y, function(d) setequalLL(x,d)))
  if (length(z) == 0)
    z <- NA
  z 
}

## matchVL2 <- function(x,y){
##   z <- which(sapply(y, function(d) {
##     u <- matchVL(x,d)
##     any(!is.na(u))
##   }))
##   if (length(z)==0)
##     z <- NA
##   z 
## }


## Union
##

## unionLL <- function(x,y){
##   as.cc(unique(listOrder(c(x, y))))
## }

unionL2L2 <- function(x,y){
  if (length(y)==0)
    return(x)
  v<-unique(listOrder(c(x,y)))
  as.L2(v)
}

## Miscellaneous
setequalLL <- function(x,y){
  !(any(is.na(sapply(x, matchVL, y))) | any(is.na(sapply(y, matchVL, x))))
}

setequalLL <- function(x,y){
  if (length(x)!=length(y))
    return(FALSE)
  work <- ans <- rep(NA,length(x))
  for(ii in 1:length(x)){
    xx <- x[[ii]]
    for (jj in 1:length(y)){
      if (setequal(xx, y[[jj]])){
        ans[ii] <- jj    
        work[jj] <- ii
        break()
        }
    }
  }
  !any(is.na(c(ans,work)))
}



setdiffLL <- function(x,y){
  if (length(y)==0)
    return(x)
  idx <- sapply(y, function(yy)matchVL(yy,x))
  x[-idx]
}

## addVL2 <- function(x,y){
##   if (length(y)){
##     z <- matchVL2(x,y)
##     ##print(z)
##     if (!is.na(z))
##       return(NA)
##   }
##   unionL2L2( as.L2(list(as.cc(x))), y)
## }


is.elementVL <- function(x,y)
  !is.na(matchVL(x,y))
  
is.subsetLL <- function(x,y){
  all(sapply(x, function(ee) is.elementVL(ee,y)))
}

match.containsLL2 <- function(x,y){
  x<- which(sapply(y, function(yy) is.subsetLL(x,yy))  )
  if (length(x)==0)
    return(NA)
  x
}



#' @title For printing in lisp style
#' @param v Object to be printed

#' @export
toLisp <- function(v) {
  UseMethod("toLisp")
}

#' @export
toLisp.list <- function(v){
  ll <- sapply(v, toLisp)
  toLisp(ll)
}

#' @export
toLisp.cc <- function(v) toLisp.list(v)

#' @export
toLisp.default <- function(v){
  uuu <- if (class(v)[1] %in% c("ecc", "vcc")) class(v)[1] 
  vs <- paste(uuu, "(", paste(v, collapse =' '), ")", sep='');  vs
}



## as.atom <- function(...){
##   x <- unlist(list(...))
##   if (length(x)==1)
##     class(x) <- c('v', 'atom', class(x))
##   else
##     class(x) <- c('e', 'atom', class(x))
##   x
## }

as.cc <- function(v)
    UseMethod("as.cc")

#' @export
as.cc.list <- function(v){
  u <- unique(sapply(v,length))
  if (length(u)>1)
    stop("Entries not of same type...\n")
  cla <- unlist(unique(lapply(v, class)))
  if ("atom" %in% cla){
    if (u>1)
      cl <- c("ecc", "cc", "list")
    else
      cl <- c("vcc", "cc", "list")
    class(v)<-cl;
  }
  v
}


#' @export
as.cc.default <- function(v){
  as.cc(list(v))
}

as.cclist <- function(x){
  if ("cc" %in% class(x[[1]]))
  class(x) <- 'cclist'
  x
}

#' @export
print.cc <- function(x, ...){
  cat(class(x)[1], toLisp(x),"\n")
}

#' @export
print.cclist <- function(x,...){
  lapply(x, print)
}

#' @export
print.atom <- function(x, ...){
  ##cat(paste("(",paste(x,collapse=','),")",sep=''),"\n")
  cat(toLisp(x),"\n")
}

## maximalSetL2 <- function(set){
##   if (length(set)<=1)
##     return(set)

##   set   <- unique(cardOrder(set))
##   wset  <- set
##   value <- NULL
##   repeat{
##     el    <- wset[[1]]
##     wset2 <- wset[-1]
##     idx <- match.containsLL2(el, wset2)
##     if (is.na(idx))
##       value <- c(value, list(el))
##     wset <- wset2
    
##     if(length(wset)==0) 
##       break()
##     if(length(wset)==1){
##       value <- c(value,wset)
##       break()
##     }
##   }
##   return(value)
## }
