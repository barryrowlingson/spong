
coords_to_matrix <- function(s, np=4, cnames){
###
### convert "x y {z m}, x y {z m}" to matrix. NA padding if missing z/m
###
    require(stringr)
    pts = str_split(s,",")[[1]]
    pts = str_split_fixed(str_trim(pts)," +", np)
    mode(pts)="numeric"
    if(ncol(pts)!=np){
        stop("Error decoding points")
    }
    colnames(pts)=cnames
    pts
}


parseWKT <- function(s){
    sf_head = str_extract(s,"^\\s*([^\\(]*)")[[1]]
    sf_body = str_trim(str_sub(s, nchar(sf_head)+1))
    sf_type_dim = str_split(str_trim(sf_head)," +")[[1]]
    sf_type =  toupper(sf_type_dim[1])
    dim = 2
    measured = FALSE
    if(length(sf_type_dim) > 1){
        if(str_detect(sf_type_dim[2],ignore.case("z"))){
            dim = 3
        }

        if(str_detect(sf_type_dim[2],ignore.case("m"))){
            measured=TRUE
        }
    }
    obj = list(type=sf_type,
        measured=measured,
        dim=dim,
        body=sf_body
        )
    class(obj)=c("wktparse",sf_type)
    sf = buildSF(obj)
    attr(sf,"type")=obj$type
    attr(sf,"dimension")=obj$dim
    attr(sf,"measured")=obj$measured
    sf
}

buildSF <- function(p){
    UseMethod("buildSF")
}

.nvalues <- function(p){
    return(p$dim + p$measured)
}

.names <- function(p){
    cn = c("x","y")
    if(p$dim == 3){
        cn = c(cn,"z")
    }
    if(p$measured){
        cn = c(cn,"m")
    }
    cn
}

.partRE = "\\(([^\\(\\)]*)\\)"


buildSF.POINT <- function(p){
    ## a 1-row matrix
    obj = coords_to_matrix(nested2list(p$body,0)[[1]], .nvalues(p), .names(p))
    class(obj)=c("sf","POINT")
    obj
}

buildSF.LINESTRING <- function(p){
    ## an N-row matrix
    obj = coords_to_matrix(nested2list(p$body,0)[[1]], .nvalues(p), .names(p))
    class(obj)=c("sf","LINESTRING")
    obj
}

buildSF.POLYGON <- function(p){
    ## a list - outer polygon then possible holes
    split = nested2list(p$body,1)
    obj = llply(split, function(part){
        coords_to_matrix(part, .nvalues(p), .names(p))
    })
    class(obj)=c("sf","POLYGON")
    obj
}

buildPOLYGON <- function(p, nvalues, names){
    parts = str_match_all(p, .partRE)[[1]][,2]
    llply(parts,
          function(part){
              coords_to_matrix(part, nvalues, names)}
          )
    
}

buildSF.MULTIPOLYGON <- function(p){
    ## a list of POLYGON objects
    splitMulti = nested2list(p$body,1)
    obj = llply(splitMulti, function(polygon){
        rings = nested2list(polygon,0)
        llply(rings, function(part){
            coords_to_matrix(part, .nvalues(p), .names(p))
        })
    })
    class(obj)=c("sf","MULTIPOLYGON")
    obj
}

commadepths <- function(s){
    parens = parendepths(s)
    commas = str_locate_all(s,fixed(","))[[1]][,1]
    commadepths = posdepth(commas,parens)
    return(list(pos=commas, depth=commadepths))
}

posdepth <- Vectorize(function(pos, parens){
    sum(parens$open<pos) - sum(parens$close<pos)
},"pos")
    
parendepths <- function(s,openclose=fixed(c("(",")"))){
    pos=str_locate_all(s,openclose)
    openclose = list(open=pos[[1]][,1],close=pos[[2]][,1])
    openclose
}

nested2list <- function(s,level=1){
    pd = parendepths(s)
    opens  = posdepth(pd$open, pd)
    closes = posdepth(pd$close, pd) - 1
    if(opens[1]!=0){
        stop("closing bracket before opening bracket")
    }
    if(closes[length(closes)]!=0){
        stop("bracket mismatch")
    }
    l1opens = which(opens==level)
    l1closes = which(closes==level)
    items = list()
    for(item in 1:length(l1opens)){
        items[[item]]=substr(s,pd$open[l1opens[item]]+1,pd$close[l1closes[item]]-1)
    }
    items
}

sp2wkt <- function(spob){
    wkts = writeWKT(spob, byid=TRUE)
    llply(wkts,
          function(w){
              parseWKT(w)
          }
          )
}

