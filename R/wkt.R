
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
    obj
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
    parts = str_match_all(p$body, .partRE)[[1]][,2]
    coords_to_matrix(parts, .nvalues(p), .names(p))
}

buildSF.POLYGON <- function(p){
    buildPOLYGON(p$body, .nvalues(p), .names(p))
}

buildPOLYGON <- function(p, nvalues, names){
    parts = str_match_all(p, .partRE)[[1]][,2]
    llply(parts,
          function(part){
              coords_to_matrix(part, nvalues, names)}
          )
    
}

buildSF.MULTIPOLYGON <- function(p){
    p$body = str_trim(p$body)
    polygonsplits = commadepths(p$body)
    commasplits = polygonsplits$pos[polygonsplits$depth==1]
    starts=c(1,commasplits+1)
    ends = c(commasplits-1, nchar(p$body)+10)
    parts = str_sub(p$body, starts, ends)
    llply(
        parts,
        function(pp){buildPOLYGON(pp, .nvalues(p), .names(p))}
        )
}

commadepths = function(s){
    parens = parendepths(s)
    commas = str_locate_all(s,fixed(","))[[1]][,1]
    commadepths = posdepth(commas,parens)
    return(list(pos=commas, depth=commadepths))
}

posdepth = Vectorize(function(pos, parens){
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
        items[[item]]=substr(s,pd$open[l1opens[item]],pd$close[l1closes[item]])
    }
    items
}
