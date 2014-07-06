
coords_to_matrix <- function(s, np=4){
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
    pts
}


parseWKT_head <- function(s){
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
    class(obj)=c("sf",sf_type)
    obj
}

parseWKT <- function(p){
    UseMethod("parseWKT")
}

.nvalues <- function(p){
    return(p$dim + p$measured)
}


parseWKT.POINT <- function(p){
    partRE = "\\(([^\\(\\)]*)\\)"
    parts = str_match_all(p$body, partRE)[[1]][,2]
    coords_to_matrix(parts, .nvalues(p))
}
