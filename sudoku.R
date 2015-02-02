## Input unsolved sudoku with 0 for missing values

s <- matrix(
  nrow=9,ncol=9,byrow=T,
  data=c(
    0,0,0,0,0,2,0,3,0,
    0,3,7,8,0,0,0,0,6,
    2,0,5,4,0,0,0,0,0,
    8,5,0,2,0,0,0,0,1,
    0,1,4,0,0,0,0,6,0,
    0,7,0,6,0,1,0,9,4,
    0,0,0,1,8,0,0,4,3,
    7,0,0,0,0,0,0,5,9,
    0,0,0,0,0,9,0,1,0))

## Matrix of box IDs (constant)

bid <- matrix(
  nrow=9,ncol=9,byrow=T,
  data=c(
    1,1,1,2,2,2,3,3,3,
    1,1,1,2,2,2,3,3,3,
    1,1,1,2,2,2,3,3,3,
    4,4,4,5,5,5,6,6,6,
    4,4,4,5,5,5,6,6,6,
    4,4,4,5,5,5,6,6,6,
    7,7,7,8,8,8,9,9,9,
    7,7,7,8,8,8,9,9,9,
    7,7,7,8,8,8,9,9,9))
    
## Create the array of possibilities

p <- array(dim=c(9,9,9),data=TRUE)

## Start loop here while Sudoku remains unsolved

message("not all Sudoku are solvable using current routine")

while (0 %in% s){

## Rule out possibilities based on rows

for (x in 1:9){
  FALSE -> p[x,,s[x,][which(s[x,]!=0)]]
}

## Rule out possibilities based on columns

for (y in 1:9){
  FALSE -> p[,y,s[,y][which(s[,y]!=0)]]
}

## Rule out possibilities based on boxes

for (i in 1:9){
  FALSE -> p[which(bid==i,arr.ind=T,useNames=F)[,1],
             which(bid==i,arr.ind=T,useNames=F)[,2],
             s[which(bid==i,arr.ind=T,useNames=F)]]
}

## Check all cells for new solutions

for (x in 1:9){
  for (y in 1:9){
    if (s[x,y] == 0){ # Only do for unsolved cells
      if (sum(p[x,y,])==1){
        which(p[x,y,]==TRUE) -> s[x,y]
        print(which(p[x,y,]==TRUE))
        FALSE -> p[x,y,][which(p[x,y,]==TRUE)]
}}}}

## End big while loop here

}

## Display solution

s