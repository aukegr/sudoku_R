#sudoku solver using backtracking by auke.greijdanus@gmail.com

sdk <-c(
  0,0,0,0,0,0,8,6,0,
  0,0,0,8,0,7,0,0,0,
  8,0,0,0,3,6,1,0,2,
  7,0,0,0,0,0,0,9,3,
  0,0,5,0,0,0,4,0,0,
  1,8,0,0,0,0,0,0,6,
  6,0,8,1,9,0,0,0,7,
  0,0,0,2,0,3,0,0,0,
  0,3,4,0,0,0,0,0,0)

sdk2 <- sdk

lc <- function(a){
  for (i in length(a):1){
    if (a[i] != sdk2[i]){
      index_value<-list("index"=i,"value"=a[i])
      return(index_value)}}}

gb <- function(x,i){
  s0<-0
  w<-0
  for (q in 1:2){
    for (j in 1:length(x)){
      r<-floor((j-1)/9)+1
      c <- j%%9
      if (c==0){c<-9}
      if (j==i & s0 == 0){
        s0 <- ceiling(r/3)**2 * ceiling(c/3)
        break}
      if (s0 == ceiling(r/3)**2 * ceiling(c/3)){
        w <- append(w, x[j])}}}
return(w)}

rev<-0
i<-1
while (i <= length(sdk)){
  for (s in sdk) {
    if (sdk[i]==0){
      t<-0
      b<-floor((i-1)/9)*9+1
      t<- append(t, sdk[seq(b, b+8, 1)])
      c <- i%%9
      if (c==0){c<-9}
      t<- append(t, sdk[seq(c, length(sdk), 9)])
      t<- append(t, gb(sdk,i))
      if (rev>0){
        t<-append(t, seq(1, rev))
        rev<-0}
    for (z in 1:9){
      if (!(z %in% t)){
        sdk[i] = z
        break}}
    if (sdk[i] != z){
      rev = lc(sdk)$value
      sdk[lc(sdk)$index]=0
      i<-1
      break}
    }
  i<-i+1
  }
}

for (i in 1:length(sdk)){
 cat(sdk[i])
  if (i%%9 == 0){cat('\n')}
}