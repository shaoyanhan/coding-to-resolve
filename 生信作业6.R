k=8;Dmax=3;dmin=4;n=0
change=TRUE

kmeansdata$V3<-sqrt((kmeansdata$V1)^2+(kmeansdata$V2)^2)
kmeansdata1<-kmeansdata[order(kmeansdata$V3),]
site<-array()
site<-floor(seq(from=0,to=nrow(kmeansdata1),by=nrow(kmeansdata1)/(k+1)))
site<-site[c(-1,-length(site))]

test<-matrix(0,nrow =k,ncol = 3)
for (i in 1:k) {
  test[i,1]<-kmeansdata1[site[i],1]
  test[i,2]<-kmeansdata1[site[i],2]
  test[i,3]<-i
}
while(change){
  dis<-array();kinds<-matrix(nrow =nrow(kmeansdata1),ncol = 2 )
  for (i in 1:nrow(kmeansdata1)) {
    kinds[i,1]<-rownames(kmeansdata1)[i]
    for (j in 1:k) {
      dis[j]<-sqrt((kmeansdata1$V1[i]-test[j,1])^2+(kmeansdata1$V2[i]-test[j,2])^2)
    }
    kinds[i,2]<-which.min(dis)
  }
  
  inner<-array()
  center<-matrix(0,ncol =2,nrow = k)
  for (i in 1:k) {
    inner[i]<-max(dist(kmeansdata1[which(kinds[,2]==test[i,3]),]))
    center[i,1]<-mean(kmeansdata1$V1[which(kinds[,2]==test[i,3])])
    center[i,2]<-mean(kmeansdata1$V2[which(kinds[,2]==test[i,3])])
  }
  
  if(length(which(test[,1:2]==center))==length(center)){
    change=FALSE
  }
  test[,1:2]<-center[,1:2]
  n=n+1
  
  if(n==500){
    change=FALSE
  }
  
  x=0;y=0;
  for (i in 1:k) {
    if(dmin>=inner[i]){
      x=x+1
    }
  }
  for (i in 1:choose(k,2)) {
    if(Dmax<=dist(center)[i]){
      y=y+1
    }
  }
  if(x==k){
    if(y==choose(k,2)){
      change=FALSE
    }
  }  
}
plot(kmeansdata$V1[as.numeric(kinds[,1])],kmeansdata$V2[as.numeric(kinds[,1])],col=kinds[,2])




plot(kmeansdata$V1[as.numeric(kinds[which(kinds[,2]==1),1])],kmeansdata$V2[as.numeric(kinds[which(kinds[,2]==1),1])],xlim = c(-6,6),ylim = c(-6,6))




k=8;Dmax=3;dmin=4;n=0
change=TRUE;continue=TRUE


while (continue) {
  kmeansdata$V3<-sqrt((kmeansdata$V1)^2+(kmeansdata$V2)^2)
  kmeansdata1<-kmeansdata[order(kmeansdata$V3),]
  site<-array()
  site<-floor(seq(from=0,to=nrow(kmeansdata1),by=nrow(kmeansdata1)/(k+1)))
  site<-site[c(-1,-length(site))]
  
  test<-matrix(0,nrow =k,ncol = 3)
  for (i in 1:k) {
    test[i,1]<-kmeansdata1[site[i],1]
    test[i,2]<-kmeansdata1[site[i],2]
    test[i,3]<-i
  }
  while(change){
    dis<-array();kinds<-matrix(nrow =nrow(kmeansdata1),ncol = 2 )
    for (i in 1:nrow(kmeansdata1)) {
      kinds[i,1]<-rownames(kmeansdata1)[i]
      for (j in 1:k) {
        dis[j]<-sqrt((kmeansdata1$V1[i]-test[j,1])^2+(kmeansdata1$V2[i]-test[j,2])^2)
      }
      kinds[i,2]<-which.min(dis)
    }
    
    inner<-array()
    center<-matrix(0,ncol =2,nrow = k)
    for (i in 1:k) {
      inner[i]<-weighted.mean(dist(kmeansdata1[which(kinds[,2]==test[i,3]),]))
      center[i,1]<-mean(kmeansdata1$V1[which(kinds[,2]==test[i,3])])
      center[i,2]<-mean(kmeansdata1$V2[which(kinds[,2]==test[i,3])])
    }
    
    if(length(which(test[,1:2]==center))==length(center)){
      change=FALSE
    }
    test[,1:2]<-center[,1:2]
    n=n+1
    
    if(n==500){
      change=FALSE
    }
    
    
  }
  x=0;y=0;
  for (i in 1:k) {
    if(dmin>=inner[i]){
      x=x+1
    }
  }
  for (i in 1:choose(k,2)) {
    if(Dmax<=dist(center)[i]){
      y=y+1
    }
  }
  if(x==k){
    if(y==choose(k,2)){
      continue=FALSE
    }
  }
  k=k+1
  change=TRUE
  
}
  

plot(kmeansdata$V1[as.numeric(kinds[,1])],kmeansdata$V2[as.numeric(kinds[,1])],col=kinds[,2])


k=9;Dmax=3;dmin=4;n=0
change=TRUE

kmeansdata$V3<-sqrt((kmeansdata$V1)^2+(kmeansdata$V2)^2)
kmeansdata1<-kmeansdata[order(kmeansdata$V3),]
site<-array()
site<-floor(seq(from=0,to=nrow(kmeansdata1),by=nrow(kmeansdata1)/(k+1)))
site<-site[c(-1,-length(site))]

test<-matrix(0,nrow =k,ncol = 3)
for (i in 1:k) {
  test[i,1]<-kmeansdata[i,1]
  test[i,2]<-kmeansdata[i,2]
  test[i,3]<-i
}
while(change){
  dis<-array();kinds<-matrix(nrow =nrow(kmeansdata1),ncol = 2 )
  for (i in 1:nrow(kmeansdata1)) {
    kinds[i,1]<-rownames(kmeansdata1)[i]
    for (j in 1:k) {
      dis[j]<-sqrt((kmeansdata1$V1[i]-test[j,1])^2+(kmeansdata1$V2[i]-test[j,2])^2)
    }
    kinds[i,2]<-which.min(dis)
  }
  
  inner<-array()
  center<-matrix(0,ncol =2,nrow = k)
  for (i in 1:k) {
    inner[i]<-max(dist(kmeansdata1[which(kinds[,2]==test[i,3]),]))
    center[i,1]<-mean(kmeansdata1$V1[which(kinds[,2]==test[i,3])])
    center[i,2]<-mean(kmeansdata1$V2[which(kinds[,2]==test[i,3])])
  }
  
  if(length(which(test[,1:2]==center))==length(center)){
    change=FALSE
  }
  test[,1:2]<-center[,1:2]
  n=n+1
  
  if(n==500){
    change=FALSE
  }
  
  x=0;y=0;
  for (i in 1:k) {
    if(dmin>=inner[i]){
      x=x+1
    }
  }
  for (i in 1:choose(k,2)) {
    if(Dmax<=dist(center)[i]){
      y=y+1
    }
  }
  if(x==k){
    if(y==choose(k,2)){
      change=FALSE
    }
  }  
}
plot(kmeansdata$V1[as.numeric(kinds[,1])],kmeansdata$V2[as.numeric(kinds[,1])],col=kinds[,2])





