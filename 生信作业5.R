positive<-positive1[1:45,];negative<-negative1[1:45,]
n<-ncol(positive);p<-nrow(positive);q<-nrow(negative)

sumxki1<-array();sumxki2<-array()
for (i in 1:n) {
  sumxki1[i]<-sum(positive[,i])
}
for (i in 1:n) {
  sumxki2[i]<-sum(negative[,i])
}

meanxi1<-array();meanxi2<-array();
for (i in 1:length(sumxki1)) {
  meanxi1[i]<-sumxki1[i]/p
}
for (i in 1:length(sumxki2)) {
  meanxi2[i]<-sumxki2[i]/q
}

sumxkixkj1<-matrix(nrow = n,ncol = n);pos=0
for (i in 1:n) {
  for (j in 1:n) {
    for (k in 1:p) {
      pos<-pos+positive[k,i]*positive[k,j]
    }
    sumxkixkj1[i,j]<-pos
    pos=0
  }
}
sumxkixkj2<-matrix(nrow = n,ncol = n);pos=0
for (i in 1:n) {
  for (j in 1:n) {
    for (k in 1:p) {
      pos<-pos+negative[k,i]*negative[k,j]
    }
    sumxkixkj2[i,j]<-pos
    pos=0
  }
}

di<-array()
for (i in 1:n) {
  di[i]<-meanxi1[i]-meanxi2[i]
}

sij<-matrix(nrow = n,ncol = n)
for (i in 1:n) {
  for (j in 1:n) {
    sij[i,j]<-sumxkixkj1[i,j]-((sumxki1[i]*sumxki1[j])/p)+sumxkixkj2[i,j]-((sumxki2[i]*sumxki2[j])/q)
  }
}
ci<-array()
ci<-solve(sij,di)

meany1=0;meany2=0;cx=0
for (i in 1:n) {
  cx<-cx+ci[i]*meanxi1[i]
}
meany1<-cx;cx=0
for (i in 1:n) {
  cx<-cx+ci[i]*meanxi2[i]
}
meany2<-cx
C<-(p*meany1+q*meany2)/(p+q)

test1<-positive1[(nrow(positive1)-4):nrow(positive1),]
test2<-negative1[(nrow(negative1)-4):nrow(negative1),]
test<-rbind(test1,test2)
y<-array()
for (i in 1:n) {
  y[i]<-sum(ci*test[i,])
  if(meany1>meany2){
    if(y[i]>C){
      print(paste("y",i,"属于positive"))
    }else{
      print(paste("y",i,"属于negative"))
    }
  }
  if(meany1<meany2){
    if(y[i]>C){
      print(paste("y",i,"属于negative"))
    }else{
      print(paste("y",i,"属于positive"))
    }
  }
}


