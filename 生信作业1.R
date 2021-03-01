PAM250<-matrix(0,ncol = 20,nrow = 20);
pstr<-c("CSTPAGNDEQHRKMILVFYW");
pstr<-unlist(strsplit(pstr,split = ""));
rownames(PAM250)<-pstr;colnames(PAM250)<-pstr;
PAM250[,1]<-c(12,0,-2,-3,-2,-3,-4,-5,-5,-5,-3,-4,-5,-5,-2,-6,-2,-4,0,-8)
PAM250[2:nrow(PAM250),2]<-c(2,1,1,1,1,1,0,0,-1,-1,0,0,-2,-1,-3,-1,-3,-3,-2)
PAM250[3:nrow(PAM250),3]<-c(3,0,1,0,0,0,0,-1,-1,-1,0,-1,0,-2,0,-3,-3,-5)
PAM250[4:nrow(PAM250),4]<-c(6,1,-1,-1,-1,-1,0,0,0,-1,-2,-2,-3,-1,-5,-5,-6)
PAM250[5:nrow(PAM250),5]<-c(2,1,0,0,0,0,-1,-2,-1,-1,-1,-2,0,-4,-3,-6)
PAM250[6:nrow(PAM250),6]<-c(5,0,1,0,-1,-2,-3,-2,-3,-3,-4,-1,-5,-5,-7)
PAM250[7:nrow(PAM250),7]<-c(2,2,1,1,2,0,1,-2,-2,-3,-2,-4,-2,-4)
PAM250[8:nrow(PAM250),8]<-c(4,3,2,1,-1,0,-3,-2,-4,-2,-6,-4,-7)
PAM250[9:nrow(PAM250),9]<-c(4,2,1,-1,0,-2,-2,-3,-2,-5,-4,-7)
PAM250[10:nrow(PAM250),10]<-c(4,3,1,1,-1,-2,-2,-2,-5,-4,-5)
PAM250[11:nrow(PAM250),11]<-c(6,2,0,-2,-2,-2,-2,-2,0,-3)
PAM250[12:nrow(PAM250),12]<-c(6,3,0,-2,-3,-2,-4,-4,2)
PAM250[13:nrow(PAM250),13]<-c(5,0,-2,-3,-2,-5,-4,-3)
PAM250[14:nrow(PAM250),14]<-c(6,2,4,2,0,-2,-4)
PAM250[15:nrow(PAM250),15]<-c(5,2,4,1,-1,-5)
PAM250[16:nrow(PAM250),16]<-c(6,2,2,-1,-2)
PAM250[17:nrow(PAM250),17]<-c(4,-1,-2,-6)
PAM250[18:nrow(PAM250),18]<-c(9,7,0)
PAM250[19:nrow(PAM250),19]<-c(10,0)
PAM250[20:nrow(PAM250),20]<-c(17)

#初始化全局比对矩阵
gap<-c(-8);
proteinstr1<-c("AGWGAHEA");
proteinstr2<-c("PAWHEAEAG");
p1<-unlist(strsplit(proteinstr1,split = ""));
p2<-unlist(strsplit(proteinstr2,split = ""));
m1<-matrix(nrow = length(p2)+1,ncol = length(p1)+1);
rownames(m1)<-c(rep(0,nrow(m1)));colnames(m1)<-c(rep(0,ncol(m1)));
rownames(m1)[1]<-c("-");colnames(m1)[1]<-c("-");
rownames(m1)[2:nrow(m1)]<-p2;colnames(m1)[2:ncol(m1)]<-p1;
m1[1,1]=0;
for (i in 2:ncol(m1)) {
  m1[1,i]=m1[1,i-1]-8
  };
for (i in 2:nrow(m1)) {
  m1[i,1]=m1[i-1,1]-8
  };

#计算全局比对得分矩阵
for (i in 2:nrow(m1)) {
  for (j in 2:ncol(m1)) {
    s1=m1[i,j-1]-8;
    s2=m1[i-1,j]-8;
    x<-which(rownames(m1)[i]==rownames(PAM250));
    y<-which(colnames(m1)[j]==colnames(PAM250));
    if(PAM250[x,y]==PAM250[y,x])
    {z=PAM250[x,y];}
    else
    {if(PAM250[x,y]==0)
      {z<-PAM250[y,x];}
      else
      {z<-PAM250[x,y];}
    }
    s3=m1[i-1,j-1]+z;
    m1[i,j]<-max(s1,s2,s3)
  }
}

#回溯
i<-nrow(m1);j<-ncol(m1);
a<-m1[i,j];
b<-array();
c<-array();
while (a>0) {
  s1=a-gap;
  x<-which(rownames(m1)[i]==rownames(PAM250));
  y<-which(colnames(m1)[j]==colnames(PAM250));
  if(PAM250[x,y]==PAM250[y,x]){
    z=PAM250[x,y];
    }else{
      if(PAM250[x,y]==0)
      {z<-PAM250[y,x];
      }else{
        z<-PAM250[x,y];
       }
  }
  s2=a-z;
  
  if(m1[i,j-1]==s1){
    b[length(b)+1]="-";
    c[length(c)+1]=colnames(m1)[j];
    i=i;j=j-1;a=m1[i,j];
   }else if(m1[i-1,j]==s1){
     b[length(b)+1]=rownames(m1)[i];
     c[length(c)+1]="-";
     i=i-1;j=j;a=m1[i,j];
   }else{
     b[length(b)+1]=rownames(m1)[i];
     c[length(c)+1]=colnames(m1)[j];
     i=i-1;j=j-1;a=m1[i,j];
   }
}
result1<-matrix(ncol = max(length(b),length(c)),nrow = 2);
result1[1,]<-rev(b);
result1[2,]<-rev(c)

#初始化局部比对矩阵
gap<-c(-8);
proteinstr1<-c("AGWGAHEA");
proteinstr2<-c("PAWHEAEAG");
p1<-unlist(strsplit(proteinstr1,split = ""));
p2<-unlist(strsplit(proteinstr2,split = ""));
m2<-matrix(nrow = length(p2)+1,ncol = length(p1)+1);
rownames(m2)<-c(rep(0,nrow(m2)));colnames(m2)<-c(rep(0,ncol(m2)));
rownames(m2)[1]<-c("-");colnames(m2)[1]<-c("-");
rownames(m2)[2:nrow(m2)]<-p2;colnames(m2)[2:ncol(m2)]<-p1;
m2[1,]=0;m2[,1]=0

#计算局部比对得分矩阵
for (i in 2:nrow(m2)) {
  for (j in 2:ncol(m2)) {
    s1=m2[i,j-1]-8;
    if(s1<0){
      s1=0;
    }
    s2=m2[i-1,j]-8;
    if(s2<0){
      s2=0;
    }
    x<-which(rownames(m2)[i]==rownames(PAM250));
    y<-which(colnames(m2)[j]==colnames(PAM250));
    if(PAM250[x,y]==PAM250[y,x]){
      z=PAM250[x,y];
      }else{
        if(PAM250[x,y]==0){
          z<-PAM250[y,x];
        }else{
            z<-PAM250[x,y];
            }
    }
    s3=m2[i-1,j-1]+z;
    m2[i,j]<-max(s1,s2,s3)
  }
}

#回溯
i<-(which.max(m2)%%10);j<-floor((which.max(m2)/10)+1)
a<-m2[i,j];
b<-array();
c<-array();
while (a>0) {
  s1=a-gap;
  x<-which(rownames(m2)[i]==rownames(PAM250));
  y<-which(colnames(m2)[j]==colnames(PAM250));
  if(PAM250[x,y]==PAM250[y,x]){
    z=PAM250[x,y];
  }else{
    if(PAM250[x,y]==0)
    {z<-PAM250[y,x];
    }else{
      z<-PAM250[x,y];
    }
  }
  s2=a-z;
  
  if(m2[i,j-1]==s1){
    b[length(b)+1]="-";
    c[length(c)+1]=colnames(m2)[j];
    i=i;j=j-1;a=m2[i,j];
  }else if(m2[i-1,j]==s1){
    b[length(b)+1]=rownames(m2)[i];
    c[length(c)+1]="-";
    i=i-1;j=j;a=m2[i,j];
  }else{
    b[length(b)+1]=rownames(m2)[i];
    c[length(c)+1]=colnames(m2)[j];
    i=i-1;j=j-1;a=m2[i,j];
  }
}
result2<-matrix(ncol = max(length(b),length(c)),nrow = 2);
result2[1,]<-rev(b);
result2[2,]<-rev(c)
result1;result2
















