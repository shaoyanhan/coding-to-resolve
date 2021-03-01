d<-read.csv(file.choose(),skip = 1,header = F);
remove(c)
c<-paste(d[1:nrow(d),],sep = "",collapse = "");

DNAstr<-unlist(strsplit(c,split = ""));
x<-array();
for (i in 1:length(DNAstr)) {
  if(DNAstr[i]=="A"){
    x[i]="T";
  }else if(DNAstr[i]=="T"){
    x[i]="A";
  }else if(DNAstr[i]=="C"){
    x[i]="G";
  }else{
    x[i]="C";
  }
}
DNAstr2<-rev(x);
s<-c("TAA" ,"TAG", "TGA");

#+1
j=1;k<-array()
for (i in seq(from=1,to=(length(DNAstr)-1),by=3)) {
  k[j]<-paste(DNAstr[i:(i+2)],sep = "",collapse = "");
  j=j+1;
}
ORFa<-array();ORFb<-array();m=1;n=1;j=0;i=1;u=0;
while(i<=length(k)) {
  u=0;
  if(k[i]=="ATG"){
    ORFa[m]=i;
    m=m+1;
    i=i+1;
    u=u+1;
    while (j==0) {
      if(k[i]=="TAA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TAG"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TGA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else{
        i=i+1;
      }
    }
    j=j-1;
  }
  if(u==0){
    i=i+1;
  }
}
ORFa<-(ORFa*3)-2
ORFb<-(ORFb*3)
ORF<-cbind(as.matrix(ORFa),as.matrix(ORFb));
j=1;y<-array();
for(i in 1:nrow(ORF)) {
  e<-ORF[i,2]-ORF[i,1];
  if(e>90){
    y[j]<-i;
    j=j+1;
  }
}
ORFs<-ORF[y,];
ORFstr<-list();
for (i in 1:nrow(ORFs)) {
  ORFstr[i]<-paste(DNAstr[ORFs[i,1]:ORFs[i,2]],sep = "",collapse = "");
}

#+2
j=1;k<-array()
for (i in seq(from=2,to=(length(DNAstr)-1),by=3)) {
  k[j]<-paste(DNAstr[i:(i+2)],sep = "",collapse = "");
  j=j+1;
}
ORFa<-array();ORFb<-array();m=1;n=1;j=0;i=1;u=0;
while(i<=length(k)) {
  u=0;
  if(k[i]=="ATG"){
    ORFa[m]=i;
    m=m+1;
    i=i+1;
    u=u+1;
    while (j==0) {
      if(k[i]=="TAA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TAG"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TGA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else{
        i=i+1;
      }
    }
    j=j-1;
  }
  if(u==0){
    i=i+1;
  }
}
ORFa<-(ORFa*3)-2
ORFb<-(ORFb*3)
ORF<-cbind(as.matrix(ORFa),as.matrix(ORFb));
j=1;y<-array();
for(i in 1:nrow(ORF)) {
  e<-ORF[i,2]-ORF[i,1];
  if(e>90){
    y[j]<-i;
    j=j+1;
  }
}
ORFs<-ORF[y,];
ORFstr2<-list();
for (i in 1:nrow(ORFs)) {
  ORFstr2[i]<-paste(DNAstr[ORFs[i,1]:ORFs[i,2]],sep = "",collapse = "");
}

#+3
j=1;k<-array()
for (i in seq(from=3,to=(length(DNAstr)-1),by=3)) {
  k[j]<-paste(DNAstr[i:(i+2)],sep = "",collapse = "");
  j=j+1;
}
ORFa<-array();ORFb<-array();m=1;n=1;j=0;i=1;u=0;
while(i<=length(k)) {
  u=0;
  if(k[i]=="ATG"){
    ORFa[m]=i;
    m=m+1;
    i=i+1;
    u=u+1;
    while (j==0) {
      if(k[i]=="TAA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TAG"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TGA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else{
        i=i+1;
      }
    }
    j=j-1;
  }
  if(u==0){
    i=i+1;
  }
}
ORFa<-(ORFa*3)-2
ORFb<-(ORFb*3)
ORF<-cbind(as.matrix(ORFa),as.matrix(ORFb));
j=1;y<-array();
for(i in 1:nrow(ORF)) {
  e<-ORF[i,2]-ORF[i,1];
  if(e>90){
    y[j]<-i;
    j=j+1;
  }
}
ORFs<-ORF[y,];
ORFstr3<-list();
for (i in 1:nrow(ORFs)) {
  ORFstr3[i]<-paste(DNAstr[ORFs[i,1]:ORFs[i,2]],sep = "",collapse = "");
}

#-1
j=1;k<-array()
for (i in seq(from=1,to=(length(DNAstr2)-1),by=3)) {
  k[j]<-paste(DNAstr2[i:(i+2)],sep = "",collapse = "");
  j=j+1;
}
ORFa<-array();ORFb<-array();m=1;n=1;j=0;i=1;u=0;
while(i<=length(k)) {
  u=0;
  if(k[i]=="ATG"){
    ORFa[m]=i;
    m=m+1;
    i=i+1;
    u=u+1;
    while (j==0) {
      if(k[i]=="TAA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TAG"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TGA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else{
        i=i+1;
      }
    }
    j=j-1;
  }
  if(u==0){
    i=i+1;
  }
}
ORFa<-(ORFa*3)-2
ORFb<-(ORFb*3)
ORF<-cbind(as.matrix(ORFa),as.matrix(ORFb));
j=1;y<-array();
for(i in 1:nrow(ORF)) {
  e<-ORF[i,2]-ORF[i,1];
  if(e>90){
    y[j]<-i;
    j=j+1;
  }
}
ORFs<-ORF[y,];
ORFstr4<-list();
for (i in 1:nrow(ORFs)) {
  ORFstr4[i]<-paste(DNAstr2[ORFs[i,1]:ORFs[i,2]],sep = "",collapse = "");
}


#-2
j=1;k<-array()
for (i in seq(from=2,to=(length(DNAstr2)-1),by=3)) {
  k[j]<-paste(DNAstr2[i:(i+2)],sep = "",collapse = "");
  j=j+1;
}
ORFa<-array();ORFb<-array();m=1;n=1;j=0;i=1;u=0;
while(i<=length(k)) {
  u=0;
  if(k[i]=="ATG"){
    ORFa[m]=i;
    m=m+1;
    i=i+1;
    u=u+1;
    while (j==0) {
      if(k[i]=="TAA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TAG"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TGA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else{
        i=i+1;
      }
    }
    j=j-1;
  }
  if(u==0){
    i=i+1;
  }
}
ORFa<-(ORFa*3)-2
ORFb<-(ORFb*3)
ORF<-cbind(as.matrix(ORFa),as.matrix(ORFb));
j=1;y<-array();
for(i in 1:nrow(ORF)) {
  e<-ORF[i,2]-ORF[i,1];
  if(e>90){
    y[j]<-i;
    j=j+1;
  }
}
ORFs<-ORF[y,];
ORFstr5<-list();
for (i in 1:nrow(ORFs)) {
  ORFstr5[i]<-paste(DNAstr2[ORFs[i,1]:ORFs[i,2]],sep = "",collapse = "");
}


#-3
j=1;k<-array()
for (i in seq(from=3,to=(length(DNAstr2)-1),by=3)) {
  k[j]<-paste(DNAstr2[i:(i+2)],sep = "",collapse = "");
  j=j+1;
}
ORFa<-array();ORFb<-array();m=1;n=1;j=0;i=1;u=0;
while(i<=length(k)) {
  u=0;
  if(k[i]=="ATG"){
    ORFa[m]=i;
    m=m+1;
    i=i+1;
    u=u+1;
    while (j==0) {
      if(k[i]=="TAA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TAG"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else if(k[i]=="TGA"){
        ORFb[n]=i;
        i=i+1;
        j=j+1;
        n=n+1;
      }else{
        i=i+1;
      }
    }
    j=j-1;
  }
  if(u==0){
    i=i+1;
  }
}
ORFa<-(ORFa*3)-2
ORFb<-(ORFb*3)
ORFa<-ORFa[1:(length(ORFa)-1)]
ORF<-cbind(as.matrix(ORFa),as.matrix(ORFb));
j=1;y<-array();
for(i in 1:nrow(ORF)) {
  e<-ORF[i,2]-ORF[i,1];
  if(e>90){
    y[j]<-i;
    j=j+1;
  }
}
ORFs<-ORF[y,];
ORFstr6<-list();
for (i in 1:nrow(ORFs)) {
  ORFstr6[i]<-paste(DNAstr2[ORFs[i,1]:ORFs[i,2]],sep = "",collapse = "");
}