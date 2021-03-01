a<-read.csv(file.choose(),header = T,sep = ",");
b<-which(substr(as.character(a[,2]),1,5)=="chr2L");
t<-0;
for (i in 1:length(b)) {
  t[i]<-substr(as.character(a[b[i],2]),(nchar(as.character(a[b[i],2]))/2)+4,nchar(as.character(a[b[i],2])))
}
t<-abs(as.numeric(t));
k<-which.max(t);
y<-matrix(0,nrow = round(t[k]/10000),ncol = round(t[k]/10000));
x<-0;
for (l in b)
  {
  x<-0;
  c<-a$List_of_fragment_coordinates[l];
  c<-as.data.frame(strsplit(as.character(a$List_of_fragment_coordinates[l]),";"));
    for (i in 1:nrow(c))
      {
       if(i<10)
         {
         x[i]<-substr(substr(as.character(c[i,1]),7,nchar(as.character(c[i,1]))-3),1,nchar(substr(as.character(c[i,1]),7,nchar(as.character(c[i,1]))-3))/2)
          }
     else
       {   
         x[i]<-substr(substr(as.character(c[i,1]),7,nchar(as.character(c[i,1]))-3),1,nchar(substr(as.character(c[i,1]),7,nchar(as.character(c[i,1]))-4))/2)
        }
    };
      x<-round(as.numeric(x)/10000);
      i=1;j=length(x);m=i+1;n=j-1;
      while (i<length(x))
        {
        while(m<length(x)+1)
          {
          y[x[i],x[m]]<-y[x[i],x[m]]+1;
           m=m+1;
         }
          i=i+1;m=i+1;
        }
      while (j>1)
      {
        while(n>-1)
        {
          y[x[j],x[n]]<-y[x[j],x[n]]+1;
          n=n-1;
        }
        j=j-1;n=j-1;
      }
}
heatmap(y,Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none",main="chr2L");
ramp <- colorRamp(c("red", "white"));
cols<-rgb( ramp(seq(0, 1, length = 128)), max = 255);
heatmap(log(y+1),col=rev(cols),Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none",main="chr2L");
heatmap(log(y[1:300,1:300]+1),col=rev(cols), Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none",main="chr2L");
heatmap(log(y[1:100,1:100]+1),col=rev(cols), Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none",main="chr2L");

b<-which(substr(as.character(a[,2]),1,4)=="chr4");
e<-matrix(0,nrow =1 ,ncol = 2);
e<-as.data.frame(e);
colnames(e)[1:2]<-c("Source","Target");j=1;
x<-0;
for (l in b)
{
  c<-a$List_of_fragment_coordinates[l];
  c<-as.data.frame(strsplit(as.character(a$List_of_fragment_coordinates[l]),";"));
  x<-0;
  for (i in 1:nrow(c))
  {
    if(i<10)
    {
      x[i]<-substr(substr(as.character(c[i,1]),6,nchar(as.character(c[i,1]))-3),1,nchar(substr(as.character(c[i,1]),6,nchar(as.character(c[i,1]))-3))/2)
    }
    else
    {   
      x[i]<-substr(substr(as.character(c[i,1]),6,nchar(as.character(c[i,1]))-3),1,nchar(substr(as.character(c[i,1]),6,nchar(as.character(c[i,1]))-4))/2)
    }
  };
  x<-round(as.numeric(x)/500);
  s=1;m=2;
  while (s<length(x))
  {
    while(m<length(x)+1)
    {
      e[j,]<-c(x[s],x[m]);
      j=j+1;m=m+1;
    }
    s=s+1;m=s+1;
  };
}
simpleNetwork(e);


#single-cell matrix overlay
y<-0;
path<-"/store/yhshao/all/k40";
fileNames <- dir(path) ;
filePath <- sapply(fileNames, function(x){ paste(path,x,sep='/')});
data <- lapply(filePath[4:14], function(x){read.csv(x, header=T)});
sn<-as.data.frame(data[1])
colnames(sn)<-c("chr1","chr2","start1","end1","start2","end2","count");
for (i in 2:length(data)) 
{
  sn1<-as.data.frame(data[i])
  colnames(sn1)<-c("chr1","chr2","start1","end1","start2","end2","count");
  sn<-rbind(sn,sn1);
};
sn$kb1<-sn$start1/40000;
sn$kb2<-sn$start2/40000;
y<-matrix(0,nrow =max(sn$kb1),ncol =max(sn$kb1));
for (i in 1:nrow(sn)) 
{
  y[sn[i,8],sn[i,9]]<-sn[i,7]+y[sn[i,8],sn[i,9]];
};
for (i in 1:nrow(sn)) 
{
  y[sn[i,9],sn[i,8]]<-sn[i,7]+y[sn[i,9],sn[i,8]];
};
heatmap(y,Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none");
ramp <- colorRamp(c("red", "white"));
cols<-rgb( ramp(seq(0, 1, length = 128)), max = 255);
heatmap(log(y+1),col=rev(cols),Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none");
heatmap(log(y[1:500,1:500]+1),col=rev(cols), Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none");

#single-cell contact matrix
path<-"/store/yhshao/all/k40";
resolution<-200000
fileNames <- dir(path) ;
filePath <- sapply(fileNames, function(x){ paste(path,x,sep='/')});
data1<-read.csv(filePath[length(filePath)],header = T)
data1$interval1<-round(((data1$start1+data1$end1)/2)/resolution)
data1$interval2<-round(((data1$start2+data1$end2)/2)/resolution)
y<-matrix(0,nrow =max(data1$interval1,data1$interval2),ncol =max(data1$interval1,data1$interval2));
for (i in 1:nrow(data1)) 
{
  y[data1[i,8],data1[i,9]]<-data1[i,7]+y[data1[i,8],data1[i,9]];
}
for (i in 1:nrow(data1)) 
{
  y[data1[i,9],data1[i,8]]<-data1[i,7]+y[data1[i,9],data1[i,8]];
}
heatmap(y,Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none");
ramp <- colorRamp(c("red", "white"));
cols<-rgb( ramp(seq(0, 1, length = 128)), max = 255);
heatmap(log(y+1),col=rev(cols),Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none");
heatmap(log(y[1:500,1:500]+1),col=rev(cols), Rowv=NA,Colv=NA, labRow=NA,labCol=NA,scale="none")


#single-cell contact matrix PCA
path<-"/store/yhshao/all/k200";
resolution<-200000
fileNames <- dir(path) ;
fileNames<-fileNames[-c(1,2,length(fileNames),length(fileNames)-1,length(fileNames)-2)]
filePath <- sapply(fileNames, function(x){ paste(path,x,sep='/')})
#filePath<-filePath[(length(filePath)-20):length(filePath)]
left<-0;right<-0;num<-1;
for (i in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[i],header = T)
  data1$interval1<-round(((data1$start1+data1$end1)/2)/resolution)
  data1$interval2<-round(((data1$start2+data1$end2)/2)/resolution)
  if (num==1) {
    left<-min(data1$interval1,data1$interval2)
    num<-num+1
  }
  if (max(data1$interval1,data1$interval2)>right) {
    right<-max(data1$interval1,data1$interval2)
  }
  if (min(data1$interval1,data1$interval2)<left) {
    left<-min(data1$interval1,data1$interval2)
  }
}
left;right


testsample<-matrix(0,nrow = length(filePath),ncol = right*right)
for (filenum in 1:length(filePath)) {
  y<-matrix(0,nrow =right,ncol =right)
  data1<-read.csv(filePath[filenum],header = T)
  data1$interval1<-round(((data1$start1+data1$end1)/2)/resolution)
  data1$interval2<-round(((data1$start2+data1$end2)/2)/resolution)
  for (i in 1:nrow(data1)) 
  {
    y[data1[i,8],data1[i,9]]<-data1[i,7]+y[data1[i,8],data1[i,9]];
  }
  for (i in 1:nrow(data1)) 
  {
    y[data1[i,9],data1[i,8]]<-data1[i,7]+y[data1[i,9],data1[i,8]];
  }
  for (m in 1:nrow(y)) {
    for (n in 1:ncol(y)) {
      testsample[filenum,((m-1)*ncol(y))+n]<-y[m,n]
    }
  }
  print(paste('第',filenum,'条样本填充覆盖及深度矩阵完毕'))
}



zerocol<-array();j=1
for (i in 1:ncol(testsample)) {
  if(sum(testsample[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
testsample<-testsample[,-zerocol]
testsample.pca<-prcomp(testsample,scale. = T)



# data1<-read.csv(filePath[length(filePath)],header = T)
# data1$interval1<-round(((data1$start1+data1$end1)/2)/resolution)
# data1$interval2<-round(((data1$start2+data1$end2)/2)/resolution)
# min(data1$interval1,data1$interval2)



#single-cell contact matrix PCA(intra-chromosome contacts counts)

path<-"/store/yhshao/all/k200";
resolution<-200000
fileNames <- dir(path) ;
fileNames<-fileNames[-c(1:4,210:211,length(fileNames),length(fileNames)-1,length(fileNames)-2)]
fileNames<-fileNames[-grep('K562',fileNames)]
#fileNames<-fileNames[-c(1:2,208:209)]
filePath <- sapply(fileNames, function(x){ paste(path,x,sep='/')})
filePath<-filePath[-c(which(((file.info(filePath)$size)/1024^2)<1),which(((file.info(filePath)$size)/1024^2)>17))]
#filePath<-filePath[(length(filePath)-20):length(filePath)]
print(paste("共",length(filePath),"条待处理样本"))
samplematrix<-matrix(0,nrow = length(filePath),ncol=23)
for (i in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[i],header = T)
  for (j in 1:nrow(data1)) {
    if (data1[j,1]==data1[j,2]) {
      if (data1[j,1]=='X') {
        samplematrix[i,23]=samplematrix[i,23]+data1[j,7]
      }
      else{
        samplematrix[i,data1[j,1]]=samplematrix[i,data1[j,1]]+data1[j,7]
      }
    }
  }
  print(paste('第',i,'条样本计数完毕'))
}

path<-"/store/yhshao/all/k40";
resolution<-40000
fileNames <- dir(path) ;
fileNames<-fileNames[-c(1,2,206,207)]
filePath <- sapply(fileNames, function(x){ paste(path,x,sep='/')})
filePath<-filePath[-which(((file.info(filePath)$size)/1024^2)<2)]
#filePath<-filePath[(length(filePath)-20):length(filePath)]
samplematrix<-matrix(0,nrow = length(filePath),ncol=23)
print(paste("共",length(filePath),"条待处理样本"))
for (i in 1:length(filePath)) {
  data1<-read.csv(filePath[i],header = T)
  for (j in 1:nrow(data1)) {
    if (data1[j,1]==data1[j,2]) {
      if (data1[j,1]=='X') {
        samplematrix[i,23]=samplematrix[i,23]+data1[j,7]
      }
      else{
        samplematrix[i,data1[j,1]]=samplematrix[i,data1[j,1]]+data1[j,7]
      }
    }
  }
  print(paste('第',i,'条样本计数完毕'))
}

chrs<-seq(1,22)
chrs[23]<-'X'
for (i in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[i],header = T)
  a1<-0
  a2<-0
  intra_contacts<-0
  for (j in chrs) {
    a1<-which(data1$chr1==j)
    a2<-which(data1$chr2[a1]==j)
    intra_contacts<-a1[a2]
    if(length(intra_contacts)!=0){
      if(j=='X'){
        samplematrix[i,23]=samplematrix[i,23]+sum(data1$count[intra_contacts])
      }
      else{
        samplematrix[i,as.numeric(j)]=samplematrix[i,as.numeric(j)]+sum(data1$count[intra_contacts])
      }
    }
    else{continue}
  }
  print(paste('第',i,'条样本计数完毕'))
}


cellgroups<-array()
aa<-seq(1,length(fileNames))
bb<-grep('oocyte',fileNames[aa])
cellgroups[aa[bb]]<-'oocyte'
aa<-aa[-bb]
bb<-grep('pronucleus',fileNames[aa])
cellgroups[aa[bb]]<-'pronucleus'
aa<-aa[-bb]
bb<-grep('K562',fileNames[aa])
cellgroups[aa[bb]]<-'K562'
aa<-aa[-bb]
bb<-grep('Hoechst',fileNames[aa])
cellgroups[aa[bb]]<-'Hoechst'
aa<-aa[-bb]




#dividing by compartments
path<-"/store/yhshao/all/k200";
resolution<-200000
fileNames <- dir(path) ;
fileNames<-fileNames[-c(1:4,210:211,length(fileNames),length(fileNames)-1,length(fileNames)-2)]
fileNames<-fileNames[-grep('K562',fileNames)]
filePath <- sapply(fileNames, function(x){ paste(path,x,sep='/')})
print(paste("共",length(filePath),"条待处理样本"))
#reading in compartments file and preprocessing
#compartments<-read.csv(file.choose(),header = T)
compartments<-read.csv(file = '/store/yhshao/work/compartment/GSM2110110_MEL_1-200kb.cis.vecs.tsv',header = T)
aa<-array()
j=1
for (i in 1:nrow(compartments)) {
  if(strsplit(as.character(compartments[i,1]),split = '\t')[[1]][4]==''){
    aa[j]<-i;j=j+1;
  }
}
compartments<-compartments[-aa,1]
bb<-matrix(nrow = length(compartments),ncol = 4)
for (i in 1:nrow(bb)) {
  bb[i,]<-strsplit(as.character(compartments[i]),split = '\t')[[1]][1:4]
}
bb<-as.data.frame(bb)
bb$V2<-as.numeric(as.character(bb$V2))/resolution
bb$V3<-as.numeric(as.character(bb$V3))/resolution
bb$V4<-as.numeric(as.character(bb$V4))
colnames(bb)<-c('chrom','start','end','Evalue')
compartments<-bb
write.table(bb,file = '/store/yhshao/MEL_1_200kb_compartments')
#reading in TADs file and preprocessing
tads<-read.csv(file = "/store/yhshao/work/tad/GSM2110110_MEL_1-200kb_tad.txt")
aa<-array()
j=1
for (i in 1:nrow(tads)) {
  if(strsplit(as.character(tads[i,1]),split = '\t')[[1]][7]=='nan'){
    aa[j]<-i;j=j+1;
  }
}
tads<-tads[-aa,1]
bb<-matrix(nrow = length(tads),ncol = 3)
for (i in 1:nrow(bb)) {
  bb[i,]<-strsplit(as.character(tads[i]),split = '\t')[[1]][1:3]
}
bb<-as.data.frame(bb)
bb$V2<-as.numeric(as.character(bb$V2))/resolution
bb$V3<-as.numeric(as.character(bb$V3))/resolution
colnames(bb)<-c('chrom','start','end')
tads<-bb
write.table(tads,file = '/store/yhshao/MEL_1_200kb_tads')

compartments<-read.table(file = '/store/yhshao/MEL_1_200kb_compartments',header = T)
compartments$Evalue[which(compartments$Evalue>0)]<-1
compartments$Evalue[which(compartments$Evalue<0)]<--1

chrs<-seq(1,19)
chrs[20:21]<-c('X','Y')
matsize<-matrix(nrow = length(chrs),ncol = 4)
i=1
a1=0
a2=0
for (chrom in chrs) {
  nchrom<-paste('chr',chrom,sep ='')
  a1<-which(compartments$chrom==nchrom)[1]
  a2<-which(compartments$chrom==nchrom)[length(which(compartments$chrom==nchrom))]
  s1<-compartments$start[a1]
  s2<-compartments$end[a2]
  j=0
  for (m in a1:a2) {
    if(m==a2){
      j=j+1
      break()
    }
    if(compartments$Evalue[m]*compartments$Evalue[m+1]<0){
      j=j+1
    }
  }
  matsize[i,1]<-nchrom
  matsize[i,2]<-s1
  matsize[i,3]<-s2
  matsize[i,4]<-j
  i=i+1
}

#merge A/B compartments
samplematrix<-matrix(0,nrow = length(filePath),ncol=length(chrs)*2)
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = T)
  data1$interval1<-round(((data1$start1+data1$end1)/2)/resolution)
  data1$interval2<-round(((data1$start2+data1$end2)/2)/resolution)
  numchrom=1
  for (chrom in chrs) {
    if(length(which(data1$chr1==chrom))!=0){
      nchrom<-paste('chr',chrom,sep ='')
      compar<-compartments[which(compartments$chrom==nchrom),]
      compar$count<-compar$Evalue*0
      s1<-as.numeric(matsize[which(matsize[,1]==nchrom),2])
      s2<-as.numeric(matsize[which(matsize[,1]==nchrom),3])
      y<-matrix(0,nrow =s2,ncol =s2)
      a1<-which(data1$chr1==chrom)
      a2<-which(data1$chr2[a1]!=chrom)
      if(length(a2)>0){
        data1<-data1[-a1[a2],]
      }
      if(length(which(data1$interval1[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval1[which(data1$chr1==chrom)]>s2)],]
      }
      if(length(which(data1$interval2[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval2[which(data1$chr1==chrom)]>s2)],]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,8],data1[i,9]]<-data1[i,7]+y[data1[i,8],data1[i,9]];
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,9],data1[i,8]]<-data1[i,7]+y[data1[i,9],data1[i,8]];
      }
     
      i=1
      j=0
      s=compar$end[i]
      counts=0
      while (i!=nrow(compar)+1) {
        if(i==nrow(compar)){
          s=compar$end[i]
          for (m in compar$end[i]:compar$start[(i-j)]) {
            for (n in compar$start[(i-j)]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          compar$count[i-j]<-counts
          counts=0
          i=i+1
          j=0
          break()
        }
        if(compar$Evalue[i]*compar$Evalue[i+1]<0){ 
          s=compar$end[i]
          for (m in compar$end[i]:compar$start[(i-j)]) {
            for (n in compar$start[(i-j)]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          compar$count[i-j]<-counts-y[compar$end[i],compar$end[i]]
          counts=0
          i=i+1
          j=0
        }
        else{
          i=i+1
          j=j+1
        }

      }
      samplematrix[filenum,numchrom]<-sum(compar$count[which(compar$Evalue>0)])
      samplematrix[filenum,numchrom+1]<-sum(compar$count[which(compar$Evalue<0)])
      numchrom=numchrom+2
    }
    else{
      numchrom=numchrom+2
      continue
    }
  
  }
  print(paste('第',filenum,'条样本计数完毕'))
}
zerocol<-0
j=1
for (i in 1:ncol(samplematrix)) {
  if(sum(samplematrix[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_merge_100k_ES_com<-samplematrix[,-zerocol]
write.csv(samplematrix_merge_com,file = '/store/yhshao/samplematrix_200k_merge_com')




#don't merge A/B compartments
samplematrix<-matrix(0,nrow = length(filePath),ncol=sum(as.numeric(matsize[,4])))
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = T)
  data1$interval1<-round(((data1$start1+data1$end1)/2)/resolution)
  data1$interval2<-round(((data1$start2+data1$end2)/2)/resolution)
  comparsite=1
  for (chrom in chrs) {
    nchrom<-paste('chr',chrom,sep ='')
    if(length(which(data1$chr1==chrom))!=0){
      compar<-compartments[which(compartments$chrom==nchrom),]
      s1<-as.numeric(matsize[which(matsize[,1]==nchrom),2])
      s2<-as.numeric(matsize[which(matsize[,1]==nchrom),3])
      y<-matrix(0,nrow =s2,ncol =s2)
      a1<-which(data1$chr1==chrom)
      a2<-which(data1$chr2[a1]!=chrom)
      if(length(a2)>0){
        data1<-data1[-a1[a2],]
      }
      if(length(which(data1$interval1[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval1[which(data1$chr1==chrom)]>s2)],]
      }
      if(length(which(data1$interval2[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval2[which(data1$chr1==chrom)]>s2)],]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,8],data1[i,9]]<-data1[i,7]+y[data1[i,8],data1[i,9]]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,9],data1[i,8]]<-data1[i,7]+y[data1[i,9],data1[i,8]]
      }
      
      i=1
      j=0
      s=compar$end[i]
      counts=0
      while (i!=nrow(compar)+1) {
        if(i==nrow(compar)){
          s=compar$end[i]
          for (m in compar$end[i]:compar$start[(i-j)]) {
            for (n in compar$start[(i-j)]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          samplematrix[filenum,comparsite]<-counts
          i=i+1
          j=0
          comparsite=comparsite+1
          break()
        }
        if(compar$Evalue[i]*compar$Evalue[i+1]<0){ 
          s=compar$end[i]
          for (m in compar$end[i]:compar$start[(i-j)]) {
            for (n in compar$start[(i-j)]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          samplematrix[filenum,comparsite]<-counts-y[compar$end[i],compar$end[i]]
          comparsite=comparsite+1
          counts=0
          i=i+1
          j=0
        }
        else{
          i=i+1
          j=j+1
        }
      }
    }
    else{
      comparsite=comparsite+as.numeric(matsize[which(matsize[,1]==nchrom),4])-1
      continue
    }
    
  }
  print(paste('第',filenum,'条样本计数完毕'))
}
zerocol<-0
j=1
for (i in 1:ncol(samplematrix)) {
  if(sum(samplematrix[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_notmerge_com<-samplematrix[,-zerocol]
write.csv(samplematrix_notmerge_com,file = '/store/yhshao/samplematrix_200k_notmerge_com')



#dividing by TADs
tads<-read.table(file = '/store/yhshao/MEL_1_200kb_tads',header = T)

chrs<-seq(1,19)
chrs[20:21]<-c('X','Y')
matsize<-matrix(nrow = length(chrs),ncol = 4)
i=1
a1=0
a2=0
for (chrom in chrs) {
  nchrom<-paste('chr',chrom,sep ='')
  a1<-which(tads$chrom==nchrom)[1]
  a2<-which(tads$chrom==nchrom)[length(which(tads$chrom==nchrom))]
  s1<-tads$start[a1]
  s2<-tads$end[a2]
  matsize[i,1]<-nchrom
  matsize[i,2]<-s1
  matsize[i,3]<-s2
  matsize[i,4]<-length(which(tads$chrom==nchrom))-1
  i=i+1
}
if(length(which(as.numeric(matsize[,4])<2))>0){
  matsize<-matsize[-which(as.numeric(matsize[,4])<2),]
}

samplematrix<-matrix(0,nrow = length(filePath),ncol=nrow(tads)-nrow(matsize))
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = T)
  data1$interval1<-round(((data1$start1+data1$end1)/2)/resolution)
  data1$interval2<-round(((data1$start2+data1$end2)/2)/resolution)
  comparsite=1
  for (chrom in chrs) {
    nchrom<-paste('chr',chrom,sep ='')
    if(length(which(data1$chr1==chrom))!=0){
      compar<-tads[which(tads$chrom==nchrom),]
      s1<-as.numeric(matsize[which(matsize[,1]==nchrom),2])
      s2<-as.numeric(matsize[which(matsize[,1]==nchrom),3])
      y<-matrix(0,nrow =s2,ncol =s2)
      a1<-which(data1$chr1==chrom)
      a2<-which(data1$chr2[a1]!=chrom)
      if(length(a2)>0){
        data1<-data1[-a1[a2],]
      }
      if(length(which(data1$interval1[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval1[which(data1$chr1==chrom)]>s2)],]
      }
      if(length(which(data1$interval2[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval2[which(data1$chr1==chrom)]>s2)],]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,8],data1[i,9]]<-data1[i,7]+y[data1[i,8],data1[i,9]]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,9],data1[i,8]]<-data1[i,7]+y[data1[i,9],data1[i,8]]
      }
      
      counts=0
      for (i in 1:(nrow(compar)-1)) {
        if(i==(nrow(compar)-1)){
          if(i==1){
            s=compar$end[i+1]
            for (m in compar$end[i+1]:compar$start[i]) {
              for (n in compar$start[i]:s) {
                counts=counts+y[m,n]
              }
              s=s-1
            }
            samplematrix[filenum,comparsite]<-counts
            comparsite=comparsite+1
          }
          else{
            s=compar$end[i+1]
            for (m in compar$end[i+1]:compar$end[i]) {
              for (n in compar$end[i]:s) {
                counts=counts+y[m,n]
              }
              s=s-1
            }
            samplematrix[filenum,comparsite]<-counts
            comparsite=comparsite+1
          }
          break()
        }
        if(i==1){
          s=compar$end[i+1]
          for (m in compar$end[i+1]:compar$start[i]) {
            for (n in compar$start[i]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          samplematrix[filenum,comparsite]<-counts-y[compar$end[i+1],compar$end[i+1]]
          comparsite=comparsite+1
          counts=0
        }
        else{ 
          s=compar$end[i+1]
          for (m in compar$end[i+1]:compar$end[i]) {
            for (n in compar$end[i]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          samplematrix[filenum,comparsite]<-counts-y[compar$end[i+1],compar$end[i+1]]
          comparsite=comparsite+1
          counts=0
        }
      }
    }
    else{
      comparsite=comparsite+as.numeric(matsize[which(matsize[,1]==nchrom),4])-1
      continue
    }
  }
  print(paste('第',filenum,'条样本计数完毕'))
}
zerocol<-0
j=1
for (i in 1:ncol(samplematrix)) {
  if(sum(samplematrix[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_tad<-samplematrix[,-zerocol]
write.csv(samplematrix_tad,file = '/store/yhshao/samplematrix_200k_tad')

#208 sample------------------------------------------------------------------------

path<-"/store/yhshao/all/allreads";
resolution<-100000
fileNames <- dir(path) ;
fileNames<-fileNames[-c(1:2,207:208)]
fileNames<-fileNames[-grep('K562',fileNames)]
filePath <- sapply(fileNames, function(x){ paste(path,x,sep='/')})
print(paste("共",length(filePath),"条待处理样本"))
#reading in compartments file and preprocessing
#compartments<-read.csv(file.choose(),header = T)
compartments<-0
compartments<-read.csv(file = '/store/yhshao/work/compartment/ES_E14_GRCm38_100kb_com.cis.vecs.tsv',header = T)
aa<-array()
j=1
for (i in 1:nrow(compartments)) {
  if(strsplit(as.character(compartments[i,1]),split = '\t')[[1]][4]==''){
    aa[j]<-i;j=j+1;
  }
}
compartments<-compartments[-aa,1]
bb<-matrix(nrow = length(compartments),ncol = 4)
for (i in 1:nrow(bb)) {
  bb[i,]<-strsplit(as.character(compartments[i]),split = '\t')[[1]][1:4]
}
bb<-as.data.frame(bb)
bb$V2<-as.numeric(as.character(bb$V2))/resolution
bb$V3<-as.numeric(as.character(bb$V3))/resolution
bb$V4<-as.numeric(as.character(bb$V4))
colnames(bb)<-c('chrom','start','end','Evalue')
compartments<-bb
compartments$Evalue[which(compartments$Evalue>0)]<-1
compartments$Evalue[which(compartments$Evalue<0)]<--1
#reading in TADs file and preprocessing
tads<-0
tads<-read.csv(file = "/store/yhshao/work/tad/ES_E14_GRCm38_100kb_tad.txt",header = T)
aa<-array()
j=1
for (i in 1:nrow(tads)) {
  if(strsplit(as.character(tads[i,1]),split = '\t')[[1]][7]=='nan'){
    aa[j]<-i;j=j+1;
  }
}
tads<-tads[-aa,1]
bb<-matrix(nrow = length(tads),ncol = 3)
for (i in 1:nrow(bb)) {
  bb[i,]<-strsplit(as.character(tads[i]),split = '\t')[[1]][1:3]
}
bb<-as.data.frame(bb)
bb$V2<-as.numeric(as.character(bb$V2))/resolution
bb$V3<-as.numeric(as.character(bb$V3))/resolution
colnames(bb)<-c('chrom','start','end')
tads<-bb

#get matrix size
chrs<-seq(1,19)
chrs[20:21]<-c('X','Y')
matsize_com<-matrix(nrow = length(chrs),ncol = 4)
matsize_tad<-matrix(nrow = length(chrs),ncol = 4)
i=1
a1=0
a2=0
for (chrom in chrs) {
  nchrom<-paste('chr',chrom,sep ='')
  a1<-which(compartments$chrom==nchrom)[1]
  a2<-which(compartments$chrom==nchrom)[length(which(compartments$chrom==nchrom))]
  s1<-compartments$start[a1]
  s2<-compartments$end[a2]
  j=0
  for (m in a1:a2) {
    if(m==a2){
      j=j+1
      break()
    }
    if(compartments$Evalue[m]*compartments$Evalue[m+1]<0){
      j=j+1
    }
  }
  matsize_com[i,1]<-nchrom
  matsize_com[i,2]<-s1
  matsize_com[i,3]<-s2
  matsize_com[i,4]<-j
  a1<-which(tads$chrom==nchrom)[1]
  a2<-which(tads$chrom==nchrom)[length(which(tads$chrom==nchrom))]
  s1<-tads$start[a1]
  s2<-tads$end[a2]
  matsize_tad[i,1]<-nchrom
  matsize_tad[i,2]<-s1
  matsize_tad[i,3]<-s2
  matsize_tad[i,4]<-length(which(tads$chrom==nchrom))-1
  i=i+1
}
if(length(which(as.numeric(matsize_tad[,4])<2))>0){
  matsize_tad<-matsize_tad[-which(as.numeric(matsize_tad[,4])<2),]
}

#count of chromsome
t1=proc.time()
samplematrix_chr<-matrix(0,nrow = length(filePath),ncol=length(chrs))
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = T)
  a1<-0
  a2<-0
  intra_contacts<-0
  chrsite<-0
  for (j in chrs) {
    a1<-which(data1$chr1==j)
    a2<-which(data1$chr2[a1]==j)
    intra_contacts<-a1[a2]
    if(length(intra_contacts)!=0){
      samplematrix_chr[filenum,chrsite]=samplematrix_chr[filenum,chrsite]+length(intra_contacts)
      chrsite=chrsite+1
    }
    else{
      chrsite=chrsite+1
      continue
    }
  }
  print(paste('第',filenum,'条样本计数完毕'))
}
zerocol<-0
j=1
for (i in 1:ncol(samplematrix_chr)) {
  if(sum(samplematrix_chr[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_100k_ES_chr<-samplematrix_chr[,-zerocol]
write.csv(samplematrix_100k_ES_chr,file = '/store/yhshao/samplematrix_100k_ES_chr')
t2=proc.time()
t=t2-t1
print(paste0('分染色体执行时间：',t[3][[1]]/60,'分'))

#count of chromosome, merge and don't merge A/B compartments
t1=proc.time()
samplematrix_chr<-matrix(0,nrow = length(filePath),ncol=length(chrs))
samplematrix_com<-matrix(0,nrow = length(filePath),ncol=length(chrs)*2)
samplematrix_com2<-matrix(0,nrow = length(filePath),ncol=sum(as.numeric(matsize_com[,4])))
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = T)
  data1$interval1<-round(data1$pos1/resolution)
  data1$interval2<-round(data1$pos2/resolution)
  numchrom=1
  comparsite=1
  
  a1<-0
  a2<-0
  intra_contacts<-0
  chrsite<-0
  for (j in chrs) {
    a1<-which(data1$chr1==j)
    a2<-which(data1$chr2[a1]==j)
    intra_contacts<-a1[a2]
    if(length(intra_contacts)!=0){
      samplematrix_chr[filenum,chrsite]=samplematrix_chr[filenum,chrsite]+length(intra_contacts)
      chrsite=chrsite+1
    }
    else{
      chrsite=chrsite+1
      continue
    }
  }
  
  for (chrom in chrs) {
    nchrom<-paste('chr',chrom,sep ='')
    if(length(which(data1$chr1==chrom))!=0){
      s1=0
      s2=0
      a1=0
      a2=0
      compar<-0
      compar<-compartments[which(compartments$chrom==nchrom),]
      compar$count<-compar$Evalue*0
      s1<-as.numeric(matsize_com[which(matsize_com[,1]==nchrom),2])
      s2<-as.numeric(matsize_com[which(matsize_com[,1]==nchrom),3])
      y<-matrix(0,nrow =s2,ncol =s2)
      a1<-which(data1$chr1==chrom)
      a2<-which(data1$chr2[a1]!=chrom)
      if(length(a2)>0){
        data1<-data1[-a1[a2],]
      }
      if(length(which(data1$interval1[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval1[which(data1$chr1==chrom)]>s2)],]
      }
      if(length(which(data1$interval2[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval2[which(data1$chr1==chrom)]>s2)],]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,7],data1[i,8]]<-1+y[data1[i,7],data1[i,8]];
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,8],data1[i,7]]<-1+y[data1[i,8],data1[i,7]];
      }
      
      i=1
      j=0
      s=compar$end[i]
      counts=0
      while (i!=nrow(compar)+1) {
        if(i==nrow(compar)){
          s=compar$end[i]
          for (m in compar$end[i]:compar$start[(i-j)]) {
            for (n in compar$start[(i-j)]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          compar$count[i-j]<-counts
          samplematrix_com2[filenum,comparsite]<-counts
          comparsite=comparsite+1
          counts=0
          i=i+1
          j=0
          break()
        }
        if(compar$Evalue[i]*compar$Evalue[i+1]<0){ 
          s=compar$end[i]
          for (m in compar$end[i]:compar$start[(i-j)]) {
            for (n in compar$start[(i-j)]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          compar$count[i-j]<-counts-y[compar$end[i],compar$end[i]]
          samplematrix_com2[filenum,comparsite]<-counts-y[compar$end[i],compar$end[i]]
          comparsite=comparsite+1
          counts=0
          i=i+1
          j=0
        }
        else{
          i=i+1
          j=j+1
        }
      }
      samplematrix_com[filenum,numchrom]<-sum(compar$count[which(compar$Evalue>0)])
      samplematrix_com[filenum,numchrom+1]<-sum(compar$count[which(compar$Evalue<0)])
      numchrom=numchrom+2
      
    }
    else{
      numchrom=numchrom+2
      comparsite=comparsite+as.numeric(matsize_com[which(matsize_com[,1]==nchrom),4])-1
      continue
    }
    
  }
  print(paste('第',filenum,'条样本计数完毕'))
}

zerocol<-0
j=1
for (i in 1:ncol(samplematrix_chr)) {
  if(sum(samplematrix_chr[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_100k_ES_chr<-samplematrix_chr[,-zerocol]
write.csv(samplematrix_100k_ES_chr,file = '/store/yhshao/samplematrix_100k_ES_chr')

zerocol2<-0
j=1
for (i in 1:ncol(samplematrix_com)) {
  if(sum(samplematrix_com[,i])==0){
    zerocol2[j]<-i
    j=j+1
  }
}
samplematrix_merge_100k_ES_com<-samplematrix_com[,-zerocol2]
write.csv(samplematrix_merge_100k_ES_com,file = '/store/yhshao/samplematrix_merge_100k_ES_com')

zerocol3<-0
j=1
for (i in 1:ncol(samplematrix_com2)) {
  if(sum(samplematrix_com2[,i])==0){
    zerocol3[j]<-i
    j=j+1
  }
}
samplematrix_notmerge_100k_ES_com<-samplematrix_com2[,-zerocol3]
write.csv(samplematrix_notmerge_100k_ES_com,file = '/store/yhshao/samplematrix_notmerge_100k_ES_com')
t2=proc.time()
t=t2-t1
print(paste0('分compartment执行时间：',t[3][[1]]/60,'分'))




#dividing by TADs
t1=proc.time()
samplematrix_tad<-matrix(0,nrow = length(filePath),ncol=nrow(tads)-nrow(matsize_tad))
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = T)
  data1$interval1<-round(data1$pos1/resolution)
  data1$interval2<-round(data1$pos2/resolution)
  tadsite=1
  for (chrom in chrs) {
    nchrom<-paste('chr',chrom,sep ='')
    if(length(which(data1$chr1==chrom))!=0){
      s1=0
      s2=0
      a1=0
      a2=0
      compar<-0
      compar<-tads[which(tads$chrom==nchrom),]
      s1<-as.numeric(matsize_tad[which(matsize_tad[,1]==nchrom),2])
      s2<-as.numeric(matsize_tad[which(matsize_tad[,1]==nchrom),3])
      y<-matrix(0,nrow =s2,ncol =s2)
      a1<-which(data1$chr1==chrom)
      a2<-which(data1$chr2[a1]!=chrom)
      if(length(a2)>0){
        data1<-data1[-a1[a2],]
      }
      if(length(which(data1$interval1[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval1[which(data1$chr1==chrom)]>s2)],]
      }
      if(length(which(data1$interval2[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval2[which(data1$chr1==chrom)]>s2)],]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,7],data1[i,8]]<-1+y[data1[i,7],data1[i,8]];
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,8],data1[i,7]]<-1+y[data1[i,8],data1[i,7]];
      }
      
      counts=0
      for (i in 1:(nrow(compar)-1)) {
        if(i==(nrow(compar)-1)){
          if(i==1){
            s=compar$end[i+1]
            for (m in compar$end[i+1]:compar$start[i]) {
              for (n in compar$start[i]:s) {
                counts=counts+y[m,n]
              }
              s=s-1
            }
            samplematrix_tad[filenum,tadsite]<-counts
            tadsite=tadsite+1
          }
          else{
            s=compar$end[i+1]
            for (m in compar$end[i+1]:compar$end[i]) {
              for (n in compar$end[i]:s) {
                counts=counts+y[m,n]
              }
              s=s-1
            }
            samplematrix_tad[filenum,tadsite]<-counts
            tadsite=tadsite+1
          }
          break()
        }
        if(i==1){
          s=compar$end[i+1]
          for (m in compar$end[i+1]:compar$start[i]) {
            for (n in compar$start[i]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          samplematrix_tad[filenum,tadsite]<-counts-y[compar$end[i+1],compar$end[i+1]]
          tadsite=tadsite+1
          counts=0
        }
        else{ 
          s=compar$end[i+1]
          for (m in compar$end[i+1]:compar$end[i]) {
            for (n in compar$end[i]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          samplematrix_tad[filenum,tadsite]<-counts-y[compar$end[i+1],compar$end[i+1]]
          tadsite=tadsite+1
          counts=0
        }
      }
    }
    else{
      tadsite=tadsite+as.numeric(matsize_tad[which(matsize_tad[,1]==nchrom),4])-1
      continue
    }
  }
  print(paste('第',filenum,'条样本计数完毕'))
}
zerocol<-0
j=1
for (i in 1:ncol(samplematrix_tad)) {
  if(sum(samplematrix_tad[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_tad<-samplematrix_tad[,-zerocol]
write.csv(samplematrix_tad,file = '/store/yhshao/samplematrix_100k_ES_tad')
t2=proc.time()
t=t2-t1
print(paste0('分TAD执行时间：',t[3][[1]]/60,'分'))


#622 sample------------------------------------------------------------------------

path<-"/store/yhshao/GSE124391_RAW";
resolution<-100000
fileNames <- dir(path) ;
fileNames<-fileNames[-c(1:5)]
sana_mESC<-read.csv(file = '/store/yhshao/mESC.txt',header = T)
sana_mESC<-sana_mESC[order(sana_mESC$GEO_Accession..exp.),]
sana_mESC<-sana_mESC[-c(1:3),]
fileNames<-fileNames[-which(sana_mESC$Organism=='Homo sapiens')]
sana_mESC<-sana_mESC[-which(sana_mESC$Organism=='Homo sapiens'),]
filePath <- sapply(fileNames, function(x){ paste(path,x,sep='/')})
print(paste("共",length(filePath),"条待处理样本"))
#reading in compartments file and preprocessing
#compartments<-read.csv(file.choose(),header = T)
compartments<-0
compartments<-read.csv(file = '/store/yhshao/work/compartment/ES_E14_GRCm38_100kb_com.cis.vecs.tsv',header = T)
aa<-array()
j=1
for (i in 1:nrow(compartments)) {
  if(strsplit(as.character(compartments[i,1]),split = '\t')[[1]][4]==''){
    aa[j]<-i;j=j+1;
  }
}
compartments<-compartments[-aa,1]
bb<-matrix(nrow = length(compartments),ncol = 4)
for (i in 1:nrow(bb)) {
  bb[i,]<-strsplit(as.character(compartments[i]),split = '\t')[[1]][1:4]
}
bb<-as.data.frame(bb)
bb$V2<-as.numeric(as.character(bb$V2))/resolution
bb$V3<-as.numeric(as.character(bb$V3))/resolution
bb$V4<-as.numeric(as.character(bb$V4))
colnames(bb)<-c('chrom','start','end','Evalue')
compartments<-bb
compartments$Evalue[which(compartments$Evalue>0)]<-1
compartments$Evalue[which(compartments$Evalue<0)]<--1
#reading in TADs file and preprocessing
tads<-0
tads<-read.csv(file = "/store/yhshao/work/tad/ES_E14_GRCm38_100kb_tad.txt",header = T)
aa<-array()
j=1
for (i in 1:nrow(tads)) {
  if(strsplit(as.character(tads[i,1]),split = '\t')[[1]][7]=='nan'){
    aa[j]<-i;j=j+1;
  }
}
tads<-tads[-aa,1]
bb<-matrix(nrow = length(tads),ncol = 3)
for (i in 1:nrow(bb)) {
  bb[i,]<-strsplit(as.character(tads[i]),split = '\t')[[1]][1:3]
}
bb<-as.data.frame(bb)
bb$V2<-as.numeric(as.character(bb$V2))/resolution
bb$V3<-as.numeric(as.character(bb$V3))/resolution
colnames(bb)<-c('chrom','start','end')
tads<-bb

#get matrix size
chrs<-seq(1,19)
chrs[20:21]<-c('X','Y')
chrs<-paste('chr',chrs,sep ='')
matsize_com<-matrix(nrow = length(chrs),ncol = 4)
matsize_tad<-matrix(nrow = length(chrs),ncol = 4)
i=1
a1=0
a2=0
for (chrom in chrs) {
  a1<-which(compartments$chrom==chrom)[1]
  a2<-which(compartments$chrom==chrom)[length(which(compartments$chrom==chrom))]
  s1<-compartments$start[a1]
  s2<-compartments$end[a2]
  j=0
  for (m in a1:a2) {
    if(m==a2){
      j=j+1
      break()
    }
    if(compartments$Evalue[m]*compartments$Evalue[m+1]<0){
      j=j+1
    }
  }
  matsize_com[i,1]<-chrom
  matsize_com[i,2]<-s1
  matsize_com[i,3]<-s2
  matsize_com[i,4]<-j
  a1<-which(tads$chrom==chrom)[1]
  a2<-which(tads$chrom==chrom)[length(which(tads$chrom==chrom))]
  s1<-tads$start[a1]
  s2<-tads$end[a2]
  matsize_tad[i,1]<-chrom
  matsize_tad[i,2]<-s1
  matsize_tad[i,3]<-s2
  matsize_tad[i,4]<-length(which(tads$chrom==chrom))-1
  i=i+1
}
if(length(which(as.numeric(matsize_tad[,4])<2))>0){
  matsize_tad<-matsize_tad[-which(as.numeric(matsize_tad[,4])<2),]
}

#count of chromsome
t1=proc.time()
samplematrix_chr<-matrix(0,nrow = length(filePath),ncol=length(chrs))
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = F)
  dd<-matrix(nrow = nrow(data1),ncol = 4)
  for (i in 1:nrow(data1)) {
    dd[i,]<-strsplit(as.character(data1[i,]),split = "\t")[[1]]
  }
  data1<-as.data.frame(dd)
  colnames(data1)<-c('chr1','pos1','chr2','pos2')
  a1<-0
  a2<-0
  intra_contacts<-0
  chrsite<-0
  for (j in chrs) {
    a1<-which(data1$chr1==j)
    a2<-which(data1$chr2[a1]==j)
    intra_contacts<-a1[a2]
    if(length(intra_contacts)!=0){
      samplematrix_chr[filenum,chrsite]=samplematrix_chr[filenum,chrsite]+length(intra_contacts)
      chrsite=chrsite+1
    }
    else{
      chrsite=chrsite+1
      continue
    }
  }
  print(paste('第',filenum,'条样本计数完毕'))
}
zerocol<-0
j=1
for (i in 1:ncol(samplematrix_chr)) {
  if(sum(samplematrix_chr[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_100k_ES_chr<-samplematrix_chr[,-zerocol]
write.csv(samplematrix_100k_ES_chr,file = '/store/yhshao/samplematrix_100k_ES_chr')
t2=proc.time()
t=t2-t1
print(paste0('分染色体执行时间：',t[3][[1]]/60,'分'))

#count of chromosome, merge and don't merge A/B compartments
t1=proc.time()
samplematrix_chr<-matrix(0,nrow = length(filePath),ncol=length(chrs))
samplematrix_com<-matrix(0,nrow = length(filePath),ncol=length(chrs)*2)
samplematrix_com2<-matrix(0,nrow = length(filePath),ncol=sum(as.numeric(matsize_com[,4])))
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = F)
  dd<-matrix(nrow = nrow(data1),ncol = 4)
  for (i in 1:nrow(data1)) {
    dd[i,]<-strsplit(as.character(data1[i,]),split = "\t")[[1]]
  }
  data1<-as.data.frame(dd)
  colnames(data1)<-c('chr1','pos1','chr2','pos2')
  a1<-0
  a2<-0
  intra_contacts<-0
  data1$interval1<-round(data1$pos1/resolution)
  data1$interval2<-round(data1$pos2/resolution)
  numchrom=1
  comparsite=1
  chrsite<-1
  intra_contacts<-0
  
  for (j in chrs) {
    a1<-which(data1$chr1==j)
    a2<-which(data1$chr2[a1]==j)
    intra_contacts<-a1[a2]
    if(length(intra_contacts)!=0){
      samplematrix_chr[filenum,chrsite]=samplematrix_chr[filenum,chrsite]+length(intra_contacts)
      chrsite=chrsite+1
    }
    else{
      chrsite=chrsite+1
      continue
    }
  }
  
  for (chrom in chrs) {
    if(length(which(data1$chr1==chrom))!=0){
      s1=0
      s2=0
      a1=0
      a2=0
      compar<-0
      compar<-compartments[which(compartments$chrom==chrom),]
      compar$count<-compar$Evalue*0
      s1<-as.numeric(matsize_com[which(matsize_com[,1]==chrom),2])
      s2<-as.numeric(matsize_com[which(matsize_com[,1]==chrom),3])
      y<-matrix(0,nrow =s2,ncol =s2)
      a1<-which(data1$chr1==chrom)
      a2<-which(data1$chr2[a1]!=chrom)
      if(length(a2)>0){
        data1<-data1[-a1[a2],]
      }
      if(length(which(data1$interval1[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval1[which(data1$chr1==chrom)]>s2)],]
      }
      if(length(which(data1$interval2[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval2[which(data1$chr1==chrom)]>s2)],]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,5],data1[i,6]]<-1+y[data1[i,5],data1[i,6]];
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,6],data1[i,5]]<-1+y[data1[i,6],data1[i,5]];
      }
      
      i=1
      j=0
      s=compar$end[i]
      counts=0
      while (i!=nrow(compar)+1) {
        if(i==nrow(compar)){
          s=compar$end[i]
          for (m in compar$end[i]:compar$start[(i-j)]) {
            for (n in compar$start[(i-j)]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          compar$count[i-j]<-counts
          samplematrix_com2[filenum,comparsite]<-counts
          comparsite=comparsite+1
          counts=0
          i=i+1
          j=0
          break()
        }
        if(compar$Evalue[i]*compar$Evalue[i+1]<0){ 
          s=compar$end[i]
          for (m in compar$end[i]:compar$start[(i-j)]) {
            for (n in compar$start[(i-j)]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          compar$count[i-j]<-counts-y[compar$end[i],compar$end[i]]
          samplematrix_com2[filenum,comparsite]<-counts-y[compar$end[i],compar$end[i]]
          comparsite=comparsite+1
          counts=0
          i=i+1
          j=0
        }
        else{
          i=i+1
          j=j+1
        }
      }
      samplematrix_com[filenum,numchrom]<-sum(compar$count[which(compar$Evalue>0)])
      samplematrix_com[filenum,numchrom+1]<-sum(compar$count[which(compar$Evalue<0)])
      numchrom=numchrom+2
      
    }
    else{
      numchrom=numchrom+2
      comparsite=comparsite+as.numeric(matsize_com[which(matsize_com[,1]==chrom),4])-1
      continue
    }
    
  }
  print(paste('第',filenum,'条样本计数完毕'))
}

zerocol<-0
j=1
for (i in 1:ncol(samplematrix_chr)) {
  if(sum(samplematrix_chr[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_100k_ES_chr<-samplematrix_chr[,-zerocol]
write.csv(samplematrix_100k_ES_chr,file = '/store/yhshao/samplematrix_100k_ES_mESC_chr')

zerocol2<-0
j=1
for (i in 1:ncol(samplematrix_com)) {
  if(sum(samplematrix_com[,i])==0){
    zerocol2[j]<-i
    j=j+1
  }
}
samplematrix_merge_100k_ES_com<-samplematrix_com[,-zerocol2]
write.csv(samplematrix_merge_100k_ES_com,file = '/store/yhshao/samplematrix_merge_100k_ES_mESC_com')

zerocol3<-0
j=1
for (i in 1:ncol(samplematrix_com2)) {
  if(sum(samplematrix_com2[,i])==0){
    zerocol3[j]<-i
    j=j+1
  }
}
samplematrix_notmerge_100k_ES_com<-samplematrix_com2[,-zerocol3]
write.csv(samplematrix_notmerge_100k_ES_com,file = '/store/yhshao/samplematrix_notmerge_100k_ES_mESC_com')
t2=proc.time()
t=t2-t1
print(paste0('分compartment执行时间：',t[3][[1]]/60,'分'))




#dividing by TADs
t1=proc.time()
samplematrix_tad<-matrix(0,nrow = length(filePath),ncol=nrow(tads)-nrow(matsize_tad))
for (filenum in 1:length(filePath)) {
  data1<-0
  data1<-read.csv(filePath[filenum],header = F)
  dd<-matrix(nrow = nrow(data1),ncol = 4)
  for (i in 1:nrow(data1)) {
    dd[i,]<-strsplit(as.character(data1[i,]),split = "\t")[[1]]
  }
  data1<-as.data.frame(dd)
  colnames(data1)<-c('chr1','pos1','chr2','pos2')
  data1$interval1<-round(data1$pos1/resolution)
  data1$interval2<-round(data1$pos2/resolution)
  tadsite=1
  for (chrom in chrs) {
    if(length(which(data1$chr1==chrom))!=0){
      s1=0
      s2=0
      a1=0
      a2=0
      compar<-0
      compar<-tads[which(tads$chrom==chrom),]
      s1<-as.numeric(matsize_tad[which(matsize_tad[,1]==chrom),2])
      s2<-as.numeric(matsize_tad[which(matsize_tad[,1]==chrom),3])
      y<-matrix(0,nrow =s2,ncol =s2)
      a1<-which(data1$chr1==chrom)
      a2<-which(data1$chr2[a1]!=chrom)
      if(length(a2)>0){
        data1<-data1[-a1[a2],]
      }
      if(length(which(data1$interval1[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval1[which(data1$chr1==chrom)]>s2)],]
      }
      if(length(which(data1$interval2[which(data1$chr1==chrom)]>s2))>0){
        data1<-data1[-which(data1$chr1==chrom)[which(data1$interval2[which(data1$chr1==chrom)]>s2)],]
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,5],data1[i,6]]<-1+y[data1[i,5],data1[i,6]];
      }
      for (i in which(data1$chr1==chrom)) 
      {
        y[data1[i,6],data1[i,5]]<-1+y[data1[i,6],data1[i,5]];
      }
      
      counts=0
      for (i in 1:(nrow(compar)-1)) {
        if(i==(nrow(compar)-1)){
          if(i==1){
            s=compar$end[i+1]
            for (m in compar$end[i+1]:compar$start[i]) {
              for (n in compar$start[i]:s) {
                counts=counts+y[m,n]
              }
              s=s-1
            }
            samplematrix_tad[filenum,tadsite]<-counts
            tadsite=tadsite+1
          }
          else{
            s=compar$end[i+1]
            for (m in compar$end[i+1]:compar$end[i]) {
              for (n in compar$end[i]:s) {
                counts=counts+y[m,n]
              }
              s=s-1
            }
            samplematrix_tad[filenum,tadsite]<-counts
            tadsite=tadsite+1
          }
          break()
        }
        if(i==1){
          s=compar$end[i+1]
          for (m in compar$end[i+1]:compar$start[i]) {
            for (n in compar$start[i]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          samplematrix_tad[filenum,tadsite]<-counts-y[compar$end[i+1],compar$end[i+1]]
          tadsite=tadsite+1
          counts=0
        }
        else{ 
          s=compar$end[i+1]
          for (m in compar$end[i+1]:compar$end[i]) {
            for (n in compar$end[i]:s) {
              counts=counts+y[m,n]
            }
            s=s-1
          }
          samplematrix_tad[filenum,tadsite]<-counts-y[compar$end[i+1],compar$end[i+1]]
          tadsite=tadsite+1
          counts=0
        }
      }
    }
    else{
      tadsite=tadsite+as.numeric(matsize_tad[which(matsize_tad[,1]==chrom),4])-1
      continue
    }
  }
  print(paste('第',filenum,'条样本计数完毕'))
}
zerocol<-0
j=1
for (i in 1:ncol(samplematrix_tad)) {
  if(sum(samplematrix_tad[,i])==0){
    zerocol[j]<-i
    j=j+1
  }
}
samplematrix_tad<-samplematrix_tad[,-zerocol]
write.csv(samplematrix_tad,file = '/store/yhshao/samplematrix_100k_ES_mESC_tad')
t2=proc.time()
t=t2-t1
print(paste0('分TAD执行时间：',t[3][[1]]/60,'分'))
