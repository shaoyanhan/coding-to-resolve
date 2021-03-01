a<-read.csv(file.choose(),header = T);
b<-read.csv(file.choose(),skip = 1,header = F);
remove(c)
c<-paste(b[1:nrow(b),],sep = "",collapse = "");


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
DNAstr2<-x;

for (i in 1:nrow(a)) {
  a$start[i]<-unlist(strsplit(as.character(a$Location[i]),"..",fixed=T))[1];
  a$end[i]<-unlist(strsplit(as.character(a$Location[i]),"..",fixed=T))[2];
}
b<-a[which(a$Strand=="+"),];
c<-a[which(a$Strand=="-"),];

mRNA<-list();
for (m in 1:nrow(b)) {
  e<-DNAstr[(as.numeric(b$start)[m]):as.numeric((b$end)[m])];
  e<-paste(e,sep = "",collapse = "");
  mRNA[m]<-e;
}

mRNA2<-list();
for (m in 1:nrow(c)) {
  e<-rev(DNAstr2[c$start[m]:c$end[m]]);
  e<-paste(e,sep = "",collapse = "");
  mRNA2[m]<-e;
}

s1=c("TTT","TTC","TTA","TTG","CTT","CTC","CTA","CTG","ATT","ATC","ATA","ATG","GTT","GTC","GTA","GTG")
s2=c("F","F","L","L","L","L","L","L","I","I","I","M","V","V","V","V")
s3=c("TCT","TCC","TCA","TCG","CCT","CCC","CCA","CCG","ACT","ACC","ACA","ACG","GCT","GCC","GCA","GCG")
s4=c("S","S","S","S","P","P","P","P","T","T","T","T","A","A","A","A")
s5=c("TAT","TAC","TAA","TAG","CAT","CAC","CAA","CAG","AAT","AAC","AAA","AAG","GAT","GAC","GAA","GAG")
s6=c("Y","Y","STOP","STOP","H","H","Q","Q","N","N","K","K","D","D","E","E")
s7=c("TGT","TGC","TGA","TGG","CGT","CGC","CGA","CGG","AGT","AGC","AGA","AGG","GGT","GGC","GGA","GGG")
s8=c("C","C","STOP","W","R","R","R","R","S","S","R","R","G","G","G","G")
mmz<-data.frame(s1,s2,s3,s4,s5,s6,s7,s8)

#+
allproteinstr<-list();
for (p in 1:length(mRNA)) {
  e<-unlist(strsplit(as.character(mRNA[p]),split = ""))
  j=1;m=0;n=0;proteinstr<-character();
  for (i in seq(from=1,to=length(e),by=3)) {
    k<-paste(e[i:(i+2)],sep = "",collapse = "");
    f<-which(mmz==k);
    m<-f%%16;
    if(m==0){
      m=16;n=f/16;
    }else{
      n<-floor((f/16)+1);
    }
    proteinstr[j]<-as.character(mmz[m,(n+1)])
    j=j+1;
  }
  allproteinstr[p]<-paste(proteinstr,sep = "",collapse = "");
}

#-
allproteinstr2<-list();
for (p in 1:length(mRNA2)) {
  e<-unlist(strsplit(as.character(mRNA2[p]),split = ""))
  j=1;m=0;n=0;proteinstr<-character();
  for (i in seq(from=1,to=length(e),by=3)) {
    k<-paste(e[i:(i+2)],sep = "",collapse = "");
    f<-which(mmz==k);
    m<-f%%16;
    if(m==0){
      m=16;n=f/16;
    }else{
      n<-floor((f/16)+1);
    }
    proteinstr[j]<-as.character(mmz[m,(n+1)])
    j=j+1;
  }
  allproteinstr2[p]<-paste(proteinstr,sep = "",collapse = "");
}







