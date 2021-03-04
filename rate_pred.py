#生信1702 邵燕涵 2017317220205
#蛋白质序列文件：ProSeq.fa.fasta
#计算结果文件：ProFoldingRate.txt


#引入模块
from urllib.request import urlopen#引入网络通讯所需模块
from urllib.parse import urlencode
from time import sleep#引入sleep模块

#读取并处理蛋白序列
f=open('/Users/macbookair/Desktop/ProSeq.fa.fasta','r')#从ncbi收集十条蛋白序列信息存于ProSeq.fa.fasta文件中，读取该文件
Seq={}#定义蛋白序列字典，用于存储各条蛋白序列
for line in f.readlines():
    if line.startswith('>'):#找到ID行
        id=line.rstrip().split('>')[1]#去除'>'符号，去除换行符
        Seq[id]=''#初始化
    elif not len(line):#跳过空行
        continue
    else:#去除蛋白序列的换行符，合并为整序列存入字典的相应ID中
        Seq[id]=Seq[id]+line.rstrip()
f.close()#读取结束

#上传数据并计算折叠速率
url="http://ibi.hzau.edu.cn/FDserver/cido.php"#提交序列的网址
foldingrate={}#定义一个字典用于存储蛋白质预测的折叠速率
for n in Seq:#逐条上传蛋白序列
    inputs = {'textarea': Seq[n], 'radiobutton': 'Unknown', 'ButtonRatePred': 'Predict Folding Rate'}#网页选项设置
    params = bytes(urlencode(inputs),encoding='utf-8')#传输数据
    f = urlopen(url, params) #open url with post method
    result = f.read()#读取网站计算结果
    f.close()
    result=bytes.decode(result)#将byte型转为txt文件可存储的字符串格式
    foldingrate[n]=result.split('(Kf) = ')[1].split('/sec.')[0]#通过字符串切割过滤得到计算数据并存储
    sleep(2)#上传间隔为两秒

#将结果按照规定格式存储为ProFoldingRate.txt
title='ProID','FoldingRate(/second)'#标题行
s=open('/Users/macbookair/Desktop/ProFoldingRate.txt','wt')#创建文件
ostr = '\t'.join(title)#制表符分隔
s.write(ostr + '\n')#写入标题行
ostr=''
for n in foldingrate:#将数据用制表符和换行符规范化
    ostr=ostr+n+'\t'+foldingrate[n]+'\n'
s.write(ostr)#写入数据
s.close()#关闭文件，写入完成