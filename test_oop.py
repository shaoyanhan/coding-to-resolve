#生信1702 邵燕涵 2017317220205

#定义基类
class Molecule():
    def __init__(self):#初始化
        self.elements = set()#元素组成集合
        self.weight = None#分子量
    def show_weight(self):#打印分子量
        print(self.weight)
    def show_elements(self):#打印元素组成字典
        print(self.elements)

#定义氨基酸类（继承自基类）
class AminoAcid(Molecule):
    def __init__(self):
        super().__init__()#继承父类函数
        self.elements = {'C': 0, 'H': 0, 'O': 0, 'N': 0, 'S': 0}#初始化
        self.score = {'C': 12, 'H': 1, 'O': 16, 'N': 14, 'S': 32}#各元素的相对原子质量
    def calc_mw(self):#计算分子量
        sum=0
        for i in self.elements:
            sum += self.elements[i]*self.score[i]
        self.weight=sum
    def show_weight(self):#重载分子量打印函数，同时计算氨基酸分子量
        self.calc_mw()
        super().show_weight()
    def show_elements(self):#重载原子组成字典打印函数，用字典中非零元素生成元素集合
        a=set()
        for i in self.elements:
            if self.elements[i]!=0:
                a.add(i)
        print(a)

#亮氨酸类（继承自氨基酸类）
class Leucine(AminoAcid):
    def __init__(self):
        super().__init__()
        self.elements = {'C': 6, 'H': 13, 'O': 2, 'N': 1, 'S': 0}
    def show_composition(self):
        print(self.elements)

#异亮氨酸类（继承自氨基酸类）
class Isoleucine(AminoAcid):
    def __init__(self):
        super().__init__()
        self.elements = {'C': 6, 'H': 13, 'O': 2, 'N': 1, 'S': 0}
    def show_composition(self):
        print(self.elements)
    def is_isoform(self,stimiuli):#判断同分异构体方法，输入一个氨基酸对象，根据氨基酸元素字典判断是否为同分异构体
        if stimiuli.elements==self.elements:
            print('TRUE')
        else:
            print('FALSE')

#半胱氨酸类（继承自氨基酸类）
class Cysteine(AminoAcid):
    def __init__(self):
        super().__init__()
        self.elements = {'C': 3, 'H': 7, 'O': 2, 'N': 1, 'S': 1}
    def show_composition(self):
        print(self.elements)

#生成对象并调用函数
leu=Leucine()
iso=Isoleucine()
cys=Cysteine()

leu.show_weight()
leu.show_elements()
leu.show_composition()

iso.show_weight()
iso.show_elements()
iso.show_composition()

cys.show_weight()
cys.show_elements()
cys.show_composition()

#检验同分异构体算法
iso.is_isoform(leu)
iso.is_isoform(cys)
