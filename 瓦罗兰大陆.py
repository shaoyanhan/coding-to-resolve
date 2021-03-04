# python大作业：瓦罗兰大陆
# 作者信息：生信1702 邵燕涵 2017317220205
# 运行环境：软件：PyCharm(2019.3.1); 环境：python-3.8.0-amd64
"""
作业描述：本作业为回合制对战游戏，采用文字报幕的形式进行对战结果的呈现，没有图形界面的涉及，因此为了达到一定的游戏性，
文字报幕使用sleep函数控制时间，以达到肉眼可见的报幕速度,在结果输出过程中任何卡顿的现象皆为使用了sleep函数进行报幕时间
的控制，并非计算卡顿的现象，对于英雄和敌人角色的属性（类的实例化），作者自行设定了一个能够打通关的值，如果感觉有难度可
以自行在主函数处更改，更多详情请运行代码或查看源代码注释
"""

from abc import ABCMeta, abstractmethod  # 引入abc模块以用于创建基类
from random import randint, randrange  # 引入random模块以使用随机数函数
from time import sleep  # 引入sleep函数以用于控制游戏的输出和界面细节的呈现


class RPG(object, metaclass=ABCMeta):  # 定义一个人物基类，作为英雄角色和敌人角色类的基础
    __slots__ = ('_name', '_hp', '_cd', '_first_hp')  # 使用slot方法来限定自定义类型的对象只能绑定这些属性

    def __init__(self, name, hp, cd):  # 初始化
        self._name = name  # 名字
        self._hp = hp  # 生命值
        self._cd = cd  # 技能点
        self._first_hp = hp  # 记录初始生命值

    @property  # 使用property包装器，暗示该属性是受保护的，以确保访问的安全性
    def name(self):
        return self._name

    @property
    def hp(self):
        return self._hp

    @property
    def cd(self):
        return self._cd

    @hp.setter  # 使用setter方法修改变量，以确保当变量在程序运行过程中小于0时得以纠正
    def hp(self, hp):
        self._hp = hp if hp >= 0 else 0

    @cd.setter
    def cd(self, cd):
        self._cd = cd if cd >= 0 else 0

    @property  # 判断角色存活状态
    def alive(self):
        return self._hp > 0

    @abstractmethod  # 创建静态方法，普通攻击函数，以用于后续继承使用
    def attack(self, target):
        pass


class Hero(RPG):  # 定义英雄角色类，继承自角色基类
    __slots__ = ('_name', '_hp', '_cd', '_mana', '_first_hp', '_first_mana')

    def __init__(self, name, hp, mana, cd):  # 初始化一个新的属性：法力值
        super().__init__(name, hp, cd)
        self._mana = mana
        self._first_hp = hp
        self._first_mana = mana  # 记录初始法力值

    @property
    def mana(self):
        return self._mana

    def mana_resume(self):  # 法力值恢复计算方法
        if self._mana < self._first_mana:  # 当角色法力值未充满时
            resume_point = randint(10, 20)  # 法力值随机恢复
            if self._mana + resume_point <= self._first_mana:  # 判断法力值恢复时是否会溢出
                self._mana += resume_point
                print('英雄%s恢复了%d点法力值' % (self._name, resume_point))
            else:
                self._mana = self._first_mana
                print('英雄%s恢复了%d点法力值' % (self._name, (self._first_mana - self._mana)))
        else:  # 当角色法力值充足时
            print('英雄%s法力值充足' % self._name)

    def skill_point(self, n):  # 终极技能充能方法，n为充能值
        if self._cd + n >= 10:  # 充能完成
            self._cd = 10
            print('英雄%s的终极技能准备就绪！！！\n' % self._name)
            sleep(1)
        else:  # 充能进行中
            self._cd += n
            print('英雄%s的终极技能充能完成%d%%\n' % (self._name, (self._cd * 10)))  # 转化为百分制
            sleep(1)

    def attack(self, target):  # 普通攻击方法，target为攻击目标，攻击可以恢复法力值
        injury_value = randint(10, 15)  # 伤害值范围
        target.hp -= injury_value  # 实现攻击
        print('英雄%s使用了普通攻击，对敌人造成%d点伤害' % (self._name, injury_value))
        self.mana_resume()  # 恢复法力值
        self.skill_point(1)  # 获得终极技能充能1点

    def hardhit_attack(self, target):  # 普通攻击暴击时的方法，攻击值和终极技能充能点翻倍
        injury_value = randint(20, 30)
        target.hp -= injury_value
        print('英雄%s使用了普通攻击,并打出了暴击！！！对敌人造成%d点伤害！！！' % (self._name, injury_value))
        self.mana_resume()
        self.skill_point(2)

    def skill1(self, target):  # 技能“天雷咒”方法，消耗20法力值，造成40点伤害，终极技能充能2点
        if self._mana >= 20:  # 攻击成功
            self._mana -= 20
            target.hp -= 40
            print('英雄%s使用了技能“天雷咒”，对敌人造成40点伤害' % self._name)
            self.skill_point(2)
        else:  # 攻击失败
            print('英雄%s的法力值不足20点，无法使用该技能' % self._name)

    def hardhit_skill1(self, target):  # 技能“天雷咒”暴击时的方法，消耗20法力值，造成60点伤害，终极技能充能4点
        if self._mana >= 20:
            self._mana -= 20
            target.hp -= 60
            print('英雄%s使用了技能“天雷咒”，并打出了暴击！！！对敌人造成60点伤害！！！' % self._name)
            self.skill_point(4)
        else:
            print('英雄%s的法力值不足20点，无法使用该技能' % self._name)

    def skill2(self):  # 技能“禅定”的方法，消耗30法力值，恢复最大生命值的百分之三十，终极技能充能2点
        if self._mana >= 30:  # 技能使用成功
            self._mana -= 30
            if self._hp < self._first_hp:  # 生命值不充足
                if self._first_hp - self._hp >= self._first_hp * 0.3:  # 判断恢复生命值时是否溢出
                    cure_value = self._first_hp * 0.3
                    self._hp += self._first_hp * 0.3
                    print('英雄%s使用了技能“禅定”，恢复了%d点生命值' % (self._name, cure_value))
                    self.skill_point(2)
                else:
                    cure_value = self._first_hp - self._hp
                    self._hp = self._first_hp
                    print('英雄%s使用了技能“禅定”，恢复了%d点生命值' % (self._name, cure_value))
                    self.skill_point(2)
            else:  # 生命值充足
                cure_value = 0
                print('英雄%s使用了技能“禅定”，恢复了%d点生命值' % (self._name, cure_value))
                self.skill_point(2)
        else:  # 技能使用失败
            print('英雄%s的法力值不足30点，无法使用该技能' % self._name)

    def hard_skill2(self):  # 技能“禅定”触发“复苏之光”的方法，消耗30法力值，恢复最大生命值的百分之六十，终极技能充能4点
        if self._mana >= 30:
            self._mana -= 30
            if self._hp < self._first_hp:
                if self._first_hp - self._hp >= self._first_hp * 0.6:
                    cure_value = self._first_hp * 0.6
                    self._hp += self._first_hp * 0.6
                    print('英雄%s使用了技能“禅定”，并触发了复苏之光！！！恢复了%d点生命值！！！' % (self._name, cure_value))
                    self.skill_point(4)
                else:
                    cure_value = self._first_hp - self._hp
                    self._hp = self._first_hp
                    print('英雄%s使用了技能“禅定”，并触发了复苏之光！！！恢复了%d点生命值！！！' % (self._name, cure_value))
                    self.skill_point(4)
            else:
                cure_value = 0
                print('英雄%s使用了技能“禅定”，并触发了复苏之光！！！恢复了%d点生命值！！！' % (self._name, cure_value))
                self.skill_point(4)
        else:
            print('英雄%s的法力值不足30点，无法使用该技能' % self._name)

    def skill3(self, targets):  # 技能“次元斩”的方法，消耗50法力值，对敌方群体targets造成设定范围内的随机伤害
        if self._mana >= 50:  # 技能使用成功
            self._mana -= 50
            for aim in targets:  # 对每个敌人攻击
                if aim.alive:
                    aim.hp -= randint(15, 25)
            print('英雄%s使用了技能“次元斩”，对敌方群体中的每一个成员随机造成(15-25)点伤害' % self._name)
            self.skill_point((len(targets) // 2) + 1)  # 终极技能点增加算法
        else:  # 技能使用失败
            print('英雄%s的法力值不足50点，无法使用该技能' % self._name)

    def hardhit_skill3(self, targets):  # 技能“次元斩”暴击时的方法，消耗50法力值，对敌方群体targets造成设定范围内的随机双倍伤害
        if self._mana >= 50:
            self._mana -= 50
            for aim in targets:
                if aim.alive:
                    aim.hp -= (randint(15, 25) * 2)
            print('英雄%s使用了技能“次元斩”，并打出了暴击！！！对敌方群体中的每一个成员随机造成(15-25)*2点伤害！！！' % self._name)
            self.skill_point(((len(targets) // 2) + 1) * 2)  # 终极技能点增加算法
        else:
            print('英雄%s的法力值不足50点，无法使用该技能' % self._name)

    def skill4(self):  # 技能“偿命泉”的方法，消耗最大生命值的百分之十，恢复该消耗值五倍的法力值，终极技能充能2点
        if self._hp > self._first_hp * 0.1:  # 技能使用成功
            if (self._first_mana - self._mana) >= (self._first_hp * 0.1) * 5:  # 判断法力值恢复时算法溢出
                resume_point = (self._first_hp * 0.1) * 5
                self._mana += resume_point
                print('英雄%s使用了技能“偿命泉”，消耗自身生命值%d，恢复法力值%d点' % (self._name, (self._first_hp * 0.1), resume_point))
                self.skill_point(2)
            else:
                resume_point = self._first_mana - self._mana
                self._mana = self._first_mana
                print('英雄%s使用了技能“偿命泉”，消耗自身生命值%d，恢复法力值%d点' % (self._name, (self._first_hp * 0.1), resume_point))
                self.skill_point(2)
        else:  # 技能使用失败
            print('英雄%s当前生命值过低，无法使用该技能' % self._name)

    def hard_skill4(self):  # 技能“偿命泉”触发法术觉醒的方法，消耗最大生命值的百分之十，恢复该消耗值十倍的法力值，终极技能充能4点
        if self._hp > self._first_hp * 0.1:
            if (self._first_mana - self._mana) >= (self._first_hp * 0.1) * 10:
                resume_point = (self._first_hp * 0.1) * 10
                self._mana += resume_point
                print('英雄%s使用了技能“偿命泉”，并触发了法术觉醒！！！消耗自身生命值%d，恢复法力值%d点' % (self._name, (self._first_hp * 0.1), resume_point))
                self.skill_point(4)
            else:
                resume_point = self._first_mana - self._mana
                self._mana = self._first_mana
                print('英雄%s使用了技能“偿命泉”，并触发了法术觉醒！！！消耗自身生命值%d，恢复法力值%d点' % (self._name, (self._first_hp * 0.1), resume_point))
                self.skill_point(4)
        else:
            print('英雄%s当前生命值过低，无法使用该技能' % self._name)

    def ultimate_skill(self, target):  # 终极技能“诸神之怒”的方法，消耗80点法力值，造成100点伤害
        if self._mana >= 80 and self._cd == 10:  # 技能使用成功
            self._mana -= 80
            target.hp -= 100
            self._cd = 0  # 终极技能充能归零
            print('英雄%s使用了终极技能“诸神之怒”！对敌方造成100点伤害！' % self._name)
            sleep(1)
        else:  # 技能使用失败的三种情况
            if self._mana < 80 and self._cd == 10:
                print('英雄%s的法力值不足80点，无法使用终极技能' % self._name)
                sleep(1)
            elif self._mana >= 80 and self._cd < 10:
                print('英雄%s的终极技能尚未完成充能' % self._name)
                sleep(1)
            else:
                print('英雄%s的法力值不足80点，且终极技能尚未完成充能' % self._name)
                sleep(1)

    def hardhit_ultimate_skill(self, target):  # 终极技能“诸神之怒”暴击时的方法，消耗80点法力值，造成200点伤害
        if self._mana >= 80 and self._cd == 10:
            self._mana -= 80
            target.hp -= 200
            self._cd = 0
            print('英雄%s使用了终极技能“诸神之怒”,并打出了暴击！！！对敌方造成200点伤害！！！' % self._name)
            sleep(1)
        else:
            if self._mana < 80 and self._cd == 10:
                print('英雄%s的法力值不足80点，无法使用终极技能' % self._name)
                sleep(1)
            elif self._mana >= 80 and self._cd < 10:
                print('英雄%s的终极技能尚未完成充能' % self._name)
                sleep(1)
            else:
                print('英雄%s的法力值不足80点，且终极技能尚未完成充能' % self._name)
                sleep(1)

    def __str__(self):  # 默认打印值，显示当前角色各种状态信息
        return '***英雄：%s***\n' % self._name + '生命值：%d\n' % self._hp \
               + '法力值：%d\n' % self._mana + '终极技能充能完成：%d%%\n' % (self._cd * 10)


class Enemy(RPG):  # 定义敌人类，继承自角色基类
    __slots__ = ('_name', '_hp', '_cd', '_mana', '_first_mana')

    def __init__(self, name, hp, mana, cd):
        super().__init__(name, hp, cd)
        self._mana = mana
        self._first_mana = mana

    @property
    def mana(self):
        return self._mana

    def mana_resume(self):  # 法力值恢复方法
        if self._mana < self._first_mana:
            resume_point = randint(10, 20)
            if self._mana + resume_point <= self._first_mana:
                self._mana += resume_point
                print('%s恢复了%d点法力值' % (self._name, resume_point))
            else:
                self._mana = self._first_mana
                print('%s恢复了%d点法力值' % (self._name, (self._first_mana - self._mana)))
        else:
            print('%s法力值充足' % self._name)

    def skill_point(self, n):  # 终极技能充能计算方法
        if self._cd + n >= 10:
            self._cd = 10
            print('注意：' + '%s的终极技能准备就绪！！！' % self._name)
            sleep(1)
        else:
            self._cd += n
            print('%s的终极技能充能完成%d%%' % (self._name, (self._cd * 10)))
            sleep(1)

    def attack(self, target):  # 普通攻击的方法，在设定范围造成随机伤害，恢复法力值，终极技能充能2点
        injury_value = randint(10, 20)
        target.hp -= injury_value
        print('%s使用了普通攻击,对我方英雄造成%d点伤害' % (self._name, injury_value))
        self.mana_resume()
        self.skill_point(2)

    def hardhit_attack(self, target):  # 普通攻击暴击时的方法，在设定范围造成随机伤害，恢复法力值，终极技能充能3点
        injury_value = randint(20, 40)
        target.hp -= injury_value
        print('%s使用了普通攻击,并打出了暴击！！！对我方英雄造成%d点伤害！！！' % (self._name, injury_value))
        self.mana_resume()
        self.skill_point(3)

    def skill(self, target):  # 技能攻击的方法，在设定范围造成随机伤害，消耗20点法力值，终极技能充能3点
        if self._mana >= 20:
            injury_value = randint(30, 40)
            target.hp -= injury_value
            self._mana -= 20
            print('%s使用了技能攻击，对我方英雄造成了%d点伤害' % (self._name, injury_value))
            self.skill_point(3)
        else:
            self.attack(target)

    def hardhit_skill(self, target):  # 技能攻击暴击时的方法，在设定范围造成随机双倍伤害，消耗20点法力值，终极技能充能5点
        if self._mana >= 20:
            injury_value = randint(60, 80)
            target.hp -= injury_value
            self._mana -= 20
            print('%s使用了技能攻击，并打出了暴击！！！对我方英雄造成了%d点伤害！！！' % (self._name, injury_value))
            self.skill_point(5)
        else:
            self.hardhit_attack(target)

    def ultimate_skill(self, target):  # 终极技能的方法，造成60点伤害，消耗法力值50点，终极技能充能归零
        if self._mana >= 50 and self._cd == 10:
            self._mana -= 50
            target.hp -= 60
            self._cd = 0
            print('%s使用了终极技能！对我方英雄造成60点伤害！' % self._name)
            sleep(1)
        else:
            self.attack(target)

    def hardhit_ultimate_skill(self, target):  # 终极技能暴击时的方法，造成120点伤害，消耗法力值50点，终极技能充能归零
        if self._mana >= 50 and self._cd == 10:
            self._mana -= 50
            target.hp -= 120
            self._cd = 0
            print('%s使用了终极技能！并打出了暴击！！！对我方英雄造成120点伤害！！！' % self._name)
            sleep(1)
        else:
            self.hardhit_attack(target)

    def __str__(self):  # 默认打印值，显示当前角色各种状态信息
        return '***敌人：%s***\n' % self._name + '生命值：%d\n' % self._hp \
               + '法力值：%d\n' % self._mana + '终极技能充能完成：%d%%\n' % (self._cd * 10)


def enemy_survival_situation(enemys):  # 判断敌人群体是否有存活者的方法
    for enemy in enemys:
        if enemy.alive > 0:
            return True
    return False


def all_roles_information(hero, enemys):  # 打印英雄角色以及敌人角色的状态信息的方法
    print(hero)
    for enemy in enemys:
        print(enemy, end='\n')


def select_enemy_fight_this_round(enemys):  # 选取一个存活着的敌人进行本回合出战的方法
    enemys_len = len(enemys)
    while True:
        index = randrange(enemys_len)
        enemy = enemys[index]
        if enemy.alive > 0:
            return enemy


def main():  # 主函数，使用函数包装，以保证运行内存的节约，并保证各变量的正常使用
    hero = Hero('一禅', 300, 200, 0)  # 生成英雄实例（名字，生命值，法力值，终极技能充能点）
    enemy1 = Enemy('皮皮怪', 150, 80, 0)  # 生成敌人实例（名字，生命值，法力值，终极技能充能点）
    enemy2 = Enemy('溜了怪', 210, 100, 0)
    enemy3 = Enemy('猛男怪', 340, 180, 0)
    enemy4 = Enemy('锤锤怪', 100, 80, 0)
    enemys = [enemy1, enemy2, enemy3, enemy4]  # 敌人群体
    welcome = str('欢迎来到瓦罗兰大陆！@在这个游戏里，你将操纵英雄“%s”对抗魔族的进攻！@加油吧玩家，让我们一起为人类而战！' % hero.name)  # 欢迎词
    for i in welcome:  # 控制输出时间，显示欢迎词
        if i == '@':
            print()
            sleep(0.6)
        else:
            print(i, end='')
            sleep(0.05)
    sleep(1)
    print('\n')
    see_story = str(input('请选择是否观看背景故事介绍（yes/no）：'))  # 询问玩家是否观看背景故事
    if see_story == 'yes':  # 控制输出时间，显示背景故事
        story = str(
            '很久很久以前；在广阔的瓦罗兰大陆上；诞生了三个种族：人、魔、神；魔族与神族关系恶劣，常年爆发战争，双方实力不相上下；然而，由于神族的一时疏忽，神族被魔族一战击溃，强势的神族最终迎来了诸神的黄昏；而自此人族便常年生活于魔族的压迫之下；但是；不知道从什么时候开始；总有那么一位隐士；他时常在人族最危难的时刻出现，帮助人族抵抗魔族的进攻，使人族能够一直在这块土地上生存下去；人们亲切地称呼他为“%s”；但却从来都没有人知道他来自哪里；更没有人知道他是谁；大家只知道当他出现时，必将天雷大作，仿若诸神之怒......' % hero.name)
        for i in story:
            if i == '；':
                print()
                sleep(0.6)
            elif i == '.':
                print(i, end='')
                sleep(0.5)
            else:
                print(i, end='')
                sleep(0.1)
    print('\n')
    begin = str(input('准备好开始紧张刺激的游戏了吗？（yes/no）：'))  # 询问是否开始游戏
    print('\n')
    if begin == 'yes':
        fight_round = 1  # 回合计数器
        while hero.alive and enemy_survival_situation(enemys):  # 如果英雄存活并且敌人仍有存活，进入游戏循环
            if fight_round == 1:  # 第一回合时显示所有敌人信息
                print('###########################################')
                print('敌人状态如下')
                for i in enemys:
                    print('***敌人：%s***' % i.name,end='    ')
                print('\n')
                for i in enemys:
                    x=str(i.hp)
                    print('生命值：%d' % i.hp,end=' '*(13-len(x)))
                print('\n')
                for i in enemys:
                    x = str(i.mana)
                    print('法力值：%d' % i.mana,end=' '*(13-len(x)))
                print('\n')
                for i in enemys:
                    print('终极技能充能完成：%d%%' % (i.cd * 10),end='\t')
                print('\n###########################################')
                print('\n——————————————准备开始！！！————————————')
                djs = (3, 2, 1)  # 游戏开始倒计时3秒
                for i in djs:
                    print(i)
                    sleep(1)
            print('==================第%d回合==================' % fight_round)  # 显示回合分界栏
            e = select_enemy_fight_this_round(enemys)  # 调用函数选取一个敌人出战
            print('\n本回合敌方出战的是:%s\n' % e.name)
            print('该敌人的战斗状态为：')
            print(e)  # 显示本回合选中的敌人信息
            print('###%s技能一览表###\n' % e.name + \
                  '“普通攻击：对我方英雄造成10~20点伤害（攻击有50%几率触发暴击，暴击时伤害为20~40），同时随机恢复法力值10~20点，终极技能充能增加20%（暴击时为30%）”\n' + \
                  '“技能攻击：对我方英雄造成30~40点伤害（攻击有30%几率触发暴击，暴击时伤害为60~80），消耗法力值20点，终极技能充能增加30%（暴击时为50%）”\n' + \
                  '“终极技能攻击（一旦充能完成立即使用）：对我方英雄造成60点伤害（攻击有20%几率触发暴击，暴击时伤害为120），消耗法力值50点”\n')  # 显示敌人技能信息
            print('———————————————————————————————————————————\n')  # 分界栏
            print('我方英雄：%s的状态为：' % hero.name)
            print(hero)  # 显示我方英雄信息
            print('###英雄%s技能一览表###\n' % hero.name + \
                  '1:“普通攻击：对敌人造成10~15点伤害（攻击有30%几率触发暴击，暴击时伤害为20~30），同时随机恢复法力值10~20点，终极技能充能增加10%（暴击时为20%）”\n' + \
                  '2:“天雷咒：操纵天雷，对敌人造成40点伤害（攻击有30%几率触发暴击，暴击时伤害为60），消耗法力值20点，终极技能充能增加20%（暴击时为40%）”\n' + \
                  '3:“禅定：开始冥想，在本回合恢复自身最大生命值的30%（释放技能时有30%几率触发"复苏之光"，恢复自身最大生命值的60%），消耗法力值30点，终极技能充能增加20%（触发复苏之光时为40%）”\n' + \
                  '4:“次元斩：遁入次元，对敌方所有单位（包括未上场单位）随机造成(15~25)点伤害（攻击有30%几率触发暴击，暴击时每次攻击伤害翻倍），消耗法力值50点，终极技能充能随攻击次数增加（暴击时翻倍）”\n' + \
                  '5:“偿命泉：消耗自身最大生命值的10%，并以其五倍数目恢复自身法力值（释放技能时有30%几率触发"法术觉醒"，法力值恢复倍数翻倍），终极技能充能增加20%（触发法术觉醒时为40%）”\n' + \
                  '6:“诸神之怒（终极技能，充能完成时才能使用）：对敌方造成100点致命伤害（攻击有30%几率触发暴击，暴击时伤害为200），消耗法力值80点”\n')  # 显示我方英雄技能信息
            skill_choice = str(input('请选择你要使用的攻击的编号:'))  # 玩家输入想要选取的技能
            print('\n——————————————双方准备开战！！！————————————')
            djs = (3, 2, 1)  # 开战倒计时3秒
            for i in djs:
                print(i)
                sleep(1)
            print('\n———————————————战斗开始！！！———————————————\n')
            sleep(1)
            hardhit_rate = randint(1, 100)
            if hardhit_rate < 71:  # 百分之七十的几率打出普通状态下的各种攻击模式
                if skill_choice == '1':
                    hero.attack(e)
                elif skill_choice == '2':
                    hero.skill1(e)
                elif skill_choice == '3':
                    hero.skill2()
                elif skill_choice == '4':
                    hero.skill3(enemys)
                elif skill_choice == '5':
                    hero.skill4()
                elif skill_choice == '6':
                    hero.ultimate_skill(e)
            else:  # 百分之三十的几率打出暴击状态下的各种攻击模式
                if skill_choice == '1':
                    hero.hardhit_attack(e)
                elif skill_choice == '2':
                    hero.hardhit_skill1(e)
                elif skill_choice == '3':
                    hero.hard_skill2()
                elif skill_choice == '4':
                    hero.hardhit_skill3(enemys)
                elif skill_choice == '5':
                    hero.hard_skill4()
                elif skill_choice == '6':
                    hero.hardhit_ultimate_skill(e)
            if e.alive > 0:  # 如果该敌人在接受攻击之后仍然存活，那么进行还击
                enemy_skill_choice = randint(1, 100)
                if enemy_skill_choice < 61 and e.cd != 10:  # 当终极技能没有充能完成时，有百分之六十几率打出普通攻击模式
                    enemy_hardhit_rate = randint(1, 10)
                    if enemy_hardhit_rate < 6:
                        e.attack(hero)
                    else:  # 在普通攻击状态下有百分之五十几率打出暴击
                        e.hardhit_attack(hero)
                elif 101 > enemy_skill_choice >= 61 and e.cd != 10:  # 当终极技能没有充能完成时，有百分之四十几率打出技能攻击模式
                    enemy_hardhit_rate = randint(1, 10)
                    if enemy_hardhit_rate < 7:
                        e.skill(hero)
                    else:  # 在技能攻击状态下有百分之三十几率打出暴击
                        e.hardhit_skill(hero)
                elif e.cd == 10:  # 当终极技能充能完成时，跳过其他攻击模式，立即释放
                    enemy_hardhit_rate = randint(1, 10)
                    if enemy_hardhit_rate < 8:
                        e.ultimate_skill(hero)
                    else:  # 在终极技能攻击状态下有百分之二十几率打出暴击
                        e.hardhit_ultimate_skill(hero)
            print('\n———————————————战斗结束！！！———————————————\n')  # 战斗结束分界栏
            print('正在整理本回合战后数据......')
            for i in djs:  # 倒计时三秒
                print(i)
                sleep(1)
            fight_round += 1  # 回合计数加一
            print('\n战斗结束之后所有角色的状态：\n')
            all_roles_information(hero, enemys)  # 显示本回合战斗结束后所有角色的信息
            print('=================回合结束==================')  # 回合结束分界栏
            print('(上翻可查看本回合双方战后数据)\n')
            if hero.alive and enemy_survival_situation(enemys):  # 如果英雄未死，且敌人仍然有存活，继续开启下一回合
                print('战斗尚未结束，即将开启下一回合......')
                for i in djs:  # 倒计时三秒
                    print(i)
                    sleep(1)
        print('\n===============战斗结束！！！===============\n')  # 战斗结束分界栏
        if hero.alive > 0:  # 如果英雄存活，显示英雄获胜
            print('英雄：%s获胜！！！' % hero.name)
        else:  # 显示敌人获胜
            print('很遗憾，英雄%s被敌人打败了......' % hero.name)
        print('一共战斗了：%d个回合' % fight_round)
    print('\n===============游戏结束！！！===============\n')  # 游戏结束分界栏


if __name__ == '__main__':
    main()
