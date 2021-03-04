from abc import ABCMeta, abstractmethod
from random import randint, randrange
from time import sleep


class RPG(object, metaclass=ABCMeta):
    __slots__ = ('_name', '_hp', '_cd', '_first_hp')

    def __init__(self, name, hp, cd):
        self._name = name
        self._hp = hp
        self._cd = cd
        self._first_hp = hp

    @property
    def name(self):
        return self._name

    @property
    def hp(self):
        return self._hp

    @property
    def cd(self):
        return self._cd

    @hp.setter
    def hp(self, hp):
        self._hp = hp if hp >= 0 else 0

    @cd.setter
    def cd(self, cd):
        self._cd = cd if cd >= 0 else 0

    @property
    def alive(self):
        return self._hp > 0

    @abstractmethod
    def attack(self, target):
        pass


class Hero(RPG):
    __slots__ = ('_name', '_hp', '_cd', '_mana', '_first_hp', '_first_mana')

    def __init__(self, name, hp, mana, cd):
        super().__init__(name, hp, cd)
        self._mana = mana
        self._first_hp = hp
        self._first_mana = mana

    def mana_resume(self):
        if self._mana < self._first_mana:
            resume_point = randint(10, 20)
            if self._mana + resume_point <= self._first_mana:
                self._mana += resume_point
                print('恢复了%d点法力值' % resume_point)
            else:
                self._mana = self._first_mana
                print('恢复了%d点法力值' % (self._first_mana - self._mana))
        else:
            print('法力值充足')

    def skill_point(self, n):
        if self._cd + n >= 10:
            self._cd = 10
            print('终极技能准备就绪！！！\n')
            sleep(1)
        else:
            self._cd += n
            print('终极技能充能完成%d%%\n' % (self._cd * 10))
            sleep(1)

    def attack(self, target):
        injury_value = randint(10, 15)
        target.hp -= injury_value
        print('使用普通攻击成功，对敌人造成%d点伤害' % injury_value)
        self.mana_resume()
        self.skill_point(1)

    def hardhit_attack(self, target):
        injury_value = randint(20, 30)
        target.hp -= injury_value
        print('使用普通攻击成功,并打出了暴击！！！对敌人造成%d点伤害！！！' % injury_value)
        self.mana_resume()
        self.skill_point(2)

    def skill1(self, target):
        if self._mana >= 20:
            self._mana -= 20
            target.hp -= 40
            print('使用技能1成功，对敌人造成40点伤害')
            self.skill_point(2)
        else:
            print('英雄法力值不足20点，无法使用该技能')

    def hardhit_skill1(self, target):
        if self._mana >= 20:
            self._mana -= 20
            target.hp -= 60
            print('使用技能1成功，并打出了暴击！！！对敌人造成60点伤害！！！')
            self.skill_point(4)
        else:
            print('英雄法力值不足20点，无法使用该技能')

    def skill2(self):
        if self._mana >= 30:
            self._mana -= 30
            if self._hp < self._first_hp:
                if self._first_hp - self._hp >= self._first_hp * 0.3:
                    cure_value = self._first_hp * 0.3
                    self._hp += self._first_hp * 0.3
                    print('使用技能2成功，英雄%s恢复了%d点生命值' % (self._name, cure_value))
                    self.skill_point(2)
                else:
                    cure_value = self._first_hp - self._hp
                    self._hp = self._first_hp
                    print('使用技能2成功，英雄%s恢复了%d点生命值' % (self._name, cure_value))
                    self.skill_point(2)
            else:
                cure_value = 0
                print('使用技能2成功，英雄%s恢复了%d点生命值' % (self._name, cure_value))
                self.skill_point(2)
        else:
            print('英雄法力值不足30点，无法使用该技能')

    def hard_skill2(self):
        if self._mana >= 30:
            self._mana -= 30
            if self._hp < self._first_hp:
                if self._first_hp - self._hp >= self._first_hp * 0.6:
                    cure_value = self._first_hp * 0.6
                    self._hp += self._first_hp * 0.6
                    print('使用技能2成功，并触发了复苏之光！！！英雄%s恢复了%d点生命值！！！' % (self._name, cure_value))
                    self.skill_point(4)
                else:
                    cure_value = self._first_hp - self._hp
                    self._hp = self._first_hp
                    print('使用技能2成功，并触发了复苏之光！！！英雄%s恢复了%d点生命值！！！' % (self._name, cure_value))
                    self.skill_point(4)
            else:
                cure_value = 0
                print('使用技能2成功，并触发了复苏之光！！！英雄%s恢复了%d点生命值！！！' % (self._name, cure_value))
                self.skill_point(4)
        else:
            print('英雄法力值不足30点，无法使用该技能')

    def skill3(self, targets):
        if self._mana >= 50:
            self._mana -= 50
            for aim in targets:
                if aim.alive:
                    aim.hp -= randint(15, 25)
            print('使用技能3成功，对敌方群体中的每一个成员随机造成(15-25)点伤害')
            self.skill_point((len(targets) // 2) + 1)
        else:
            print('英雄法力值不足50点，无法使用该技能')

    def hardhit_skill3(self, targets):
        if self._mana >= 50:
            self._mana -= 50
            for aim in targets:
                if aim.alive:
                    aim.hp -= (randint(15, 25) * 2)
            print('使用技能3成功，并打出了暴击！！！对敌方群体中的每一个成员随机造成(15-25)*2点伤害！！！')
            self.skill_point(((len(targets) // 2) + 1) * 2)
        else:
            print('英雄法力值不足50点，无法使用该技能')

    def skill4(self):
        if self._hp > self._first_hp * 0.1:
            if (self._first_mana - self._mana) >= (self._first_hp * 0.1) * 5:
                resume_point = (self._first_hp * 0.1) * 5
                self._mana += resume_point
                print('使用技能4成功，消耗自身生命值%d，恢复法力值%d点' % ((self._first_hp * 0.1), resume_point))
                self.skill_point(2)
            else:
                resume_point = self._first_mana - self._mana
                self._mana = self._first_mana
                print('使用技能4成功，消耗自身生命值%d，恢复法力值%d点' % ((self._first_hp * 0.1), resume_point))
                self.skill_point(2)
        else:
            print('英雄当前生命值过低，无法使用该技能')

    def hard_skill4(self):
        if self._hp > self._first_hp * 0.1:
            if (self._first_mana - self._mana) >= (self._first_hp * 0.1) * 10:
                resume_point = (self._first_hp * 0.1) * 10
                self._mana += resume_point
                print('使用技能4成功，并触发了法术觉醒！！！\
                消耗自身生命值%d，恢复法力值%d点' % ((self._first_hp * 0.1), resume_point))
                self.skill_point(4)
            else:
                resume_point = self._first_mana - self._mana
                self._mana = self._first_mana
                print('使用技能4成功，并触发了法术觉醒！！！\
                消耗自身生命值%d，恢复法力值%d点' % ((self._first_hp * 0.1), resume_point))
                self.skill_point(4)
        else:
            print('英雄当前生命值过低，无法使用该技能')

    def ultimate_skill(self, target):
        if self._mana >= 80 and self._cd == 10:
            self._mana -= 80
            target.hp -= 100
            self._cd = 0
            print('终极技能使用成功！对敌方造成100点伤害！')
            sleep(1)
        else:
            if self._mana < 80 and self._cd == 10:
                print('英雄法力值不足80点，无法使用终极技能')
                sleep(1)
            elif self._mana >= 80 and self._cd < 10:
                print('英雄终极技能尚未完成充能')
                sleep(1)
            else:
                print('英雄法力值不足80点，且终极技能尚未完成充能')
                sleep(1)

    def hardhit_ultimate_skill(self, target):
        if self._mana >= 80 and self._cd == 10:
            self._mana -= 80
            target.hp -= 200
            self._cd = 0
            print('终极技能使用成功,并打出了暴击！！！对敌方造成200点伤害！！！')
            sleep(1)
        else:
            if self._mana < 80 and self._cd == 10:
                print('英雄法力值不足80点，无法使用终极技能')
                sleep(1)
            elif self._mana >= 80 and self._cd < 10:
                print('英雄终极技能尚未完成充能')
                sleep(1)
            else:
                print('英雄法力值不足80点，且终极技能尚未完成充能')
                sleep(1)

    def __str__(self):
        return '***英雄：%s***\n' % self._name + '生命值：%d\n' % self._hp \
               + '法力值：%d\n' % self._mana + '终极技能充能完成：%d%%\n' % (self._cd * 10)


class Enemy(RPG):
    __slots__ = ('_name', '_hp', '_cd', '_mana', '_first_mana')

    def __init__(self, name, hp, mana, cd):
        super().__init__(name, hp, cd)
        self._mana = mana
        self._first_mana = mana

    def mana_resume(self):
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

    def skill_point(self, n):
        if self._cd + n >= 10:
            self._cd = 10
            print('注意：' + '%s的终极技能准备就绪！！！' % self._name)
            sleep(1)
        else:
            self._cd += n
            print('%s的终极技能充能完成%d%%' % (self._name, (self._cd * 10)))
            sleep(1)

    def attack(self, target):
        injury_value = randint(10, 20)
        target.hp -= injury_value
        print('%s使用了普通攻击,对我方英雄造成%d点伤害' % (self._name, injury_value))
        self.mana_resume()
        self.skill_point(2)

    def hardhit_attack(self, target):
        injury_value = randint(20, 40)
        target.hp -= injury_value
        print('%s使用了普通攻击,并打出了暴击！！！对我方英雄造成%d点伤害！！！' % (self._name, injury_value))
        self.mana_resume()
        self.skill_point(3)

    def skill(self, target):
        if self._mana >= 20:
            injury_value = randint(30, 40)
            target.hp -= injury_value
            self._mana -= 20
            print('%s使用了技能攻击，对我方英雄造成了%d点伤害' % (self._name, injury_value))
            self.skill_point(3)
        else:
            self.attack(target)

    def hardhit_skill(self, target):
        if self._mana >= 20:
            injury_value = randint(60, 80)
            target.hp -= injury_value
            self._mana -= 20
            print('%s使用了技能攻击，并打出了暴击！！！对我方英雄造成了%d点伤害！！！' % (self._name, injury_value))
            self.skill_point(5)
        else:
            self.hardhit_attack(target)

    def ultimate_skill(self, target):
        if self._mana >= 50 and self._cd == 10:
            self._mana -= 50
            target.hp -= 60
            self._cd = 0
            print('%s使用了终极技能！对我方英雄造成60点伤害！' % self._name)
            sleep(1)
        else:
            self.attack(target)

    def hardhit_ultimate_skill(self, target):
        if self._mana >= 50 and self._cd == 10:
            self._mana -= 50
            target.hp -= 120
            self._cd = 0
            print('%s使用了终极技能！并打出了暴击！！！对我方英雄造成120点伤害！！！' % self._name)
            sleep(1)
        else:
            self.hardhit_attack(target)

    def __str__(self):
        return '***敌人：%s***\n' % self._name + '生命值：%d\n' % self._hp \
               + '法力值：%d\n' % self._mana + '终极技能充能完成：%d%%\n' % (self._cd * 10)


def enemy_survival_situation(enemys):
    for enemy in enemys:
        if enemy.alive > 0:
            return True
    return False


def all_roles_information(hero, enemys):
    print(hero)
    for enemy in enemys:
        print(enemy, end='\n')


def select_enemy_fight_this_round(enemys):
    enemys_len = len(enemys)
    while True:
        index = randrange(enemys_len)
        enemy = enemys[index]
        if enemy.alive > 0:
            return enemy


def main():
    hero = Hero('一禅', 300, 200, 0)
    enemy1 = Enemy('皮皮怪', 150, 80, 0)
    enemy2 = Enemy('看看怪', 210, 100, 0)
    enemy3 = Enemy('但是怪', 340, 180, 0)
    enemy4 = Enemy('蛋白怪', 100, 80, 0)
    enemys = [enemy1, enemy2, enemy3, enemy4]
    fight_round = 1
    while hero.alive and enemy_survival_situation(enemys):
        print('==================第%d回合==================' % fight_round)
        e = select_enemy_fight_this_round(enemys)
        print('本回合敌方出战的是:%s\n' % e.name)

        print('该敌人的战斗状态为：\n')

        print(e)

        print('我方英雄：%s的状态为\n' % hero.name)

        print(hero)

        print('###技能表###\n' + \
              'attack:“普通攻击：对敌人造成10~15点伤害（攻击有30%几率触发暴击，暴击时伤害为20~30），同时随机恢复法力值10~20点，终极技能充能增加10%（暴击时为20%）”\n' + \
              'skill1:“天雷咒：操纵天雷，对敌人造成40点伤害（攻击有30%几率触发暴击，暴击时伤害为60），消耗法力值20点，终极技能充能增加20%（暴击时为40%）”\n' + \
              'skill2:“禅定：开始冥想，在本回合恢复自身最大生命值的30%（释放技能时有30%几率触发"复苏之光"，恢复自身最大生命值的60%），消耗法力值30点，终极技能充能增加20%（触发复苏之光时为40%）”\n' + \
              'skill3:“次元斩：遁入次元，对敌方所有单位（包括未上场单位）随机造成(15~25)点伤害（攻击有30%几率触发暴击，暴击时每次攻击伤害翻倍），消耗法力值50点，终极技能充能随攻击次数增加（暴击时翻倍）”\n' + \
              'skill4:“偿命泉：消耗自身最大生命值的10%，并以其五倍数目恢复自身法力值（释放技能时有30%几率触发"法术觉醒"，法力值恢复倍数翻倍），终极技能充能增加20%（触发法术觉醒时为40%）”\n' + \
              'ultimate:“诸神之怒（终极技能，充能完成时才能使用）：对敌方造成100点致命伤害（攻击有30%几率触发暴击，暴击时伤害为200），消耗法力值80点”\n')

        skill_choice = str(input('请选择你要使用的攻击方式:'))
        print('\n——————————————双方准备开战！！！————————————')
        djs = (3, 2, 1)
        for i in djs:
            print(i)
            sleep(1)
        print('\n———————————————战斗开始！！！———————————————\n')
        sleep(1)
        hardhit_rate = randint(1, 100)
        if hardhit_rate < 71:
            if skill_choice == 'attack':
                hero.attack(e)

            elif skill_choice == 'skill1':
                hero.skill1(e)

            elif skill_choice == 'skill2':
                hero.skill2()

            elif skill_choice == 'skill3':
                hero.skill3(enemys)

            elif skill_choice == 'skill4':
                hero.skill4()

            elif skill_choice == 'ultimate':
                hero.ultimate_skill(e)

        else:
            if skill_choice == 'attack':
                hero.hardhit_attack(e)

            elif skill_choice == 'skill1':
                hero.hardhit_skill1(e)

            elif skill_choice == 'skill2':
                hero.hard_skill2()

            elif skill_choice == 'skill3':
                hero.hardhit_skill3(enemys)

            elif skill_choice == 'skill4':
                hero.hard_skill4()

            elif skill_choice == 'ultimate':
                hero.hardhit_ultimate_skill(e)

        if e.alive > 0:
            enemy_skill_choice = randint(1, 100)
            if enemy_skill_choice < 61 and e.cd != 10:
                enemy_hardhit_rate = randint(1, 10)
                if enemy_hardhit_rate < 6:
                    e.attack(hero)

                else:
                    e.hardhit_attack(hero)

            elif 101 > enemy_skill_choice >= 61 and e.cd != 10:
                enemy_hardhit_rate = randint(1, 10)
                if enemy_hardhit_rate < 7:
                    e.skill(hero)

                else:
                    e.hardhit_skill(hero)
            elif e.cd == 10:
                enemy_hardhit_rate = randint(1, 10)
                if enemy_hardhit_rate < 8:
                    e.ultimate_skill(hero)
                else:
                    e.hardhit_ultimate_skill(hero)
        print('\n———————————————战斗结束！！！———————————————\n')
        print('正在整理战后数据......')
        for i in djs:
            print(i)
            sleep(1)
        fight_round += 1
        print('\n战斗结束之后所有角色的状态：\n')
        all_roles_information(hero, enemys)
        print('=================回合结束==================')
        print('(上翻可查看本回合双方战后数据)\n')
        if hero.alive and enemy_survival_situation(enemys):
            print('战斗尚未结束，即将开启下一回合......')
            for i in djs:
                print(i)
                sleep(1)
    print('\n===============战斗结束！！！===============\n')
    if hero.alive > 0:
        print('英雄：%s获胜！！！' % hero.name)
    else:
        print('很遗憾，你的英雄被敌人打败了......')
    print('一共战斗了：%d个回合' % fight_round)


if __name__ == '__main__':
    main()
