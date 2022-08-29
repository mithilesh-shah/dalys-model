#Estimating age wise population

import pandas as pd
import numpy as np

#Calculate new borns
fert = pd.read_excel("fertility.xlsx")
t_pop = pd.read_excel("total_pop.xlsx")
mort = pd.read_excel("mortality.xlsx")
gbd = pd.read_excel("gbd.xlsx")

#print(fert.columns)

def cal_new_born():
    fert_age_grps = list(fert)
    births = []
    age_checks = np.arange(15,50,1)
    for age_grp in fert_age_grps:
        for age in age_checks:
            if (age_grp == age):
                new_born = fert[age_grp][0]*(t_pop[age_grp][0]/2)
                births.append(new_born)
    return (sum(births))

t_pop.at[1,0] = cal_new_born()
#print(t_pop)

#print(mort['<1 year'][0])
single_age = np.arange(0,101,1)
#print(single_age)
def cal_pop():
    for age in single_age:
        deaths = 0
        if age == 0:
            deaths = ((t_pop[age][0]*(mort['<1 year'][0])))/100000
            t_pop.at[1,age+1] = (t_pop[age][0]) - deaths
        if (age>=1 and age<5):
            print(age)
            deaths = ((t_pop[age][0]*(mort['1-4 years'][0])))/100000
            t_pop.at[1,age+1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age>=5 and age<10):
            print(age)
            deaths = ((t_pop[age][0]*(mort['5-9 years'][0])))/100000
            t_pop.at[1,age+1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 10 and age < 15):
            print(age)
            deaths = ((t_pop[age][0] * (mort['10-14 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 15 and age < 20):
            print(age)
            deaths = ((t_pop[age][0] * (mort['15-19 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 20 and age < 25):
            print(age)
            deaths = ((t_pop[age][0] * (mort['20-24 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 25 and age < 30):
            print(age)
            deaths = ((t_pop[age][0] * (mort['25-29 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 30 and age < 35):
            print(age)
            deaths = ((t_pop[age][0] * (mort['30-34 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 35 and age < 40):
            print(age)
            deaths = ((t_pop[age][0] * (mort['35-39 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 40 and age < 45):
            print(age)
            deaths = ((t_pop[age][0] * (mort['40-44 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 45 and age < 50):
            print(age)
            deaths = ((t_pop[age][0] * (mort['45-49 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 50 and age < 55):
            print(age)
            deaths = ((t_pop[age][0] * (mort['50-54 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 55 and age < 60):
            print(age)
            deaths = ((t_pop[age][0] * (mort['55-59 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 60 and age < 65):
            print(age)
            deaths = ((t_pop[age][0] * (mort['60-64 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 65 and age < 70):
            print(age)
            deaths = ((t_pop[age][0] * (mort['65-69 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 70 and age < 75):
            print(age)
            deaths = ((t_pop[age][0] * (mort['70-74 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 75 and age < 80):
            print(age)
            deaths = ((t_pop[age][0] * (mort['75-79 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 80 and age < 85):
            print(age)
            deaths = ((t_pop[age][0] * (mort['80-84 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 85 and age < 90):
            print(age)
            deaths = ((t_pop[age][0] * (mort['85-89 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 90 and age < 95):
            print(age)
            deaths = ((t_pop[age][0] * (mort['90-94 years'][0]))) / 100000
            t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
            print(t_pop[age][1])
        if (age >= 95 and age<99):
                print(age)
                deaths = ((t_pop[age][0] * (mort['95+ years'][0]))) / 100000
                t_pop.at[1, age + 1] = ((t_pop[age][0]) - deaths)
                print(t_pop[age][1])
        if (age == 99):
                print(age)
                deaths = ((t_pop[age][0] * (mort['95+ years'][0]))) / 100000 + ((t_pop[age+1][0] * (mort['95+ years'][0]))) / 100000
                t_pop.at[1, age + 1] = ((t_pop[age][0]+t_pop[age+1][0]) - deaths)
                print(t_pop[age][1])

def slow_aging():
    t_pop_slow = t_pop.copy()
    for age in single_age:
        deaths = 0
        if (age >= 30 and age < 35):
            print(age)
            deaths = ((t_pop_slow[age][0] * (mort['25-29 years'][0]))) / 100000
            t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
            print(t_pop_slow[age][1])
        if (age >= 35 and age < 40):
            print(age)
            deaths = ((t_pop_slow[age][0] * (mort['30-34 years'][0]))) / 100000
            t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
            print(t_pop_slow[age][1])
        if (age >= 40 and age < 45):
            print(age)
            deaths = ((t_pop_slow[age][0] * (mort['35-39 years'][0]))) / 100000
            t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
            print(t_pop_slow[age][1])
        if (age >= 45 and age < 50):
            print(age)
            deaths = ((t_pop_slow[age][0] * (mort['40-44 years'][0]))) / 100000
            t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
            print(t_pop_slow[age][1])
        if (age >= 50 and age < 55):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['45-49 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 55 and age < 60):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['50-54 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 60 and age < 65):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['55-59 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 65 and age < 70):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['60-64 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 70 and age < 75):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['65-69 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 75 and age < 80):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['70-74 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 80 and age < 85):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['75-79 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 85 and age < 90):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['80-84 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 90 and age < 95):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['85-89 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age >= 95 and age < 99):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['90-94 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]) - deaths)
                print(t_pop_slow[age][1])
        if (age == 99):
                print(age)
                deaths = ((t_pop_slow[age][0] * (mort['90-94 years'][0]))) / 100000 + ((t_pop_slow[age+1][0] * (mort['90-94 years'][0]))) / 100000
                t_pop_slow.at[1, age + 1] = ((t_pop_slow[age][0]+t_pop[age+1][0]) - deaths)
                print(t_pop[age][1])
    return t_pop_slow

def age_grp_pop():
    pop = []
    total_sum = 0
    for age in single_age:
        temp_pop = {}
        if (age == 0):
            temp_pop.update({'<1year' : t_pop[age][0]})
            pop.append(temp_pop)
        if (age >=1 and age <5):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==4):
                temp_pop.update({'1-4 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=5 and age <10):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==9):
                temp_pop.update({'5-9 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=10 and age <15):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==14):
                temp_pop.update({'10-14 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=15 and age <20):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==19):
                temp_pop.update({'15-19 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=20 and age <25):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==24):
                temp_pop.update({'20-24 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=25 and age <30):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==29):
                temp_pop.update({'25-29 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=30 and age <35):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==34):
                temp_pop.update({'30-34 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=35 and age <40):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==39):
                temp_pop.update({'35-39 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=40 and age <45):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==44):
                temp_pop.update({'40-44 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=45 and age <50):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==49):
                temp_pop.update({'45-49 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=50 and age <55):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==54):
                temp_pop.update({'50-54 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=55 and age <60):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==59):
                temp_pop.update({'55-59 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=60 and age <65):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==64):
                temp_pop.update({'60-64 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=65 and age <70):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==69):
                temp_pop.update({'65-69 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=70 and age <75):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==74):
                temp_pop.update({'70-74 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=75 and age <80):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==79):
                temp_pop.update({'75-79 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=80 and age <85):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==84):
                temp_pop.update({'80-84 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=85 and age <90):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==89):
                temp_pop.update({'85-89 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=90 and age <95):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==94):
                temp_pop.update({'90-94 years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
        if (age >=95 and age <101):
            temp_sum = t_pop[age][0]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==100):
                temp_pop.update({'95+ years': total_sum})
                pop.append(temp_pop)
                total_sum = 0
    return pop


def cal_yld():
    pop_yld = t_pop.copy()
    gbd_yld = gbd[gbd['measure_name']=='YLDs (Years Lived with Disability)']
    gbd_yld = gbd_yld.reset_index()
    gbd_yld['yld_value'] = 0
    for age in single_age:
        yld = 0
        temp = 0
        for cause in gbd_yld['cause_name'].unique():
            if (age == 0):
                temp = (gbd_yld.loc[(gbd_yld['cause_name'] == cause) & (gbd_yld['age_name'] == '<1 year')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[0]['<1year']
                gbd_yld.loc[(gbd_yld['cause_name'] == cause) & (gbd_yld['age_name'] == '<1 year'), 'yld_value'] = yld
            if (age>=1 and age<5):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='1-4 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[1]['1-4 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='1-4 years'),'yld_value'] = yld/100000
            if (age>=5 and age<10):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='5-9 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[2]['5-9 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='5-9 years'),'yld_value'] = yld/100000
            if (age>=10 and age<15):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='10-14 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[3]['10-14 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='10-14 years'),'yld_value'] = yld/100000
            if (age>=15 and age<20):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='15-19 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[4]['15-19 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='15-19 years'),'yld_value'] = yld/100000
            if (age>=20 and age<25):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='20-24 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[5]['20-24 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='20-24 years'),'yld_value'] = yld/100000
            if (age>=25 and age<30):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='25-29 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[6]['25-29 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='25-29 years'),'yld_value'] = yld/100000
            if (age>=30 and age<35):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='30-34 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[7]['30-34 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='30-34 years'),'yld_value'] = yld/100000
            if (age>=35 and age<40):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='35-39 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[8]['35-39 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='35-39 years'),'yld_value'] = yld/100000
            if (age>=40 and age<45):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='40-44 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[9]['40-44 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='40-44 years'),'yld_value'] = yld/100000
            if (age>=45 and age<50):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='45-49 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[10]['45-49 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='45-49 years'),'yld_value'] = yld/100000
            if (age>=50 and age<55):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='50-54 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[11]['50-54 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='50-54 years'),'yld_value'] = yld/100000
            if (age>=55 and age<60):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='55-59 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[12]['55-59 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='55-59 years'),'yld_value'] = yld/100000
            if (age>=60 and age<65):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='60-64 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[13]['60-64 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='60-64 years'),'yld_value'] = yld/100000
            if (age>=65 and age<70):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='65-69 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[14]['65-69 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='65-69 years'),'yld_value'] = yld/100000
            if (age>=70 and age<75):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='70-74 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[15]['70-74 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='70-74 years'),'yld_value'] = yld/100000
            if (age>=75 and age<80):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='75-79 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[16]['75-79 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='75-79 years'),'yld_value'] = yld/100000
            if (age>=80 and age<85):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='80-84 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[17]['80-84 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='80-84 years'),'yld_value'] = yld/100000
            if (age>=85 and age<90):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='85-89 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[18]['85-89 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='85-89 years'),'yld_value'] = yld/100000
            if (age>=90 and age<95):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='90-94 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[19]['90-94 years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='90-94 years'),'yld_value'] = yld/100000
            if (age>=95 and age<101):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='95+ years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[20]['95+ years']
                gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='95+ years'),'yld_value'] = yld/100000
    print(gbd_yld.tail(5))

pop = age_grp_pop()
print(pop)
cal_pop()

#t_pop_slow = slow_aging()
#print(t_pop.head(5))
#print(t_pop_slow.head(5))
cal_yld()
