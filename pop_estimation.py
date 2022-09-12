#Estimating age wise population

import pandas as pd
import numpy as np

#Calculate new borns
fert = pd.read_excel("fertility.xlsx")
t_pop = pd.read_excel("total_pop.xlsx")
mort = pd.read_excel("mortality.xlsx")
gbd = pd.read_excel("gbd.xlsx")
temp_pop = {}
disease_rate = {}
total_sum = 0
#print(fert.columns)
t_pop.set_index('Year', inplace=True)
fert.set_index('Year', inplace=True)
pop_age = list(t_pop)
print(pop_age)
fert_age_grps = list(fert)

def cal_new_born(year):
    births = []
    age_checks = np.arange(15,50,1)
    for age_grp in fert_age_grps:
        for age in age_checks:
            if (age_grp == age):
                new_born = (fert[age_grp][year]/1000)*(t_pop[age_grp][year]/2)
                births.append(new_born)
    return (sum(births))

#t_pop.at[1,0] = cal_new_born()
#print(t_pop)

#print(mort['<1 year'][0])
single_age = np.arange(0,101,1)
#print(single_age)
pop=[]
def total_pop():
    for year, pop_row in t_pop.iterrows():
        if (year >= 2021 and year < 2100):
            for age in pop_age :
                t_population = cal_pop(year,age)
                print(t_population.head(10))
                ts_population = slow_aging(year,age)
                print(ts_population.head(10))
                pop = age_grp_pop(year,age)
                age_grp_population = pd.DataFrame(pop)
    t_population.to_excel("pop_output.xlsx")
    ts_population.to_excel("pop_slow_output.xlsx")
    age_grp_population.to_excel("age_group_output.xlsx")
    yld_population.to_excel("yld_rate_population.xlsx")
    return  age_grp_population

def cal_pop(year, age):
        t_pop.at[year+1, 0] = cal_new_born(year)
        deaths = 0
        if age == 0:
            deaths = ((t_pop[age][year]*(mort['<1 year'][0])))/100000
            t_pop.at[year+1,age+1] = (t_pop[age][year]) - deaths
        if (age>=1 and age<5):
            print(age)
            deaths = ((t_pop[age][year]*(mort['1-4 years'][0])))/100000
            t_pop.at[year+1,age+1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age>=5 and age<10):
            print(age)
            deaths = ((t_pop[age][year]*(mort['5-9 years'][0])))/100000
            t_pop.at[year+1,age+1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 10 and age < 15):
            print(age)
            deaths = ((t_pop[age][year] * (mort['10-14 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 15 and age < 20):
            print(age)
            deaths = ((t_pop[age][year] * (mort['15-19 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 20 and age < 25):
            print(age)
            deaths = ((t_pop[age][year] * (mort['20-24 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 25 and age < 30):
            print(age)
            deaths = ((t_pop[age][year] * (mort['25-29 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 30 and age < 35):
            print(age)
            deaths = ((t_pop[age][year] * (mort['30-34 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 35 and age < 40):
            print(age)
            deaths = ((t_pop[age][year] * (mort['35-39 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 40 and age < 45):
            print(age)
            deaths = ((t_pop[age][year] * (mort['40-44 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 45 and age < 50):
            print(age)
            deaths = ((t_pop[age][year] * (mort['45-49 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 50 and age < 55):
            print(age)
            deaths = ((t_pop[age][year] * (mort['50-54 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 55 and age < 60):
            print(age)
            deaths = ((t_pop[age][year] * (mort['55-59 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 60 and age < 65):
            print(age)
            deaths = ((t_pop[age][year] * (mort['60-64 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 65 and age < 70):
            print(age)
            deaths = ((t_pop[age][year] * (mort['65-69 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 70 and age < 75):
            print(age)
            deaths = ((t_pop[age][year] * (mort['70-74 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 75 and age < 80):
            print(age)
            deaths = ((t_pop[age][year] * (mort['75-79 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 80 and age < 85):
            print(age)
            deaths = ((t_pop[age][year] * (mort['80-84 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 85 and age < 90):
            print(age)
            deaths = ((t_pop[age][year] * (mort['85-89 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 90 and age < 95):
            print(age)
            deaths = ((t_pop[age][year] * (mort['90-94 years'][0]))) / 100000
            t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
            print(t_pop[age][year])
        if (age >= 95 and age<99):
                print(age)
                deaths = ((t_pop[age][year] * (mort['95+ years'][0]))) / 100000
                t_pop.at[year+1, age + 1] = ((t_pop[age][year]) - deaths)
                print(t_pop[age][year])
        if (age == 99):
                print(age)
                deaths = ((t_pop[age][year] * (mort['95+ years'][0]))) / 100000 + ((t_pop[age+1][year] * (mort['95+ years'][0]))) / 100000
                t_pop.at[year+1, age+1] = ((t_pop[age][year]+t_pop[age+1][year]) - deaths)
                print(t_pop[age][year])
        return t_pop

def slow_aging(year,age):
        t_pop_slow = t_pop.copy()
        deaths = 0
        if (age >= 30 and age < 35):
            print(age)
            deaths = ((t_pop_slow[age][year] * (mort['25-29 years'][0]))) / 100000
            t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
            print(t_pop_slow[age][year+1])
        if (age >= 35 and age < 40):
            print(age)
            deaths = ((t_pop_slow[age][year] * (mort['30-34 years'][0]))) / 100000
            t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
            print(t_pop_slow[age][year+1])
        if (age >= 40 and age < 45):
            print(age)
            deaths = ((t_pop_slow[age][year] * (mort['35-39 years'][0]))) / 100000
            t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
            print(t_pop_slow[age][year+1])
        if (age >= 45 and age < 50):
            print(age)
            deaths = ((t_pop_slow[age][year] * (mort['40-44 years'][0]))) / 100000
            t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
            print(t_pop_slow[age][year+1])
        if (age >= 50 and age < 55):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['45-49 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 55 and age < 60):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['50-54 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 60 and age < 65):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['55-59 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 65 and age < 70):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['60-64 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 70 and age < 75):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['65-69 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 75 and age < 80):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['70-74 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 80 and age < 85):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['75-79 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 85 and age < 90):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['80-84 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 90 and age < 95):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['85-89 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age >= 95 and age < 99):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['90-94 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]) - deaths)
                print(t_pop_slow[age][year+1])
        if (age == 99):
                print(age)
                deaths = ((t_pop_slow[age][year] * (mort['90-94 years'][0]))) / 100000 + ((t_pop_slow[age+1][year] * (mort['90-94 years'][0]))) / 100000
                t_pop_slow.at[year+1, age + 1] = ((t_pop_slow[age][year]+t_pop[age+1][year]) - deaths)
                print(t_pop[age][year+1])
        return t_pop_slow


def age_grp_pop(year, age):
        global total_sum
        global temp_pop
        if (age == 0):

            temp_pop.update({'year': year})
            temp_pop.update({'<1year' : t_pop[age][year]})

        if (age >=1 and age <5):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==4):
                temp_pop.update({'1-4 years': total_sum})
                total_sum = 0
        if (age >=5 and age <10):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==9):
                temp_pop.update({'5-9 years': total_sum})
                total_sum = 0
        if (age >=10 and age <15):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==14):
                temp_pop.update({'10-14 years': total_sum})
                total_sum = 0
        if (age >=15 and age <20):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==19):
                temp_pop.update({'15-19 years': total_sum})
                total_sum = 0
        if (age >=20 and age <25):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==24):
                temp_pop.update({'20-24 years': total_sum})
                total_sum = 0
        if (age >=25 and age <30):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==29):
                temp_pop.update({'25-29 years': total_sum})
                total_sum = 0
        if (age >=30 and age <35):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==34):
                temp_pop.update({'30-34 years': total_sum})
                total_sum = 0
        if (age >=35 and age <40):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==39):
                temp_pop.update({'35-39 years': total_sum})
                total_sum = 0
        if (age >=40 and age <45):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==44):
                temp_pop.update({'40-44 years': total_sum})
                total_sum = 0
        if (age >=45 and age <50):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==49):
                temp_pop.update({'45-49 years': total_sum})
                total_sum = 0
        if (age >=50 and age <55):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==54):
                temp_pop.update({'50-54 years': total_sum})
                total_sum = 0
        if (age >=55 and age <60):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==59):
                temp_pop.update({'55-59 years': total_sum})

                total_sum = 0
        if (age >=60 and age <65):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==64):
                temp_pop.update({'60-64 years': total_sum})
                total_sum = 0
        if (age >=65 and age <70):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==69):
                temp_pop.update({'65-69 years': total_sum})
                total_sum = 0
        if (age >=70 and age <75):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==74):
                temp_pop.update({'70-74 years': total_sum})
                total_sum = 0
        if (age >=75 and age <80):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==79):
                temp_pop.update({'75-79 years': total_sum})
                total_sum = 0
        if (age >=80 and age <85):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==84):
                temp_pop.update({'80-84 years': total_sum})
                total_sum = 0
        if (age >=85 and age <90):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==89):
                temp_pop.update({'85-89 years': total_sum})
                total_sum = 0
        if (age >=90 and age <95):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==94):
                temp_pop.update({'90-94 years': total_sum})
                total_sum = 0
        if (age >=95 and age <101):
            temp_sum = t_pop[age][year]
            total_sum = total_sum + temp_sum
            print(total_sum)
            if (age ==100):
                temp_pop.update({'95+ years': total_sum})
                pop.append(temp_pop)
                temp_pop ={}
                total_sum = 0
        print("Printing Pop...")
        print(pop)
        return pop

def cal_yld(dataframe):
    gbd_yld = gbd[gbd['measure_name']=='YLDs (Years Lived with Disability)']
    gbd_yld = gbd_yld.reset_index()
    gbd_yld['yld_value'] = 0
    disease_rate = {}
    yld = 0
    temp = 0
    for ind, row in dataframe.iterrows():
        for cause in gbd_yld['cause_name'].unique():
            if (age == 0):
                temp = (gbd_yld.loc[(gbd_yld['cause_name'] == cause) & (gbd_yld['age_name'] == '<1 year')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * dataframe[year]['<1year']
                disease_rate.update({'cause_name' : cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name' : "<1 year"})
                disease_rate.update({'yld_value' : yld/100000})
                gbd_yld = gbd_yld.append(disease_rate,ignore_index = True)
            if (age>=1 and age<5):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='1-4 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * dataframe[year]['1-4 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "1-4 years"})
                disease_rate.update({'yld_value': yld/100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=5 and age<10):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='5-9 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['5-9 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "5-9 years"})
                disease_rate.update({'yld_value': yld/100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=10 and age<15):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='10-14 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['10-14 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "10-14 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=15 and age<20):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='15-19 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['15-19 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "15-19 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=20 and age<25):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='20-24 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['20-24 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "20-24 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=25 and age<30):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='25-29 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['25-29 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "25-29 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=30 and age<35):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='30-34 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['30-34 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "30-34 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=35 and age<40):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='35-39 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['35-39 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "35-39 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=40 and age<45):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='40-44 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['40-44 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "40-44 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=45 and age<50):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='45-49 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['45-49 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "45-49 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=50 and age<55):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='50-54 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['50-54 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "50-54 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=55 and age<60):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='55-59 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['55-59 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "55-59 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=60 and age<65):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='60-64 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['60-64 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "60-64 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=65 and age<70):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='65-69 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['65-69 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "65-69 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=70 and age<75):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='70-74 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['70-74 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "70-74 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=75 and age<80):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='75-79 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['75-79 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "75-79 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=80 and age<85):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='80-84 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['80-84 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "80-84 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=85 and age<90):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='85-89 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['85-89 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "85-89 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=90 and age<95):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='90-94 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['90-94 years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "90-94 years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
            if (age>=95 and age<101):
                temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='95+ years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[year]['95+ years']
                disease_rate.update({'cause_name': cause})
                disease_rate.update({'year': year})
                disease_rate.update({'age_name': "95+ years"})
                disease_rate.update({'yld_value': yld / 100000})
                gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
    return gbd_yld

def sa_yld():
    gbd_sayld = gbd_yld.copy()
    gbd_sayld['yld_value_slow'] = gbd_yld['yld_value']
    for age in single_age:
        yld = 0
        temp = 0
        for cause in gbd_sayld['cause_name'].unique():
            if (age>=30 and age<35):
                temp = (gbd_sayld.loc[(gbd_yld['cause_name']==cause) & (gbd_sayld['age_name']=='25-29 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[7]['30-34 years']
                gbd_sayld.loc[(gbd_sayld['cause_name']==cause) & (gbd_sayld['age_name']=='30-34 years'),'yld_value_slow'] = yld/100000

            if (age >= 35 and age < 40):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '30-34 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[8]['35-39 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '35-39 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 40 and age < 45):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '35-39 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[9]['40-44 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '40-44 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 45 and age < 50):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '40-44 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[10]['45-49 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '45-49 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 50 and age < 55):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '45-49 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[11]['50-54 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '50-54 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 55 and age < 60):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '50-54 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[12]['55-59 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '55-59 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 60 and age < 65):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '55-59 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[13]['60-64 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '60-64 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 65 and age < 70):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '60-64 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[14]['65-69 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '65-69 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 70 and age < 75):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '65-69 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[15]['70-74 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '70-74 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 75 and age < 80):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '70-74 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[16]['75-79 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '75-79 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 80 and age < 85):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '75-79 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[17]['80-84 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '80-84 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 85 and age < 90):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '80-84 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[18]['85-89 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '85-89 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 90 and age < 95):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '85-89 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[19]['90-94 years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '90-94 years'), 'yld_value_slow'] = yld / 100000

            if (age >= 95 and age < 101):
                temp = (
                gbd_sayld.loc[(gbd_yld['cause_name'] == cause) & (gbd_sayld['age_name'] == '90-94 years')]['val'])
                temp_value = np.array(temp)
                yld = temp_value[0] * pop[20]['95+ years']
                gbd_sayld.loc[(gbd_sayld['cause_name'] == cause) & (gbd_sayld['age_name'] == '95+ years'), 'yld_value_slow'] = yld / 100000

    return gbd_sayld

grp_population = total_pop()
grp_population.set_index('year')
yld_population = cal_yld(grp_population)
print(pop)
#t_pop_slow = slow_aging()
#gbd_yld = cal_yld()
#gbd_yld_new = sa_yld()

#gbd_yld_new.to_excel("output_final.xlsx")
#t_pop.to_excel("pop_output.xlsx")
#t_pop_slow.to_excel("pop_slow_output.xlsx")
