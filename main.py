# This script estimates dalys and corresponding economic costs for the time period 2020-2100

#Load all required libs
import numpy as np
import pandas as pd

#Load the input data files

pop = pd.read_excel("pop.xlsx")
gbd = pd.read_excel("gbd_2019.xlsx")

#print(pop.head())
#print(gbd.dtypes)
pop = pop.melt(id_vars = ['year'], var_name = "age_name", value_name = "pop")

print(pop.head())

#Add a new year column to the main gbd data
gbd['year'] = gbd['year'] + 2


print(gbd['year'].tail())

gbd_temp = gbd.copy(deep=True)
print(gbd_temp.head())

#Create new rows for years until 2100
for i in range(1,80):

    gbd_year = gbd_temp['year']+1
    gbd_temp['year'] = gbd_year
    gbd = pd.concat([gbd,gbd_temp],ignore_index=True)
    print(gbd_year[0])

#merge gbd data with corresponding population data

gbd = pd.merge(gbd,pop, on=['year','age_name'])
print(gbd['year'],gbd['age_name'],gbd['pop'])
print(gbd.shape)
#gbd.to_excel('gbd_ouput.xlsx')
print(gbd.isnull().sum().sum())
#Slow-aging

#print(gbd.columns)
unique_age_names = gbd.age_name.unique()
print(unique_age_names)
gbd['age_name_id'] = gbd['age_name'].copy(deep=True)
gbd.age_name_id = pd.Categorical(gbd.age_name_id,['<1 year','1-4 years','5.9 years','10-14 years','15-19 years','20-24 years', '25-29 years',
                                 '30-34 years','35-39 years','40-44 years','45-49 years','50-54 years','55-59 years','60-64 years',
                                 '65-69 years','70-74 years','75-79 years','80-84 years','85-89 years','90-94 years','95+ years'], ordered=True)
print(gbd.age_name_id)
print(gbd.columns)
gbd.age_name_id = gbd.age_name_id.cat.codes
print(gbd.head(20))
print(gbd['cause_id'].head(20))

gbd['age_cause_id'] = gbd['age_name_id']*gbd['cause_id']

print(gbd['age_name_id'].unique())
#print(gbd['val'].dtypeaudi)
gbd['dalys_val']=None
temp_data=[]
for year in gbd['year'].unique():
    print(year)
    for cause in gbd['cause_id'].unique():
        print(cause)
        for age_grps in unique_age_names:
            filter = ((gbd['age_name'] == age_grps) & (gbd['cause_id'] == cause) & (gbd['year'] == year))
            if age_grps == '30-34 years':
                temp_filter = ((gbd['age_name'] == '25-29 years')& (gbd['cause_id'] == cause) & (gbd['year'] == year))
                print(gbd.loc[temp_filter, 'cause_name'])
                temp_data = gbd.loc[temp_filter,'val'].copy(deep=True)
                temp_val = np.array(temp_data)
                main_data = gbd[filter]['dalys_val']
                gbd.loc[filter,'dalys_val'] = temp_val[0]
            else:
                print("did not enter the loop")
print(gbd['dalys_val'].unique())
excel_filter = (gbd['age_name'] == '30-34 years')
excel_gbd = gbd[excel_filter].copy(deep=True)

gbd.to_excel('test.xlsx')