import pandas as pd
import numpy as np

grp_population = pd.read_excel("age_group_output.xlsx")
grp_population = grp_population.set_index('Year')
print(grp_population.head(10))
gbd = pd.read_excel("gbd.xlsx")

single_age = np.arange(0,101,1)


def cal_yld(dataframe):
    gbd_yld = gbd[gbd['measure_name']=='YLDs (Years Lived with Disability)']
    gbd_yld = gbd_yld.reset_index()
    gbd_yld['yld_value'] = 0
    disease_rate = {}
    yld = 0
    temp = 0
    for ind, row in dataframe.iterrows():
        for cause in gbd_yld['cause_name'].unique():
            for age in single_age :
                    if (age == 0):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name'] == cause) & (gbd_yld['age_name'] == '<1 year')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'<1year']
                        disease_rate.update({'cause_name' : cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name' : "<1 year"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value' : yld/100000})
                        gbd_yld = gbd_yld.append(disease_rate,ignore_index = True)
                    if (age == 1):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='1-4 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'1-4 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "1-4 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld/100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 5):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='5-9 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'5-9 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "5-9 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld/100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age ==10):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='10-14 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'10-14 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "10-14 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 15):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='15-19 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'15-19 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "15-19 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age ==20):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='20-24 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'20-24 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "20-24 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age ==25):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='25-29 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'25-29 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "25-29 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age ==30):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='30-34 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'30-34 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "30-34 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age ==35):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='35-39 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'35-39 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "35-39 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age ==40):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='40-44 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'40-44 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "40-44 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 45):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='45-49 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'45-49 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "45-49 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 50):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='50-54 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'50-54 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "50-54 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 55):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='55-59 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'55-59 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "55-59 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 60):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='60-64 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'60-64 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "60-64 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 65):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='65-69 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'65-69 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "65-69 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 70):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='70-74 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'70-74 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "70-74 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age ==75):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='75-79 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'75-79 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "75-79 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 80):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='80-84 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'80-84 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "80-84 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age ==85):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='85-89 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'85-89 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "85-89 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 90):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='90-94 years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind,'90-94 years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "90-94 years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
                    if (age == 95):
                        temp = (gbd_yld.loc[(gbd_yld['cause_name']==cause) & (gbd_yld['age_name']=='95+ years')]['val'])
                        temp_value = np.array(temp)
                        yld = temp_value[0] * dataframe.at[ind, '95+ years']
                        disease_rate.update({'cause_name': cause})
                        disease_rate.update({'year': ind})
                        disease_rate.update({'age_name': "95+ years"})
                        disease_rate.update({'val': temp_value[0]})
                        disease_rate.update({'yld_value': yld / 100000})
                        gbd_yld = gbd_yld.append(disease_rate, ignore_index=True)
    return gbd_yld


yld_population = cal_yld(grp_population)
yld_population.to_excel('yld_output.xlsx')
