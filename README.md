# dalys-model

Input data : 

•	Mortality : https://vizhub.healthdata.org/gbd-results/

•	Fertility : https://population.un.org/wpp/Download/Standard/Fertility/

•	DALYS : https://vizhub.healthdata.org/gbd-results/

•	Population estimates for 2021 : https://population.un.org/wpp/Download/Standard/Population/


•	Pop_Estimation Python file – The file does four main operations : 
  1.	Calculates age wise population estimates upto 2100 /n
  2.	Calculates dalys with business as usual case
  3.	Calculates mortality adjusted age wise population estimates upto 2100
  4.	Calculates adjusted dalys with slow-aging applied

•	Population Estimation Process : 
  1.	2021 is used a base year for age-wise population estimation
  2.	New-borns are calculated for (n+1) year by using fertility data for female population between 15-49 years. (Assumption : Female population = Total population/2)
  3.	Next, mortality rates from Global burden of diseases are used to calculate age wise population figures for (n+1) year. ( Since GBD mortality rates are available in five year age groups, the same mortality rate was applied for age within that five age group. For example, for a person aged 36 years, mortality rate for age group 35-39 years is applied and population for the next year is calculated.

•	Slowing down mortality rates: Starting age 30, we applied mortality rates of the k-1 age group to kth age group. For example, a person in the age group of 55-59 years, mortality rates for age group 50-54 were applied and then population figures were calculated for the next year
