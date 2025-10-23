# Introduction
The aim of this Cato Institute Social Security model is to reproduce the Social Security Administration's methodology for projecting outlays and scoring reforms. The Social Security Administration uses Modeling Income in the Near Term 8 (MINT 8) to evaluate some policies based on assumptions set by the Social Security Trustees Report. However, our model Our model, like MINT 8, is non-dynamic.   It differs from MINT 8 substantially, insofar as it does not rely on regression methods to predict and forecast future outcomes. Instead, most of our microsimulations rely on inverse transform sampling based on the Social Security Administration's assumptions. The aim of this document is to guide any attempts to replicate the Cato Model. 
# Data Sources
We use five main data sources for our model:
1. Microdata from the Current Population Survey Annual Social and Economic Supplement (ASEC), retrieved from the Integrated Public Use Microdataset (IPUMS).
2. Social Security Trustee's Reports (from 2023 onwards)
3. Social Security Statistical Supplements (from 2023 onwards)
4. The 2024 Survey of Income and Program Participation (SIPP)
5. The 2007 Social Security Public Use Files Microdata
These data sources are then supplemented by other publicly available data sources. Instances of this are noted in the following documentation.  
# Initial Sample
We sampled 10,000 households from the 2007 ASEC microdata to create the initial sample. This sample was created using proportional sampling based on age and sex, with probabilities provided by the 2007 Social Security report. All households are assigned a unique household ID. Individuals are also assigned a unique personal ID. We also simplify family relationships significantly. In our model, households only consist of a household head, their spouse, and children. 

To impute pre-2007 earnings, we use a simplified version of Smith et al.'s (2001) algorithm for matching Social Security records with Survey of Income and Program Participation (SIPP) data.  We use a modified version of Smith et al.’s method for joining the 2006 Social Security Public Use File with earnings data. Specifically, we:
* Find all records in the 2007 Social Security Public Use Files that have ages in the range $[Age_i-1,Age_i+2]$, where $Age_i$ is the age of the individual. 
* Let individuals in the 2007 Social Security PUF who meet these conditions be called D. From the set of Social Security Public Use File records, we compute $D(i,j)=(W_j-W_i)^2$  , where  W_i is the wage income of the individual in our sample and $W_j$ is the wage of all individual $d$ in $D$. 
* We then find an individual d in with the minimum $D(i,j)$, and let their 1951-2007 earnings history represent the earning histories of our original sampled individual. 

This simple algorithm gives us usable income and wage histories from 1951 to 2007. There are some limitations to this method. For one, married individuals and individuals with children have different income trajectories. Unfortunately, the Social Security PUFs do not report year of marriage and number of children. As such, we cannot account for these variations. Moreover, the Social Security PUFs are top coded at the taxable maximum. As such, we have some difficulties trying to find the earning histories of high-income individuals. However, this method gives us a good approximation of cohort-specific income dynamics. 

# Mortality and New Households Process
Since 2004, the Social Security Administration has produced a set of life tables. We calculate the probability of death using the following:

$$Death_{i,y}=I(K_{i,a,s,y}>M_{a,s,y}^d )$$

Where $K_{i,a,s,y}  \sim U(0,1)$ is the probability of living for individual i by age, sex, and year, and $M_{a,s,y}^d$ is the marginal probability of death by age, sex, and year.  $M_{a,s,y}^d$ is retrieved from actuarial lifetables provided by the Social Security Administration.  If the individual who dies is married, their spouse is marked as a widow. If the individual who dies has children, their children are marked as orphans. Each surviving individual has their age increased by a year. Here we also make the relatively strong assumption that every individual who reaches the age of 18 moves away from their original family unit. As such, these individuals are assigned to a new household ID.  

There is evidence of significant income differentials with respect to death probabilities.  Moreover, there has also been an increase in adults living with their parents.  Unfortunately, our model does not (at this stage) capture these demographic processes.  
# Fertility Process

The Social Security Administration also produces projections of the age structure of the United States over the next 75 years. Using these projections and National Vital Statistics Report surveys, we are able to find how many children were born to specific age groups. First, we assume that the proportion of children born to each child-rearing age (14-45) does not differ significantly from 2024 to 2100. We can then calculate the proportion of children born in each cohort from 2007 to 2024 and project this to 2100. As such, the number of children born to age group a in year t where t > 2025 is:

$$b_{a,t}=p_{a,2024}*∑b_{a,t}$$  

Where $∑b_g$ is the number of babies born in year t, and $p_{g,2024}$ is the proportion of babies born to age group g. The probability of a woman having a baby can thus be calculated:

$$p_{i,a,t}= \frac{b_{a,t}}{n_{a,t}} $$ 

Where $n_{a,t}$ is the number of women age a at time t, we can determine which woman has a baby by using the following method:

$$B_{i,t}=I(K_{i,t} \< p_{i,a,t} )$$

Where $K_{i,t} \sim U(0,1)$ is the probability of not having a child. 

We note here that this method has limitations. For one, fertility varies significantly by income and education.  This relationship is one which has been very well documented in the fertility literature. Our model does not capture this relationship. Moreover, the age of first fertility for women has been constantly increasing in the U.S. over the last two decades. This may mean that our assumption of constant fertility proportions by age may not be the most accurate in the long term. 

# Marriage Process
The Social Security Administration also produces estimates of the number of married individuals by cohort. As such, to find the number of newly married individuals by age, sex, and year, we:

$$NewMarried_{a,s,t}=TotalMarried_{a,s,t}-AlreadyMarried_{a,s,t}$$

$AlreadyMarried_{a,s,t}$ is simply the number of individuals within our model who are already married. We can then find the probability of a non-married individual being married by:

$$p_{i,s,t}= \frac{NewMarried_{a,s,t}}{Single_{a,s,t}}$$

We can then find the set of newly married individuals for men and women by using the following equation:

$$M_{i,s,t}=I(k_{i,s,t} < p_{i,s,t})$$


Where $k_{i,s,t} \sim U(0,1)$ is the probability of an individual staying single for year k. Now that we have the set of married individuals, we have to match individuals. To do this, we take the set of single men and calculate the weighted distance of that man’s demographic characteristics to other women using the following:

$$Dist(m,f)=
\frac{1}{\pi}\sqrt{
\frac{1}{1000}(Inc_m-Inc_f )^2+ +(Age_m-Age_f )^2
}$$

Where π is the probability that the woman gets married. The woman with the minimum distance from the man is married. The woman, in this case, is assigned the spouse role, and this newly married couple is assigned a new household ID. 

A similar process can be assigned for divorce. The SSA provides an estimate of the number of divorced individuals. To estimate the number of newly divorced individuals, we first find:


$$NewDivorced_{a,s,t}=TotalDivorced_{a,s,t}-AlreadyMarried_{a,s,t}$$

We can then find the set of newly married individuals for men and women by using the following equation:

$$M_{i,s,t}=I(k_{i,s,t} < p_{i,s,t})$$

We can then find the probability of a married individual obtaining a divorce:

$$dp_{i,s,t} = \frac{NewDivorced_{a,s,t}}{Married_{a,s,t}} 
$$

We can then find the set of newly divorced individuals for men and women by using the following equation:

$$D_{i,s,t}=I(k_{i,s,t} < dp_{i,s,t})$$

Where $k_{i,s,t}$ is the probability of the couple staying together.  We note that these methods are also relatively simplistic and may miss some demographic regularities. For one, our model assumes that marriage is a function of age and income. Of course, marriage is not only based on these factors. Moreover, we assume that every couple has an identical rate of divorce, which may not necessarily be true. 

# Employment and Income Process
The Employment and Income Process first starts by ascertaining whether an individual stays or exits employment. To do this, we first estimate a transition matrix by age, year, and sex, of whether an individual stays or exits the labor force. We assume that the transition matrix stays constant from 2024 onwards. We also make some changes: an individual age 70 or more exits the labor force with a 0.5 probability and never reenters the labor force once they exit. An individual who exits employment at the age of 62 or more is considered to be retired, andretired and thus will get Social Security benefits. We also correct the transition matrix by calibrating the employment percentage to the assumptions of the Social Security Administration. 

All employed workers then have their previous years’ income grow solely at the AWI rate. New workers are assigned a new income randomly from the 2006 SSA public use file income distribution, adjusted for the AWI at the time of workforce entry. In order to ensure that the imputed incomes have roughly the same distribution, we also quantile match the incomes to income distribution data received from the SSA. We assume that income distribution from 2025-2100 does not change significantly.
# Model Capacities and Limitations
Our model is able to yield a conventional, long-term Social Security analysis. This includes outputting metrics like the 75th year balance, the long-range actuarial balance, the Trust Fund ratio, the insolvency date, ten-year savings, and first-year of positive balance. We also have experimental capacities to conduct distributional analyses.  

Our model can also score a wide variety of reforms, including (but not limited to) changes in the retirement ages, replacement rates, bend points, indexing years/methodology, and retirement credits/penalties. We cannot score policies relating to tax code changes, such as pass-through tax reforms. We also have limited capabilities to score provisions relating to changes in Windfall Elimination and the Government Pension Offset.  

# Instructions For Using Repository
* Create _init_sim_data.Rdata_ using _data_prep_init.R_ in _data_prep_. For your convenience, a copy of  _init_sim_data.Rdata_ has been uploaded into  _data_prep_.
* Load _init_sim_data.Rdata_ into _initial_simulation.R_ in _code_src_, and run the code. This will output _initial_simulation.RDS_,. which contains simulated projections of the demographic structure of the United States for the entire projection period.
* Create _baseline_data_prep.RData_ using _baseline_data_prep.R_. For your convenience, a copy of  _baseline_data_prep.RData_ has been uploaded.
* Load _baseline_data_prep.RData_ and  _initial_simulation.RDS_ into _baseline_simulation.R_ in _code_src_. This will give you the Cato projections.
* Note that _baseline_simulation.R_ cannot be run as it is, since it requires access to the Social Security 2006 PUF, which has been uploaded to the Cato MariaDB database. Users wishing to replicate _baseline_simulation.R_ must create a table of the PUF with the following format:
<img width="473" height="196" alt="image" src="https://github.com/user-attachments/assets/50030b93-8198-4cf9-8982-aa67a88dd349" />

# Further Assistance
Please contact [socialsecurity@cato.org](socialsecurity@cato.org) for further inquiries and assistance. 
