README

Replication data for "The Ties that Double Bind: Social Roles and Women's Underrepresentation in Politics." For any questions about this replication file, please contact Josh Kalla at josh.kalla@gmail.com.

Note that the online appendix can be replicated using "Online Appendix for Ties that Double Bind.Rmd". 

Folder Structure Required for Replication:
For the code to run, we recommend recreating the below file structure locally. 
. #Base folder. For example, on your desktop you might have a file called "Ties that Double Bind Replication Data". When any of the code changes the working directory, you should set this base folder as the working directory.
├── code #This sub-folder within the base folder contains all the .do files necessary to create the below figures.
├──├── appendix_figs # This sub-folder within "code" contains the files specifically for the appendix figures. The particular .do files are listed below.
├──├── main_figs # This sub-folder within "code" contains the files specifically for the figures in the main text. The particular .do files are listed below.
├── data #This  sub-folder within the base folder contains all the data files listed in “Data” below. All .csv and .dta files go here.
├── figures #All of the output figures produced from the code go into this sub-folder. When starting, this folder can be empty. "Survey Screenshot" goes here.
├──├── appendix_figs # This sub-folder within "figures" holds the appendix figures
├──├── main_figs # This sub-folder within "figures" holds the main text figures figures
All other files can be in the base folder. Specifically, this includes "Online Appendix for Ties that Double Bind.Rmd" and "README.txt". 

Data:
The data necessary to replicate all figures, tables, and results are contained in the “data” folder. This includes:
conjoint_data.dta -- primary data for the conjoint analysis
conjoint_data_2017leg.dta -- 2017 legislator conjoint data where attributes were changed
respondents_YEAR.csv -- data on the legislators who took the survey in each year
candidate_bios_pvs.csv -- biographies from Project Vote Smart

Variable Notes:
sample -- Voter or legislator sample
replication -- If in the legislator sample, replication = 1 for 2017 data; 0 for 2014 data
first_orig -- If in the 2017 legislator sample, first_orig = 1 if the respondent saw the original candidate attributes from the 2014 experiment; 0 if the respondent was in the conditions where either corporate was removed from the lawyer occupation or occupation and political post were separated.
first_corp -- Corporate was removed from the lawyer occupation if = 1
first_pol -- Occupation and political post were seperate attributes

**Main Text Figures:**

Figure 1: Testing for outright hostility against female candidates among political elites and the mass public.
--> See: fig1.pdf
--> Produced in: fig 1 overall results by sample.do
Figure 2: Testing for outright hostility by respondent sex.
--> See: fig2.pdf
--> Produced in: fig 2 overall results by respondent gender.do
Figure 3: Testing for outright hostility by respondent partisanship.
--> See: fig3.pdf
--> Produced in: fig 3 overall results by respondent party.do 
Figure 4: Testing for the double standard. Results pooled for both legislators and voters.
--> See: fig4.pdf
--> Produced in: fig 4 differential reward by candidate gender.do
Figure 5: Measuring social role bias. Do people favor traditional family responsibilities?
--> See: fig5.pdf
--> Produced in: fig 5 social role bias.do

**Appendix Figures:**

Results from the 2014 Surveys by Outcome Question Wording
--> See: fig1_bydv.pdf
--> Produced in: fig 1 overall results by DV.do
Results by First Rating Task
--> See: fig1_firstrating.pdf
--> Produced in: fig 1 overall results by sample first rating task only.do
Replication Results, Removing Corporate Lawyer
--> See: fig1_nocorp.pdf
--> Produced in: fig 1 replication no corp law.do
Replication Results, Separating Political Post from Occupation
--> See: fig1_nopol.pdf
--> Produced in: fig 1 replication separate political.do
Effect of Changing Attribute by Gender of Candidate for Democratic Respondents
--> See: fig4_democrats.pdf
--> Produced in: fig 4 differential reward by candidate gender democrats.do
Effect of Changing Attribute by Gender of Candidate for Republican Respondents
--> See: fig4_republicans.pdf
--> Produced in: fig 4 differential reward by candidate gender republicans.do
Effect of Changing Attribute by Gender of Candidate among Voters
--> See: fig4_voters.pdf
--> Produced in: fig 4 differential reward by candidate gender voters only.do
Effect of Changing Attribute by Gender of Candidate among Legislators
--> See: fig4_leg.pdf
--> Produced in: fig 4 differential reward by candidate gender legislators only.do
