Leader Data: BdM2S2 
In this zip file there five files. 
leaderdata.dta (stata 7 data file) and leader_data_orginal_collected_data.xls (excel file) represent the data orginially collected. 
The batch file organize_leader_data.do organizes these data and converts them into the files we use. For instance, it assigns each leader a unique identifying code (leader_id), and codes the day leaders entered and left office. This btach file then creates two datasets, the ones we actually use. 
bdm2s2_basic_leader_list.dta (stata 7 data file) is a list of all leaders and the dates they entered and left office. 
bdm2s2_leader_year_data.dta (stata 7 data file) organises data by leader year. That is to say a leader year observations appears for any leader who was in power at least part of this year. For example, there are observations for William Clinton (US President) for 1993, 1994, ..., 2001, and observations for George W. Bush for 2001, 2002, etc. It is this file that we utilize to analyze survival in chapter 7. 


