PHABsites=read.csv("C:/Users/jenni/Box/NAMC (Trip Armstrong)/Research Projects/AIM/P_Hab Modeling/Final analyses/modeling/final_screened_dataset.csv")
PHABsites$COMID

l=list()
for (s in 1759:length(PHABsites$COMID)){
  l[[s]]=p_drainage_density(SQLite_file_path,geometry_input_path,geometry_input_name,PHABsites$COMID[s])

}

97
573
576
1728
1739
1743
1758
