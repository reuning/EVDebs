elec <- read.dta("SLERs1967to2010_2012_05_26.dta") #ICPSR NO 8907 Data 

states <- unique(elec$v02)
year <- unique(elec$v05)

senate.df <- expand.grid(states,year)
house.df <- expand.grid(states,year)

gen.elec <-
  elec[elec$v17 == 1 | elec$v16 %in% c("SSG", "SR", "S") ,]

for(ii in 1:nrow(senate.df)){
  tmp <- gen.elec[gen.elec$v02 == senate.df[ii,1] & gen.elec$v05 == senate.df[ii,2], ]

  tmp.senate <- tmp[tmp$v07==8, ]
  if(nrow(tmp.senate)!=0){
    for(jj in 1:10){
      senate.df[ii,(2+jj)] <- any(tmp.senate$v12 == jj)*1
    }
  }

  tmp.house <- tmp[tmp$v07==9, ]
  if(nrow(tmp.house)!=0){
    for(jj in 1:10){
      house.df[ii,(2+jj)] <- any(tmp.house$v12 == jj)*1
    }
  }

}

colnames(senate.df) <- c("State", "Year", "SMD", "MMD-Post",
                    "MMD-FFA", "MMD-AY-1PY", "MMD-AY-Posts",
                    "MMD-AY-FFA", "FD-SMD", "FFD-MMD-Posts",
                    "FFD-MMD-FFA", "MMD-Cumulative")
colnames(house.df) <- c("State", "Year", "SMD", "MMD-Post",
                         "MMD-FFA", "MMD-AY-1PY", "MMD-AY-Posts",
                         "MMD-AY-FFA", "FD-SMD", "FFD-MMD-Posts",
                         "FFD-MMD-FFA", "MMD-Cumulative")
senate.df <- senate.df[rowSums(is.na(senate.df))!=10,]
house.df <- house.df[rowSums(is.na(house.df))!=10,]

write.csv(senate.df, "Senate-District-Type.csv", row.names=F)
write.csv(house.df, "House-District-Type.csv", row.names=F)