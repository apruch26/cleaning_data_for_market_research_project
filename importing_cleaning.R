# Importing Data from Survey Monkey and cleaning it for Analysis
# read spss data in w/ haven package 
data_spss<-read_spss("Arts_DataCleanExample.sav")
# delete first row
data_spss<-data_spss[-1,]
# unite Type of Craft data into single column 'Craft_Activity' in a new dataframe w/ unite function from tidyr package 
data_unite<-unite(data_spss,Craft_Activity,TextileCraft,PaperCraft,DecorativeCraft,FashionCraft,VisualCraft,sep="")
# add the new column 'Craft_Activity' to the Original Dataset w/ bind_cols from dplyr package
data_spss<-bind_cols(data_spss,data_unite['Craft_Activity'])
# rearrange the columns so the new column 'Craft_Activity' is following the Type of Craft Data
data_spss<-data_spss[,c(1:7,59,8:58)]
# add a new column 'Craft_Code' with value of 0 for every observation 
data_spss$Craft_Code<-0
# for loop for filling the new column 'Craft_Code' with correct values 
for(i in 1:159){
  result<-character()
  if(nchar(data_spss$Craft_Activity[i])==1){result<-data_spss$Craft_Activity[i]}
  else if(nchar(data_spss$Craft_Activity[i])==2){result<-6} # crafters who selected 2 = 6
  else if(nchar(data_spss$Craft_Activity[i])==3){result<-7} # crafters who selected 3 = 7
  else if(nchar(data_spss$Craft_Activity[i])==4){result<-8} # crafters who selected 4 = 8
  else if(nchar(data_spss$Craft_Activity[i])==5){result<-9} # crafters who selected 5 = 9
  else{result<-''}
  data_spss$Craft_Code[i]<-result
  #print(result)
  #print(i)
}
# new for loop for new CraftType code
for(i in 1:159){
  result<-character()
  if(nchar(data_spss$Craft_Activity[i])==1){result<-1}
  else if(nchar(data_spss$Craft_Activity[i])==2){result<-2} # crafters who selected 2 = 2
  else if(nchar(data_spss$Craft_Activity[i])==3){result<-3} # crafters who selected 3 = 3
  else if(nchar(data_spss$Craft_Activity[i])==4){result<-4} # crafters who selected 4 = 4
  else if(nchar(data_spss$Craft_Activity[i])==5){result<-5} # crafters who selected 5 = 5
  else{result<-''}
  data_spss$Craft_Code[i]<-result
  #print(result)
  #print(i)
}
# rearranging columns so Craft_Code is after Craft_Activity
data_spss<-data_spss[,c(1:8,60,9:59)]


# coding YearBorn to millenial, genX, genY, Baby Boomer
year_born<-as.numeric(data_spss$YearBorn)
age<-2016-year_born
for(i in 1:159){
  if(is.na(age[i])){age[i]<-0}
}

data_spss$Generation<-'0'

for(i in 1:159){
  Generation<-character()
  if(age[i]==0){Generation<-""}
  else if(age[i]>=71 & 91>=age[i]){Generation<-'1'} # 1 = The Silent Generation
  else if(age[i]>=52 & 70>=age[i]){Generation<-'2'} # 2 = Baby Boomer Generation
  else if(age[i]>=37 & 51>=age[i]){Generation<-'3'} # 3 = Generation X
  else{Generation<-'4'} # 4 = Generation Y & Millenial
  print(Generation)
  data_spss$Generation[i]<-Generation
}

for(i in 1:159){
  if(is.na(data_spss$TypeCrafter[i])){data_spss$TypeCrafter[i]<-0}
}

data_spss$TypeCrafter_New<-0
for(i in 1:159){
  new_craft<-numeric()
  if(data_spss$TypeCrafter[i]==1 | data_spss$TypeCrafter[i]==2){new_craft<-1}
  else if(data_spss$TypeCrafter[i]==3 | data_spss$TypeCrafter[i]==4){new_craft<-2}
  else{new_craft<-''}
  data_spss$TypeCrafter_New[i]<-new_craft
}


# writing the new data set to a csv file 
write.csv(data_spss,"ArtsCrafts_CleanData.csv",row.names=FALSE)

# writing the new data set to a csv file for new cross tab between craft type 
write.csv(data_spss,"ArtsCrafts_CleanData1.csv",row.names=FALSE)

# writing the new data set to a csv file for new variable Type of Crafter = 1 for Beginner or Crafer or 2 for Exeperienced or Professional 
write.csv(data_spss,"ArtsCrafts_CleanData2.csv",row.names=FALSE)

