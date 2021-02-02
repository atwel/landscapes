library(tidyverse)

library(jsonlite)

##Read in current selection not data.selection
df_player_stages<-read.csv('player-stages.csv')

#convert submission column into the current selection and the score
json_to_columns <- function(df, column){
  column <- enquo(column)
  
  json_df <- df %>% 
    pull(!!column) %>% 
    paste(collapse = ",") %>% 
    paste("[",.,"]") %>% 
    fromJSON(flatten = TRUE)
  
  df %>% 
    select(-!!column) %>% 
    bind_cols(json_df)
}

df_rounds_fixed<-json_to_columns(df_player_stages,data.submission)

library(anytime)

#read in stages
df_stages<-read.csv('stages.csv')
#merge with rounds
colnames(df_stages)[1]<-'stageId'
Merged_df<-merge(df_stages,df_rounds_fixed, no.dups=F)

#get correct time formats
Merged_df$startTimeAt<-anytime(Merged_df$startTimeAt)
Merged_df$submittedAt<-anytime(Merged_df$submittedAt)
#if no submit then add 30 seconds because that is the time alloted
Merged_df$submittedAt[is.na(Merged_df$submittedAt)]<-Merged_df$startTimeAt[is.na(Merged_df$submittedAt)]+30
#get time taken
Merged_df$time_taken<-Merged_df$submittedAt-Merged_df$startTimeAt
#remove for reflection
Merged_df[Merged_df[,'name'] %in% c('Reflection'),]$time_taken<-NA

#sort first by player id then by index (which corresponds to time as of right now at least)
Merged_df<-Merged_df[order(Merged_df['playerId'], Merged_df['index']),]
#get previous selection and 2 before, and their scores
Merged_df<-Merged_df %>%
  mutate(PreviousSelection = lag(curSelection),
        PreviousScore           = lag(score))
Merged_df<-Merged_df %>%
  mutate(Previous2Selection = lag(PreviousSelection),
        Previous2Score     = lag(PreviousScore))



manhattan_dist <- function(vector_1,vector_2){
  distance <- abs(as.double(unlist(vector_1))-as.double(unlist(vector_2)))
  distance <- sum(distance)
  return(distance)
}
#Put Step size into Dataframe
Merged_df$Step_Size_1<-mapply(Merged_df$curSelection,FUN= manhattan_dist,vector_2=Merged_df$PreviousSelection)
Merged_df$Step_Size_2<-mapply(Merged_df$curSelection,FUN= manhattan_dist,vector_2=Merged_df$Previous2Selection)

#Now Replace with NA's for the reflection and players first submissions
Merged_df[Merged_df[,'name'] %in% c('Submission 1','Reflection'),]$Step_Size_1<-NA
Merged_df[Merged_df[,'name'] %in% c('Submission 1','Reflection'),]$PreviousScore<-NA

#Second submission for two time lag
Merged_df[Merged_df[,'name'] %in% c('Submission 1','Submission 2','Reflection'),]$Step_Size_2<-NA
Merged_df[Merged_df[,'name'] %in% c('Submission 1','Submission 2','Reflection'),]$Previous2Score<-NA

#Also get previous scores
Merged_df$Score_Diff_1<-Merged_df$score-Merged_df$PreviousScore
Merged_df$Score_Diff_2<-Merged_df$score-Merged_df$Previous2Score


##Now Get player-rounds which tells us the task difficulty ranking
df_player_rounds<-read.csv('player-rounds.csv')
#keep pertinent data
df_player_rounds<-df_player_rounds[,c('roundId','playerId','data.quiz')]
#expand column
df_player_rounds<-json_to_columns(df_player_rounds,data.quiz)
#merge
Merged_df<-merge(Merged_df,df_player_rounds)




#Now get player inputs on task difficulty
df_player_inputs<-read.csv('player-inputs.csv')
df_player_inputs<-df_player_inputs[,c('gameId','playerId','data.task1','data.task2','data.task3','data.feedback')]
#The tasks are not in order, we must order the rounds from players to match the task order.
#Order Player submissions data set
Merged_df<-Merged_df[order(Merged_df['playerId'], Merged_df['index']),]

#Group by player to get the rounds they play in case we change round stuff later on
by_player <- Merged_df %>% group_by(playerId)
Num_rounds_by_player <-by_player %>% summarise(
  num_rounds_player = length(unique(roundId))
)

#only include players which completed all rounds
Rounds_To_Complete<-3
Num_rounds_by_player$Completed_All_Rounds<-0
Num_rounds_by_player[Num_rounds_by_player[,'num_rounds_player']==3,]$Completed_All_Rounds<-1


#group by player and round id
by_player_round_id <- Merged_df %>% group_by(playerId,roundId)
Task_Ordering_Times <-by_player_round_id %>% summarise(
     earliest = min(submittedAt),
    )
#merge
Task_Ordering_Times<-merge(Task_Ordering_Times,Num_rounds_by_player)
################
################################Now drop players which did not complete every round!
################
Task_Ordering_Times<-Task_Ordering_Times[Task_Ordering_Times$Completed_All_Rounds==1,] 


#### Redo this if there are varying number of rounds and we dont want to drop incompletes
#Order times then assign as task1, task2, task3
Task_Ordering_Times<-Task_Ordering_Times[order(Task_Ordering_Times['playerId'],(Task_Ordering_Times['earliest'])),] 
Num_Players <-nrow(Task_Ordering_Times)/3
Task_Ordering_Times$condition<-c(rep(c('task1','task2','task3'),Num_Players))

#convert from wide to long
player_inputs_long <- gather(df_player_inputs, condition, measurement, data.task1:data.task3, factor_key=TRUE)
#remove prefix in data
player_inputs_long$condition<- sub("data.", "", player_inputs_long$condition)
#merge into task ordering then merge back into dataframe
Merge_Tasks<-merge(player_inputs_long,Task_Ordering_Times,on=c('playerId','condition'))
#merge back into main dataset
Merged_df<-merge(Merged_df,Merge_Tasks)
#Drop useless columns
drops <- c("earliest","data.selection","durationInSeconds")
Merged_df<-Merged_df[ , !(names(Merged_df) %in% drops)]

#Rename measurement as difficulty
colnames(Merged_df)[colnames(Merged_df)=='measurement']<-'difficulty'


####NOW what I am missing is the data for landscape types. I will have to find a way to merge that in later.
#For now let us presume I have some piece of data called 'landscape_type'

#Get sample statistics by landscape type
#for now sub in gorup id


##I need factor ids which are treatments. Factor id is unique to player.
### In file named 'games' tehre are treatmentIDs and roundIDs and player IDs
#in 'treatments' there are factorIds. I must expand the concatenated 6. It is in the order which they are selected.
#"_id in 'factors.csv' corresponds to factorIDs of treatments (order changes each time)

#'games.csv' maps to 'treatments.csv' and factorids map onto 'factors'

#I know which rounds happen first second and third; I need to know which factors happen first second and third.
#factor types has game ordering. 'id_' in factory-types.csv maps back into 'factorTypeID' in factors.csv


df_games<-read.csv('games.csv')
df_treatments<-read.csv('treatments.csv')

df_factors<-read.csv('factors.csv')
df_factor_types<-read.csv('factor-types.csv')

#expand df_treatments so each factor id has a row
df_treatments_expanded<-df_treatments %>% 
  mutate(factorIds = strsplit(as.character(factorIds), ",")) %>% 
  unnest(factorIds)

#rename columns
colnames(df_treatments_expanded)[1]<-'treatmentId'
colnames(df_treatments_expanded)[2]<-'Type_Experiment'
colnames(df_treatments_expanded)[3]<-'factor_Id'
#drop columns not needed
df_treatments_expanded<-df_treatments_expanded[ , -which(names(df_treatments_expanded) %in% c("createdAt",'archivedAt'))]

#now prepare to merge with df_games
#first change names
colnames(df_games)[1]<-'gameId'
treatment_game_merge<-merge(df_games,df_treatments_expanded,by='treatmentId' ,no.dups=F)


#drop unneeded columns
treatment_game_merge<-treatment_game_merge[ , -which(names(treatment_game_merge) %in% c("finishedAt",'gameLobbyId','createdAt'))]


#Factor types and factors must merge to get ordering

factor_type_order<-merge(df_factors,df_factor_types, by.x='factorTypeId',by.y='X_id')
#change name of 'name.y' into task condition (i.e. task1, task2, task3) 
#then change into a condition task1,task2,task3 for later merge with the Merged_df
#Task_Ordering_Times$condition<-c(rep(c('task1','task2','task3'),Num_Players))
colnames(factor_type_order)[2]<-'factor_Id'
colnames(factor_type_order)[6]<-'condition'
#Drop rows where condition is not numeric
#Define function to extract numbers
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
factor_type_order<-factor_type_order %>% 
  mutate(condition =paste('task',(numextract(condition)),sep=''))



#drop these colums: type, min, max,value createdAt.y, archivedAt, required
factor_type_order<-factor_type_order[ , -which(names(factor_type_order) %in% c("max","min",'value','createdAt.y','createdAt.x','type','archivedAt','required'))]

#Now merge  treatment_game_merge with factor_type_order by factor_id
merged_gameid_treatment_factor<-merge(factor_type_order,treatment_game_merge,by='factor_Id' ,no.dups=F)
#change column names
colnames(merged_gameid_treatment_factor)[3]<-'landscape_type'
#drop if task condition is NA
merged_gameid_treatment_factor<-merged_gameid_treatment_factor[merged_gameid_treatment_factor$condition!='taskNA',]
#Merge with merged df

Final_Merger<-merge(Merged_df,merged_gameid_treatment_factor,on=c('condition,gameId'))


#merge by game id (X_Id in df_games and treatment game merge) and condition

Stats_By_Landscsape <-Final_Merger %>% 
  group_by(landscape_type) %>%
  summarise(
  avg_score = mean(score),
  avg_step_size1 = mean(Step_Size_1[!is.na(Step_Size_1)]), #include !is.na so I dont include reflections or stages without data
  avg_step_size2 = mean(Step_Size_2[!is.na(Step_Size_2)]),
  avg_score_change1 = mean(Score_Diff_1[!is.na(Score_Diff_1)]),
  avg_score_change2 = mean(Score_Diff_1[!is.na(Score_Diff_2)]),
  time_taken = mean(time_taken[!is.na(time_taken)]),
  mean_easy = sum(difficulty[name=='Reflection']=='easy')/3,
  mean_hard = sum(difficulty[name=='Reflection']=='hard')/3,
  mean_medium = sum(difficulty[name=='Reflection']=='med')/3
)


#Get stats to chart behavior round by round
Stats_by_Submission <- Final_Merger %>% group_by(landscape_type,index) %>% #replace round_id with landscape type
  summarise(
    avg_score = mean(score),
    avg_step_size1 = mean(Step_Size_1[!is.na(Step_Size_1)]), #include !is.na so I dont include reflections or stages without data
    avg_step_size2 = mean(Step_Size_2[!is.na(Step_Size_2)]),
    avg_score_change1 = mean(Score_Diff_1[!is.na(Score_Diff_1)]),
    avg_score_change2 = mean(Score_Diff_1[!is.na(Score_Diff_2)]),
    time_taken = mean(time_taken[!is.na(time_taken)]),
    name=name
    )
Stats_by_Submission

