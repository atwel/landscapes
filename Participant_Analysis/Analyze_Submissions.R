library(tidyverse)

library(jsonlite)

###SEt Working directory
#setwd("~/Documents/ResearchProjects/landscapes/Participant_Analysis")
##Read in current selection not data.selection
df_player_stages<-read.csv('player-stages.csv')
##read in players-csv
df_players<-read.csv('players.csv')
#rename to prolific id and playerid
colnames(df_players)[2]<-'prolificId'
colnames(df_players)[1]<-'playerId'
#df_players[df_players$prolificId=='600a74055f76dc1a858efe06','playerId']
#keep only those who finished
df_players<-df_players[df_players$exitStepsDone=='ExitSurvey',1:2]
#Merge
df_player_stages<-merge(df_player_stages,df_players,by.y ='playerId',by.x='playerId' ,no.dups=F)

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

#DROP IF EMPTY
df_player_stages <- df_player_stages[df_player_stages$data.submission != '', ]
df_rounds_fixed<-json_to_columns(df_player_stages,data.submission)

library(anytime)

#read in stages
df_stages<-read.csv('stages.csv')
#merge with rounds
colnames(df_stages)[1]<-'stageId'
df_stages
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
#Get final score(remmber to subtract last score as it duplicates)
df_player_rounds$Final_Score<-df_player_rounds$data.totalScore-df_player_rounds$data.curScore
#keep pertinent data
df_player_rounds<-df_player_rounds[,c('roundId','playerId','data.quiz','data.totalScore','Final_Score')]
#expand column but make sure its not empty
df_player_rounds <- df_player_rounds[df_player_rounds$data.quiz != '', ]

df_player_rounds<-json_to_columns(df_player_rounds,data.quiz)

sum(df_player_rounds[df_player_rounds$playerId=='bnZMAt3N2iYBaE3x6','Final_Score'])

#merge with prolific Ids
df_player_rounds<-merge(df_players,df_player_rounds)

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
##Analyze players we want to analyze
#Get new column that is the submissions
Final_Merger$submission<-as.numeric(sub('Submission |Reflection', '', Final_Merger$name))

#See if reached_maxima
Final_Merger$Reached_Maxima<-0
Final_Merger[Final_Merger$score==.1,]$Reached_Maxima<-1

######NOW subset for the prolific profiles.
################## GET PAYMENT AMOUNT

########Get prolific completers, copy and paste here
##DO not include 9GqhK2QyhAHDLdcPT since they noticed they could do opposite of dot easy starting to reahc the max


Prolific_Completers<-c('5e52af8dd120e7000bc35826','5f1637cf9e2ee97acd002228','5fa628181418d321a560b8b4','5e6322085dfe910b8fd2b4b4','5af611d350008e00018b3bb7','5b9dcb3bd2599000011033a7')
#Prolific_Completers<-c('5e52af8dd120e7000bc35826','5f1637cf9e2ee97acd002228','5fa628181418d321a560b8b4','5e6322085dfe910b8fd2b4b4','5af611d350008e00018b3bb7','5b9dcb3bd2599000011033a7',
  #'5f57a1ac39d0c726ad8ef5ba','5dc8d8b90a0b38630af7a0ba','5fde4827c5f99b5a814e25ff','5f43ef4041bb5f11a27aa07e','5e9389d87d6dc1078fe0ebee','5c3f336bef1d0d0001b2c422','5dd0d2f02b033b190687eaee','5c6babadec1fb80001c18bed','5f1ce0d079caaa5c3da2e9a6','5e6fe86f7245355b33d7b045')

Prolific_Completers<-c('5f0c71b44f61610a1bb31860','5f4a5b3dccae1c5e215cfe3e','5c5e42fb001e3400014509f0','5f7b37b9de9c78096b1a0ab1','5fc150bd290b5717801b5821','6021f6c1f883e82b11739f31',
                       '5f559c6c5d41a496a328a0dc','5d0f8ab43382180019625865','5ff3e87b15b59b9f1c8f3b1e','5929ec666c835800011a31fc','5aa26bda35237b000112e35d','60221ffd6360d82d89b621ce',
                       '5e651a304eb006294c9164ea','59a88f5b321f870001d16d68','5f127bf1c550c71c839454f6','5d13c348b8a57200193442c0','5dcf558d8f45b9095605ec6c','5c8388c440650900136d1a38',
                       '5fcac62f81b544773a597764','5acfc892d9f7470001db04d6','5f57a1ac39d0c726ad8ef5ba','5dc8d8b90a0b38630af7a0ba','5fde4827c5f99b5a814e25ff','5f43ef4041bb5f11a27aa07e','5e9389d87d6dc1078fe0ebee','5c3f336bef1d0d0001b2c422','5dd0d2f02b033b190687eaee','5c6babadec1fb80001c18bed','5f1ce0d079caaa5c3da2e9a6','5e6fe86f7245355b33d7b045')
#'5fc2c8276bcebe503858d619','5ff56789238d4b8774c2dd5e','5a6fa72d9cdd180001775149','5f90869547ff2e159dd64902','55b6a2e5fdf99b350d57360e','5e7d7840f89874402cdb9ebe','572cd18388902e000e1e3fae','5df1bef511999f0ce6a28a05','5d9a09095223b6001809f1c9','5fb58d11a037f00cda417905','6008e6570f44505b2cfae696','5614460c7ffc8a000d811ab2','5fcf53e9196fbf275b2e8ada','5f97020f7a03b7055a8ef470','5bd4afa8654665000102e6db','5b29a05e6d03870001976bdb','57fdfb0456820500011017fc','5f42b557f859ca4b3395dc8b','5f0a9f170ad0d636d5a7d1ca','600a74055f76dc1a858efe06')


#get new 30 landscape completers:
Prolific_Completers<-c('5eb46298a781b12b178f2326','5b619b6c5256470001bcf992','5cf1f164fb05610017de2327','5e606b5b1d34273bfbe0422b','5d29120e4d046f0016a74a49','5bdf4540378c3d000161e102',
'602c2db6166df74c98e017d7','5f3c44ac99a44da7cb3eb601','5e779d262e31442fab225c49','5edbf754e001740d9fdbcd94','5f52c941fc18905c7e8f78fd','5c9acf28ddbaab001610a0ae','5de7d11fa1a3fb7445c7e643','604c3f310b27cd98b7c0c8a1','5a6d3a19e6cc4a0001b6c47a','5eecf20fb465300ddb5bb547','5c3faea48cb6f5000180c64b','5f3d482077a67609607ac05c','5e8fbc4dddd9ff2bfc87a879','5dcc5f2545b8f98fd44ab4d7',
'5b91f3f9d99ca20001ef6d8b','5ca6046dbbb3040012c2ba7d','5dac7604cab234001570fe9e','5f050c222c19677febfe6579','5f2dadfc576b0c0c60879586','5d1d0270601cd7000172942f',
'5efa5ee9526cb014da183efe','5830b22cb232860001b1107a','56797198c3041a0012b64b5e','5cfb5233df7d70001619ca90')

# Get HARD-Dot-NK HARD-NK-Dot piloters
Prolific_Completers<-c('5dd70de43269926aec722f9','5dd70de43269926aec722f9b',
'5efb5648d7ea0e08aa5d1179','5efcf906346ca111efac889f','5f1f23a738fd760dc6c77ec1','5f2c2ba4ddc060087fe211a8',
'5d1e1b45965f7a001bd6508c','5f91fe1f5ed1060f5afb58a7','5d4c7aa8f05d320001760340','57188300a459c1001211a7f2',
'5c83277df2d62e001875cc53','5f4eb642ce413e0ecc88e227','5dc899f7d987355fcb3d6ac7','5e7cf03fea45cb369b402703',
'5c935d881754ba001b25697a','5e8296e802677c9c70b3cd84','5ec4465c16290213a8e30fd5','5c3820ee1ede2b0001155929',
'5f768b607107d413d47b2303','600b162565eb1c0f1351081e','5cab4ce7fc216d0016162830','5f3ca0a3e30eacb376290fe1',
'55a2fa33fdf99b073cd94947','5dde21012a30acd8ada645ec','5f5bac39ce8ed832aa378a53','5f2ddeae9dfd7a132a7f4ae8',
'5ed9dcfebc8c1157ed75fa51','5f7914d33c89b6177249ba51','5f8313c64e8792079f84cd84','5f01f2ec11d6c60698e1956c',
'5ed2c4fe85676325a891de20','5c82263e392ce000163ee09f','5efbdfe2d66b1418c2ba858a','5ce606fb4caf470018da7fb0','5c31fd5c24d16c00015b0bb4',
'5de83840271a1c7b0fb7ec16','60417bd239d30915be3c1b50','5f567931e492b30ae2080059','5d01b9d4c9281200151c7c6b','5b6872f33ffbf200015434ef')



##############RECODE BY USING PLAYER ID FROM PLAYERS>CSV BY TICKING BOX ON EMPIRICA
Aggregated_payouts<-aggregate(df_player_rounds$Final_Score, by=list(prolificId=df_player_rounds$prolificId,df_player_rounds$playerId), FUN=sum)
colnames(Aggregated_payouts)[2:3]<-c('playerId','payout')
#round up
roundUp <- function(x) ceiling((x*100))/100
Aggregated_payouts$payout<-roundUp(Aggregated_payouts$payout)
#subset to completers
Aggregated_payouts<-Aggregated_payouts[Aggregated_payouts$prolificId %in% Prolific_Completers,]
#Aggregated_payouts
write_csv(Aggregated_payouts[Aggregated_payouts$prolificId %in% Prolific_Completers,],'pilot_payouts.csv')
nrow(Aggregated_payouts[Aggregated_payouts$prolificId %in% Prolific_Completers,])
#############################Get Pilot_Tester_DATA by ID

Pilot_Tester_data<-Final_Merger[Final_Merger$prolificId %in% Prolific_Completers,]
length(unique(Pilot_Tester_data$prolificId))
mean(Aggregated_payouts$payout)


save(Pilot_Tester_data,file='Pilot_Tester_Data.Rda')

library("xlsx")
write.xlsx(Pilot_Tester_data, file, sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


#GEt stats by submission
Stats_by_Submission <-Pilot_Tester_data %>% group_by(landscape_type,submission) %>% #replace round_id with landscape type
  summarise(
    avg_score = mean(score),
    avg_step_size1 = mean(Step_Size_1[!is.na(Step_Size_1)]), #include !is.na so I dont include reflections or stages without data
    avg_step_size2 = mean(Step_Size_2[!is.na(Step_Size_2)]),
    avg_score_change1 = mean(Score_Diff_1[!is.na(Score_Diff_1)]),
    avg_score_change2 = mean(Score_Diff_1[!is.na(Score_Diff_2)]),
    time_taken = mean(time_taken[!is.na(time_taken)]),
    Reached_Maxima = mean(Reached_Maxima),
    name=name
  )
#order the data
Pilot_Tester_data <- Pilot_Tester_data[order(Pilot_Tester_data$playerId, Pilot_Tester_data$index),]
model_step_size<-(lm(I((Step_Size_1))~I(lag(Score_Diff_1,1))+I(lag(Score_Diff_1,2))+I(I(lag(Score_Diff_1,1))<0 &I(lag(Score_Diff_1,2))<0)+
                      score+landscape_type+I(log(submission))+I(log(index))
                     +I(lag(score==.1,1)),data=Pilot_Tester_data[Pilot_Tester_data$Reached_Maxima==0,]))

summary(model_step_size)
Pilot_Tester_data$index
library(car)
mean(Pilot_Tester_data$Score_Diff_1,na.rm=T)
linearHypothesis(model_step_size, "0 = landscape_typedot_hard - landscape_typeNK_hard")


mean(Pilot_Tester_data$Score_Diff_1,na.rm = T)
library(ggplot2)
ggplot()
#plot average score
Stats_by_Submission%>% ggplot( aes(x=submission, y=avg_score, group=landscape_type, color=landscape_type)) +
  ggtitle('Average Score')+xlab('Submission')+ylab('Average Score (across players)')+
  geom_line() 

#plot average score change
Stats_by_Submission%>% ggplot( aes(x=submission, y=avg_score_change1, group=landscape_type, color=landscape_type)) +
  ggtitle('Average Score Change')+xlab('Submission')+ylab('Average Score Change (across players)')+
  geom_line() 

#plot proportion at global maximum 
Stats_by_Submission%>% ggplot( aes(x=submission, y=Reached_Maxima, group=landscape_type, color=landscape_type)) +
  ggtitle('Proportion at global maximum')+xlab('Submission')+ylab('Proportion at global maximum')+
  geom_line() 


#plot average step size
Stats_by_Submission%>% ggplot( aes(x=submission, y=avg_step_size1, group=landscape_type, color=landscape_type)) +
  ggtitle('Average Step Size')+xlab('Submission')+ylab('Average Step Size (across players)')+
  geom_line() 

Stats_by_Submission%>% ggplot( aes(x=submission, y=abs(time_taken), group=landscape_type, color=landscape_type)) +
  ggtitle('Average Time Taken')+xlab('Submission')+ylab('Average Time Taken (across players)')+
  geom_line() 

#Pilot_Tester_data$data.feedback[11]

###Get summary Stats by landscape
Stats_By_Landscsape <-Pilot_Tester_data %>% 
  group_by(landscape_type) %>%
  summarise(
    avg_score = mean(score),
    avg_step_size1 = mean(Step_Size_1[!is.na(Step_Size_1)]), #include !is.na so I dont include reflections or stages without data
    avg_step_size2 = mean(Step_Size_2[!is.na(Step_Size_2)]),
    avg_score_change1 = mean(Score_Diff_1[!is.na(Score_Diff_1)]),
    avg_score_change2 = mean(Score_Diff_1[!is.na(Score_Diff_2)]),
    avg_time_taken = mean(time_taken[!is.na(time_taken)]),
    Count_easy   = sum(difficulty[name=='Reflection'] =='easy'),
    Count_hard   = sum(difficulty[name=='Reflection'] =='hard'),
    Count_medium = sum(difficulty[name=='Reflection'] =='med')
  ) 
Stats_By_Landscsape

##Get Feedback
Learn_by_person_type<-Pilot_Tester_data %>% 
  group_by(landscape_type,playerId,prolificId) %>%
  distinct(learn)%>% 
  summarise(
    Learn = learn
    )
#View(Learn_by_person_type)

Feedback_by_person_type<-Pilot_Tester_data %>% 
  group_by(landscape_type,playerId,prolificId) %>%
  distinct(data.feedback)%>% 
  summarise(
    Feedback = data.feedback
  )
#View(Feedback_bxy_person_type)


##Get maxima reachers
Reaches_Maxima<-Pilot_Tester_data %>% 
  group_by(playerId,landscape_type) %>%
  summarise(
    score = max(score),
    landscape_type=landscape_type
  ) 
Pilot_Tester_data$landscape_type
sum(Reaches_Maxima[Reaches_Maxima$score==.1,]$landscape_type=='dot_easy')
sum(Reaches_Maxima[Reaches_Maxima$score==.1,]$landscape_type=='dot_hard')

sum(Pilot_Tester_data[(Pilot_Tester_data$score==.1) ,]$landscape_type=='dot_hard')/sum(Pilot_Tester_data$landscape_type=='dot_easy')
sum(Pilot_Tester_data[(Pilot_Tester_data$score==.1),]$landscape_type=='dot_easy')/sum(Pilot_Tester_data$landscape_type=='dot_hard')

sum(Pilot_Tester_data$landscape_type=='dot_easy')
sum(Pilot_Tester_data$landscape_type=='dot_hard')


#View(Pilot_Tester_data[Pilot_Tester_data$playerId=='9GqhK2QyhAHDLdcPT' & Pilot_Tester_data$landscape_type=='dot_easy',])



Landscapes_types<-Pilot_Tester_data %>% 
  group_by(landscape_type) %>%
  summarise(
    task3=sum(condition=='task3')/26,
    task2=sum(condition=='task2')/26,
    task1=sum(condition=='task1')/26
    
  ) 
Landscapes_types

#View(Pilot_Tester_data)
