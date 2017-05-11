data = read.table("ggevent.log", sep = ",")
colnames(data) = c("ai5", "debug", "random", "sdkv", "event", "ts", "params", "timestamp", "game_id")
data$ts = gsub(" ts: ", "", data$ts)
data$ts = gsub("}", "", data$ts)
data$ai5 = gsub(".headers: .ai5: ", "", data$ai5)
data$debug = gsub("debug: ", "", data$debug)
data$random = gsub("random: ", "", data$random)
data$sdkv = gsub("sdkv: ", "", data$sdkv)
data$event = gsub("post: .event: ", "", data$event)
data$params = gsub("params: ..", " ", data$params)
data$timestamp = gsub("bottle: .timestamp: ", " ", data$timestamp)
data$game_id = gsub("game_id: ", " ", data$game_id)
data$sdkv = gsub("}", "", data$sdkv)
data$game_id = gsub("}}", " ", data$game_id)
data$sdkv = as.factor(data$sdkv)
data$game_id = as.factor(data$game_id)
data$ts = as.numeric(data$ts)
data$event = as.factor(data$event)
data$timestamp = gsub("  ", "", data$timestamp)
data$timestamp = strptime(data$timestamp, format = "%Y-%m-%d %H:%M:%OS")
options(digits.secs = 6)
data$ai5 = as.factor(data$ai5)

result = c("Game ID", "Number of Sessions", "Number of Valid Sessions", "Average valid session time")

for(k in 1:length(table(data$game_id))){
  a = subset(data, data$game_id == levels(data$game_id)[k]) 
  sessions = 0
  valid = 0
  session_time = 0
  for(i in 1:length(table(a$ai5))){
    aj = subset(a, a$ai5 == levels(data$ai5)[i])
    if (nrow(aj) > 1){
    for(j in 1:(nrow(aj)-1)){
      if(aj[j,5] == "ggstart"  & aj[j+1,5] == "ggstop"){
        if(1 <(aj[j+1,8] - aj[j,8]) & (aj[j+1,8] - aj[j,8]) < 60)
          sessions = sessions + 1
        else if((aj[j+1,8] - aj[j,8]) > 60){
          valid = valid + 1
          session_time = aj[j+1,8] - aj[j,8] + session_time
        }
      }
      else if(aj[j,5] == "ggstop" & aj[j+1,5] == "ggstart"){
        if((aj[j,8] - aj[j+1,8]) < 30)
          sessions = sessions -1
      }
    }
  result = rbind(result, c(as.character(levels(data$game_id[k]),  sessions,valid, session_time/valid)))
    }
  }
}