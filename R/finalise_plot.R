library(readr)
library(ggplot2)

myfile <- 'https://raw.githubusercontent.com/simon-kingston/2019-FIA-f1-World-Championship/master/data/2019-f1-results.csv'

f1_data <- read_csv(myfile)

drivers <- unique(f1_data$Driver)
teams <- sort(unique(f1_data$Team))
score_system <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

omit <- function(race){
  c(na.omit(race))
}

raw_driver_results <- matrix(c(omit(f1_data$AUS), omit(f1_data$BHR), omit(f1_data$CHN), omit(f1_data$AZE), omit(f1_data$ESP), omit(f1_data$MON), omit(f1_data$CAN), omit(f1_data$FRA), omit(f1_data$AUT),  omit(f1_data$GBR), 
                               omit(f1_data$GER), omit(f1_data$HUN), omit(f1_data$BEL), omit(f1_data$ITA), omit(f1_data$SIN), omit(f1_data$RUS), omit(f1_data$JPN), omit(f1_data$MEX), omit(f1_data$USA), omit(f1_data$BRA), omit(f1_data$ABU)), nrow = 21, ncol = 20, byrow = TRUE)



is_Fastest_Lap <- function(position){
  grepl('F', position)
}

parse_int <- function(position){
  to_int(gsub('[a-zA-Z ]', '', position))
}

fastest_lap <- function(driver_position, driver_points, score_system, fastest_lap_point){
  f_points <- driver_points
  if (driver_position <  11 && !is.na(driver_position)){
    f_points <- f_points + score_system[driver_position] + fastest_lap_point
  }
  f_points
}


to_int <- function(a){
  as.integer(a)
}

dnf <- function(a){
  ifelse(a%in%c('DSQ', 'RET'), TRUE, FALSE)
}

update_points <- function(data_set, rows, score_system){
  points <- numeric(20)
  
  fastest_lap_point <- 1
  
  for(row in 1:rows) {
    
    for (id in 1:20) {
      if (dnf(data_set[row, id])){

      } else if (is_Fastest_Lap(data_set[row, id])) {
        # FASTEST LAP
        points[id] <- fastest_lap(parse_int(data_set[row, id]), points[id],
                                  score_system, fastest_lap_point)
        
      } else if (!is.na(to_int(data_set[row, id]))) {
        # Regular Points
        points[id] <- points[id] + score_system[to_int(data_set[row, id])]
        
      } 
      
    }
  }
  
  points
}

r1 = update_points(raw_driver_results, 1, score_system)
r2 = update_points(raw_driver_results, 2, score_system)
r3 = update_points(raw_driver_results, 3, score_system)
r4 = update_points(raw_driver_results, 4, score_system)
r5 = update_points(raw_driver_results, 5, score_system)
r6 = update_points(raw_driver_results, 6, score_system)
r7 = update_points(raw_driver_results, 7, score_system)
r8 = update_points(raw_driver_results, 8, score_system)
r9 = update_points(raw_driver_results, 9, score_system)
r10 = update_points(raw_driver_results, 10, score_system)
r11 = update_points(raw_driver_results, 11, score_system)
r12 = update_points(raw_driver_results, 12, score_system)
r13 = update_points(raw_driver_results, 13, score_system)
r14 = update_points(raw_driver_results, 14, score_system)
r15 = update_points(raw_driver_results, 15, score_system)
r16 = update_points(raw_driver_results, 16, score_system)
r17 = update_points(raw_driver_results, 17, score_system)
r18 = update_points(raw_driver_results, 18, score_system)
r19 = update_points(raw_driver_results, 19, score_system)
r20 = update_points(raw_driver_results, 20, score_system)
r21 = update_points(raw_driver_results, 21, score_system)

races <- factor(names(f1_data)[3:23], levels = names(f1_data)[3:23])
df1 <- data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21)

var0 <- as.numeric(df1[1, ])
var1 <- as.numeric(df1[2, ])

ggplot() + 
  geom_line(aes(x=races,y=as.numeric(df1[1, ])), color='cyan', group = 1) + # HAM
  geom_line(aes(x=races,y=as.numeric(df1[2, ])), color='cyan', group = 1) + # BOT
  geom_line(aes(x=races,y=as.numeric(df1[3, ])), color='blue', group = 1) + # VER
  geom_line(aes(x=races,y=as.numeric(df1[4, ])), color='red', group = 1) + # LEC
  geom_line(aes(x=races,y=as.numeric(df1[5, ])), color='red', group = 1) + # VET
  geom_line(aes(x=races,y=as.numeric(df1[6, ])), color='orange', group = 1) + # SAI
  geom_line(aes(x=races,y=as.numeric(df1[7, ])), color='light blue', group = 1) + # GAS
  geom_line(aes(x=races,y=as.numeric(df1[8, ])), color='blue', group = 1) + # ALB
  geom_line(aes(x=races,y=as.numeric(df1[9, ])), color='yellow', group = 1) + # RIC
  geom_line(aes(x=races,y=as.numeric(df1[10, ])), color='pink', group = 1) + # PER
  geom_line(aes(x=races,y=as.numeric(df1[11, ])), color='orange', group = 1) + # NOR
  geom_line(aes(x=races,y=as.numeric(df1[12, ])), color='dark red', group = 1) + # RAI
  geom_line(aes(x=races,y=as.numeric(df1[13, ])), color='light blue', group = 1) + # KYV
  geom_line(aes(x=races,y=as.numeric(df1[14, ])), color='yellow', group = 1) + # HUL
  geom_line(aes(x=races,y=as.numeric(df1[15, ])), color='gold', group = 1) + # MAG
  geom_line(aes(x=races,y=as.numeric(df1[16, ])), color='pink', group = 1) + # STR
  geom_line(aes(x=races,y=as.numeric(df1[17, ])), color='dark red', group = 1) + # GIO
  geom_line(aes(x=races,y=as.numeric(df1[18, ])), color='gold', group = 1) + # # GRO
  geom_line(aes(x=races,y=as.numeric(df1[19, ])), color='grey', group = 1) + # KUB
  geom_line(aes(x=races,y=as.numeric(df1[20, ])), color='grey', group = 1) + # RUS
  ylab('Points')+xlab('Race')+ggtitle('2019 F1 Driver Championship') +
  theme(plot.title = element_text(hjust = 0.5))

ss <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 1)
ss_header <- c('1st', '2nd', '3rd', '4th', '5th', '6th', '7th', '8th', '9th', '10th', 'Fastest Lap')
df_ss <- data.frame(position=factor(ss_header, levels=ss_header), points=ss)

ggplot(data=df_ss, aes(x=position, y=points))+geom_bar(stat='identity',fill="red",color='white') + 
ylab('Points Awarded')+xlab('Position')+ggtitle('Scoring System')+theme(plot.title = element_text(hjust = 0.5))

 

