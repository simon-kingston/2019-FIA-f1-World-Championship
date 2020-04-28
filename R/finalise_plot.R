library(readr)
library(ggplot2)
library(plotly)

omit <- function(race){
  c(na.omit(race))
}

is_Fastest_Lap <- function(position){
  grepl('F', position)
}

parse_int <- function(chr){
  to_int(gsub('[a-zA-Z ]', '', chr))
}

fastest_lap <- function(position, points, score_system, fastest_lap_point){
  ifelse(position<=10 && !is.na(position), points+score_system[position]+fastest_lap_point, points)
}

to_int <- function(value){
  as.integer(value)
}

isDNF <- function(result){
  ifelse(result %in% c('DSQ', 'RET'), TRUE, FALSE)
}

myfile <- 'https://raw.githubusercontent.com/simon-kingston/2019-FIA-f1-World-Championship/master/data/2019-f1-results.csv'
f1_data <- read_csv(myfile)
drivers <- unique(f1_data$Driver)
teams <- sort(unique(f1_data$Team))
# 11th score is the fastest driver bonus point
score_system <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 1) 


race_standings <- matrix(c(omit(f1_data$AUS), omit(f1_data$BHR), omit(f1_data$CHN), omit(f1_data$AZE), omit(f1_data$ESP), omit(f1_data$MON), omit(f1_data$CAN), omit(f1_data$FRA), omit(f1_data$AUT),  omit(f1_data$GBR), 
                               omit(f1_data$GER), omit(f1_data$HUN), omit(f1_data$BEL), omit(f1_data$ITA), omit(f1_data$SIN), omit(f1_data$RUS), omit(f1_data$JPN), omit(f1_data$MEX), omit(f1_data$USA), omit(f1_data$BRA), omit(f1_data$ABU)), nrow = 21, ncol = 20, byrow = TRUE)



update_points <- function(data_set, row, score_system){
  points <- numeric(20)
  for (id in 1:20) {
    if (!isDNF(data_set[row, id])){
      if (is_Fastest_Lap(data_set[row, id])) {
        # FASTEST LAP
        points[id] <- fastest_lap(parse_int(data_set[row, id]), points[id],
                                  score_system, score_system[11])
        
      } else if (!is.na(to_int(data_set[row, id])) && to_int(data_set[row, id]) <= 10) {
        # Regular Points
        points[id] <- points[id] + score_system[to_int(data_set[row, id])]
      }  
    }  
  }
  
  points
}

driver_results <- function(race_standings, score_system){
  df1 <- data.frame(update_points(race_standings, 1, score_system))
  for (id in 2:21){
    df1 <- cbind(df1, update_points(race_standings, id, score_system))
  }
  df1
}

df1 <- driver_results(race_standings, score_system)
races <- factor(names(f1_data)[3:23], levels = names(f1_data)[3:23])
team_colours <- c('cyan', 'blue', 'red', 'orange', 'light blue', 'yellow', 'pink', 'dark red', 'gold', 'grey')

p1 <- ggplot() + 
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[1, ]))), color=team_colours[1], group = 1)  +# HAM
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[2, ]))), color=team_colours[1], group = 2) + # BOT
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[3, ]))), color=team_colours[2], group = 3) + # VER
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[4, ]))), color=team_colours[3], group = 4) + # LEC
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[5, ]))), color=team_colours[3], group = 5) + # VET
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[6, ]))), color=team_colours[4], group = 6) + # SAI
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[7, ]))), color=team_colours[5], group = 7, linetype=4) + # GAS
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[8, ]))), color=team_colours[2], group = 8, linetype=4) + # ALB
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[9, ]))), color=team_colours[6], group = 9) + # RIC
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[10, ]))), color=team_colours[7], group = 10) + # PER
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[11, ]))), color=team_colours[4], group = 11) + # NOR
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[12, ]))), color=team_colours[8], group = 12) + # RAI
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[13, ]))), color=team_colours[5], group = 13) + # KYV
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[14, ]))), color=team_colours[6], group = 14) + # HUL
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[15, ]))), color=team_colours[9], group = 15) + # MAG
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[16, ]))), color=team_colours[7], group = 16) + # STR
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[17, ]))), color=team_colours[8], group = 17) + # GIO
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[18, ]))), color=team_colours[9], group = 18) + # # GRO
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[19, ]))), color=team_colours[10], group = 19) + # KUB
  geom_line(aes(x=races,y=cumsum(as.numeric(df1[20, ]))), color=team_colours[10], group = 20) + # RUS
  ylab('Points')+xlab('Race')+ggtitle('2019 F1 Drivers Championship') +
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw()

ss_header <- c('1st', '2nd', '3rd', '4th', '5th', '6th', '7th', '8th', '9th', '10th', 'Fastest Lap')
df_ss <- data.frame(position=factor(ss_header, levels=ss_header), points=score_system)

p2 <- ggplot(data=df_ss, aes(x=position, y=points))+geom_bar(stat='identity',fill="red",color='white') + 
ylab('Points Awarded')+xlab('Position')+ggtitle('Scoring System')+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()

# Drivers Championship
fig1 <- ggplotly(p1)
fig1

# 2019 Scoring System
fig2 <- ggplotly(p2)
fig2 

# Constructors Championship

# aus_construstors <- c(f1_data$Team[complete.cases(f1_data$AUS)], f1_data$AUS[complete.cases(f1_data$AUS)])
# bel_construstors <- c(f1_data$Team[complete.cases(f1_data$BEL)], f1_data$BEL[complete.cases(f1_data$BEL)])

# constructors_results_table <-  data.frame(teams, AUS = r1, BHR = r2, CHN = r3, AZE = r4, ESP = r5, MON = r6, CAN = r7, FRA = r8,
#                                           AUT = r9, GBR = r10, GER = r11, HUN = r12, BEL = r13, ITA = r14, SIN = r15, RUS = r16, JPN = r17, MEX = r18, USA = r19, BRA = r20, ABU = r21)