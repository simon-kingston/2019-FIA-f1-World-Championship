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

fastest_lap <- function(position, score_system){
  fastest_lap_point <- score_system[11]
  ifelse(position<=10 && !is.na(position), score_system[position]+fastest_lap_point, 0)
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
teams <- f1_data$Team
# 11th score is the fastest driver bonus point
score_system <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 1) 

driver_standings <- matrix(c(omit(f1_data$AUS), omit(f1_data$BHR), omit(f1_data$CHN), omit(f1_data$AZE), omit(f1_data$ESP), 
                             omit(f1_data$MON), omit(f1_data$CAN), omit(f1_data$FRA), omit(f1_data$AUT),  omit(f1_data$GBR), 
                             omit(f1_data$GER), omit(f1_data$HUN), omit(f1_data$BEL), omit(f1_data$ITA), omit(f1_data$SIN), 
                             omit(f1_data$RUS), omit(f1_data$JPN), omit(f1_data$MEX), omit(f1_data$USA), omit(f1_data$BRA), 
                             omit(f1_data$ABU)), nrow = 21, ncol = 20, byrow = TRUE)

constructor_standings <- matrix(c(cad(teams, f1_data$AUS), cad(teams, f1_data$BHR), cad(teams, f1_data$CHN), cad(teams, f1_data$AZE),
                                  cad(teams, f1_data$ESP), cad(teams, f1_data$MON), cad(teams, f1_data$CAN), cad(teams, f1_data$FRA), cad(teams, f1_data$AUT),
                                  cad(teams, f1_data$GBR), cad(teams, f1_data$GER), cad(teams, f1_data$HUN), cad(teams, f1_data$BEL), cad(teams, f1_data$ITA),
                                  cad(teams, f1_data$SIN), cad(teams, f1_data$RUS), cad(teams, f1_data$JPN), cad(teams, f1_data$MEX), cad(teams, f1_data$USA),
                                  cad(teams, f1_data$BRA), cad(teams, f1_data$ABU)), nrow = 21, ncol = 40, byrow = TRUE)



update_driver_points <- function(data_set, row, score_system){
  points <- numeric(20)
  for (id in 1:20) {
    if (!isDNF(data_set[row, id])){
      if (is_Fastest_Lap(data_set[row, id])) {
        # Fastest Lap
        points[id] <- points[id] + fastest_lap(parse_int(data_set[row, id]), score_system)
        
      } else if (!is.na(to_int(data_set[row, id])) && to_int(data_set[row, id]) <= 10) {
        # Regular Points
        points[id] <- points[id] + score_system[to_int(data_set[row, id])]
      }  
    }  
  }
  
  points
}

driver_results <- function(driver_standings, score_system){
  df1 <- data.frame(update_driver_points(driver_standings, 1, score_system))
  for (id in 2:21){
    df1 <- cbind(df1, update_driver_points(driver_standings, id, score_system))
  }
  df1
}

df1 <- driver_results(driver_standings, score_system)
races <- factor(names(f1_data)[3:23], levels = names(f1_data)[3:23])
driver_colours <- c('cyan', 'cyan', 'blue', 'red', 'red', 'orange', 'light blue', 'blue', 'yellow', 'pink', 'orange', 'dark red', 'light blue', 'yellow', 'gold', 'pink', 'dark red', 'gold', 'grey', 'grey')

p1 <- ggplot() + ylab('Points')+xlab('Race')+ggtitle('2019 F1 Drivers Championship') +
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw()

for (i in 1:20) {
  if (i == 7 || i == 8)
    p1 <- p1 + geom_line(aes_string(x=races,y=cumsum(as.numeric(df1[i, ]))), color=driver_colours[i], group = i, linetype=4)
  else
    p1 <- p1 + geom_line(aes_string(x=races,y=cumsum(as.numeric(df1[i, ]))), color=driver_colours[i], group = i) 
}
# TODO - Add driver names to plot

ss_header <- c('1st', '2nd', '3rd', '4th', '5th', '6th', '7th', '8th', '9th', '10th', 'Fastest Lap')
df_ss <- data.frame(position=factor(ss_header, levels=ss_header), points=score_system)

p2 <- ggplot(data=df_ss, aes(x=position, y=points))+geom_bar(stat='identity',fill="red",color='white') + 
  ylab('Points Awarded')+xlab('Position')+ggtitle('Scoring System')+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()


cad <- function(team, race){
  c(team[complete.cases(race)], race[complete.cases(race)])
}


award_constructor_points <- function(points, team_name, points_awarded){
  finishing_order <- c('Mercedes', 'Ferrari', 'Red Bull', 'McLaren', 'Renault', 'Toro Rosso', 'Racing Point', 'Alfa Romeo', 'Haas', 'Williams')

  for (id in 1:10){
    if ( team_name %in% c(finishing_order[id])){
      points[id] <- points[id] + points_awarded
    }
  }

  points
}

update_constructor_points <- function(data_set, row, score_system){
  points <- numeric(10)
  for (id in 1:20) {
    dId <- id + 20
    if (!isDNF(data_set[row, dId])){
      if (is_Fastest_Lap(data_set[row, dId])) {
        # Fastest Lap
        pointsToAdd <- fastest_lap(parse_int(data_set[row, dId]), score_system)
        points <- award_constructor_points(points, data_set[row, id], pointsToAdd)      

      } else if (!is.na(to_int(data_set[row, dId])) && to_int(data_set[row, dId]) <= 10) {
        # Regular Points
        pointsToAdd <- score_system[to_int(data_set[row, dId])]
        points <- award_constructor_points(points, data_set[row, id], pointsToAdd)

      }  
    }  
  }
  
  points
}

# Constructors Championship
constructor_results <- function(constructor_standings, score_system){
  df3 <- data.frame(update_constructor_points(constructor_standings, 1, score_system))
  for (id in 2:21){
    df3 <- cbind(df3, update_constructor_points(constructor_standings, id, score_system))
  }
  df3
}

df3 <- constructor_results(constructor_standings, score_system)
team_colours <- c('cyan', 'red', 'blue', 'orange', 'yellow', 'light blue', 'pink', 'dark red', 'gold', 'grey')
p3 <- ggplot() + ylab('Points')+xlab('Race')+ggtitle('2019 F1 Constructors Championship') +
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw()
for (i in 1:20) {
  p3 <- p3 + geom_line(aes_string(x=races,y=cumsum(as.numeric(df3[i, ]))), color=team_colours[i], group = i)
}
# TODO - Add constructor names to plot

# 2019 Drivers Championship
fig1 <- ggplotly(p1)
fig1

# 2019 Scoring System
fig2 <- ggplotly(p2)
fig2 

# 2019 Constructors Championship
fig3 <- ggplotly(p3)
fig3

