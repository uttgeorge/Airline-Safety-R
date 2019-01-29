#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#load data and libriry
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(plotly)
library(ggplot2)
require(gganimate)
require(dplyr)
library(corrplot)
require(gridExtra)
airline.safety <- read.csv("/Users/QiJin/Downloads/airline-safety-2.csv")
#index <- which(airline.safety$incidents_85_99 >= 1)
index <- which(airline.safety$fatal_accidents_00_14 >= 1)
attach(airline.safety)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#define data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#set data frame of 1985 - 1999 airline 
incidents_85_99_model <- airline.safety$incidents_85_99[index]
fatalities_85_99_model <- airline.safety$fatalities_85_99[index]
airseat_85_99_model <- airline.safety$avail_seat_km_per_week[index]
airline_85_99_model <- airline.safety$airline[index]
fatal_accidents_85_99_model <- airline.safety$fatal_accidents_85_99[index]
# year_model <- airline.safety$year[index1]
year_85_99_model <- c(1:length(index))
year_85_99_model[1:length(index)] = 1985
airsafe_85_99_model<-data.frame(airline_85_99_model,
                                incidents_85_99_model,
                                fatal_accidents_85_99_model,
                                fatalities_85_99_model,
                                airseat_85_99_model,
                                year_85_99_model)

#set data frame of 2000 - 2014 airline
incidents_00_14_model <- airline.safety$incidents_00_14[index]
fatalities_00_14_model <- airline.safety$fatalities_00_14[index]
airseat_00_14_model <- airline.safety$avail_seat_km_per_week[index]
airline_00_14_model <- airline.safety$airline[index]
fatal_accidents_00_14_model <- airline.safety$fatal_accidents_00_14[index]
# year_model <- airline.safety$year[index1]
year_00_14_model <- c(1:length(index))
year_00_14_model[1:length(index)] = 2014
airsafe_00_14_model<-data.frame(airline_00_14_model,
                                incidents_00_14_model,
                                fatal_accidents_00_14_model,
                                fatalities_00_14_model,
                                airseat_00_14_model,
                                year_00_14_model)

#set data frame of 1985 - 2014 airline
incidents_all_model <- c(incidents_85_99_model,incidents_00_14_model)
fatalities_all_model <- c(fatalities_85_99_model,fatalities_00_14_model)
fatal_accidents_all_model <- c(fatal_accidents_85_99_model,fatal_accidents_00_14_model)
airseat_all_model <- c(airseat_85_99_model,airseat_00_14_model)
airline_all_model <- paste(airline_85_99_model)
# year_model <- airline.safety$year[index1]
year_all_model = c(year_85_99_model,year_00_14_model)
airsafe_all_model<-data.frame( airline_all_model,
                               incidents_all_model,
                               fatal_accidents_all_model,
                               fatalities_all_model,
                               airseat_all_model,
                               year_all_model)

airsafe_all_model <- data.frame(airsafe_all_model)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Part 1 pie chart

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
w<-table(airline.safety$countries)
t<-as.data.frame(w)
t
#China=3,US=7,UK=3,Germany=2
slices <- c(2, 3, 7, 3, 41) 
lbls <- c("Germany", "UK", "US", "China", "Other Country/Region \nonly have one airline")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls
    ,main="Airlines")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# this is the bar chart of incidents &fatal accidents& fatalities

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# no. 1
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
airline <- factor(airline,
                  levels = airline[order(incidents_85_99)])
testA <- data.frame(airline,incidents=incidents_00_14, year=2014)

testB <- data.frame(airline,incidents=incidents_85_99, year=1985)
test = bind_rows(testA,testB)
test$year = factor(test$year)
ggplot(test, aes(factor(airline), incidents, fill = year)) + 
  geom_bar(stat="identity", position = "dodge2") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#no.2
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
airline <- factor(airline,
                  levels = airline[order(fatal_accidents_85_99)])
testA <- data.frame(airline,fatals=fatal_accidents_00_14, year=2014)

testB <- data.frame(airline,fatals=fatal_accidents_85_99, year=1985)
test = bind_rows(testA,testB)
test$year = factor(test$year)
ggplot(test, aes(factor(airline), fatals, fill = year)) + 
  geom_bar(stat="identity", position = "dodge2") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#no.3
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
airline <- factor(airline,
                  levels = airline[order(fatalities_85_99)])
testA <- data.frame(airline,fatalities=fatalities_00_14, year=2014)

testB <- data.frame(airline,fatalities=fatalities_85_99, year=1985)
test = bind_rows(testA,testB)
test$year = factor(test$year)
ggplot(test, aes(factor(airline), fatalities, fill = year)) + 
  geom_bar(stat="identity", position = "dodge2") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#plot them
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# grid.arrange(a1,a2,a3,nrow=1)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# correlation
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
par(mfrow=c(2,2))
airlineCor <- cor(airline.safety[2:8])
corrplot(airlineCor, method = "ellipse")

airlineCor_85_99 <- cor(airline.safety[2:5])
corrplot(airlineCor_85_99, method = "ellipse")

airlineCor_00_14 <- cor(airline.safety[c(2,6,7,8)])
corrplot(airlineCor_00_14, method = "ellipse")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#This the animated chart
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ggplot(airsafe_all_model, aes(x= airsafe_all_model$fatal_accidents_all_model,
                              y= airsafe_all_model$fatalities_all_model,
                              size = (airsafe_all_model$fatalities_all_model)^1.2,
                              colour = airsafe_all_model$airline_all_model))+
  geom_point(alpha = 0.7, show.legend = FALSE)+
  scale_colour_manual(values = as.numeric(airsafe_all_model$airline_all_model)) +
  scale_size(range = c(2, 20)) +
  scale_x_log10()+
  geom_text( label = airsafe_all_model$airline_all_model,
             show.legend = F,
             size = 4,
             col = "black")+
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Fatal Accidents', y = 'fatalities') +
  transition_time(year_all_model) +
  ease_aes('linear')+
  assign("last.warning", NULL, envir = baseenv())


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# animated box chart
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

p1<-ggplot(airsafe_all_model, aes(1,airsafe_all_model$fatalities_all_model)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    airsafe_all_model$year_all_model,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

p2<-ggplot(airsafe_all_model, aes(1,airsafe_all_model$fatal_accidents_all_model)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    airsafe_all_model$year_all_model,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  scale_y_log10()+
  ease_aes('sine-in-out')

p3<-ggplot(airsafe_all_model, aes(1,airsafe_all_model$incidents_all_model)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    airsafe_all_model$year_all_model,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# interactive box chart
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

testA <- data.frame(incidents=incidents_00_14, year=2014)

testB <- data.frame(incidents=incidents_85_99, year=1985)
test = bind_rows(testA,testB)
test$year = factor(test$year)
plot_ly(test, x = ~test$incidents, 
        color = ~test$year, 
        type = "box")

testA <- data.frame(fatal_accidents=fatal_accidents_00_14, year=2014)

testB <- data.frame(fatal_accidents=fatal_accidents_85_99, year=1985)
test = bind_rows(testA,testB)
test$year = factor(test$year)
plot_ly(test, x = ~test$fatal_accidents, 
        color = ~test$year, 
        type = "box")

testA <- data.frame(fatalities=fatalities_00_14, year=2014)

testB <- data.frame(fatalities=fatalities_85_99, year=1985)
test = bind_rows(testA,testB)
test$year = factor(test$year)
plot_ly(test, x = ~test$fatalities, 
        color = ~test$year, 
        type = "box")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# interactive bubble chart
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


q<-ggplot(airsafe_all_model, aes(x=airsafe_all_model$fatal_accidents_all_model,
                                 y= airsafe_all_model$fatalities_all_model,
                                 size = airsafe_all_model$fatalities_all_model,
                                 colour = airsafe_all_model$airline_all_model))+
  geom_point(aes(size = airsafe_all_model$fatalities_all_model,
                 frame = airsafe_all_model$year_all_model, 
                 ids = airsafe_all_model$incidents_all_model),
             show.legend =T)+
  xlab("Fatal Accidents")+
  ylab("Fatalities")
ggplotly(q)



