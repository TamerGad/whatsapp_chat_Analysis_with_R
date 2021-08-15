library(rwhatsapp)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggthemes)



chats <- rwa_read('WhatsApp Chat.txt')

head(chats)
dim(chats)


colnames(chats)

# Preparing the Data frame columns which will be used in plotting.

chats <- chats %>%
  mutate(message_date = date(time)) %>%
  mutate(message_month = month(time, label = TRUE)) %>%
  mutate(message_month = factor(message_month)) %>%
  mutate(message_weekday_number = wday(message_date)) %>%
  mutate(message_weekday_name = weekdays(message_date)) %>%
  mutate(message_weekday_name = factor(message_weekday_name)) %>%
  mutate(message_hour = hour(time)) %>%
  mutate(message_year = year(time)) %>%
  filter(message_year == 2021) %>%
  filter(!is.na(author))

head(chats)  

# Plot WhatsApp Messages Per Day

chats %>%
  group_by(message_month) %>%
  count(message_date) %>%
  ggplot(aes(x = message_date, y = n, fill = message_month)) +
  geom_bar(stat = 'identity') + 
  scale_fill_viridis(discrete = T) +
  labs(x = "Month", y = "Number of Messages", fill = "Month") +
  ggtitle("WhatsApp Messages Per Day","Frequency of Messages \nGrouped by Month") +
  theme_minimal() +
  theme(legend.title = element_text(color = 'blue', size = 9, face = 'bold'),
        legend.text = element_text(color = 'red', size = 7),
        legend.position = 'bottom',
        legend.key.size = unit(0.4,'cm'),
        legend.key.width = unit(0.4,'cm'),
        axis.text.x = element_text(size = 7, colour = 'red'),
        axis.text.y = element_text(size = 7, colour = 'red'),
        axis.title = element_text(size = 10, colour = 'blue'),
        plot.title = element_text(size = 18, color = "blue"),
        plot.subtitle = element_text( color = "blue"))

chats_temp <-  chats %>%
                group_by(message_month) %>%
                count(message_date)
chats_temp[chats_temp$n == max(chats_temp$n),]$message_date 

##Plotting the second graph ( Number of WhatsApp Messages per weekday)

chats %>%
  group_by(message_month,message_weekday_number,message_weekday_name) %>%
  count() %>%
  ggplot(aes(x = reorder(message_weekday_name, -message_weekday_number), y = n, fill = message_month  )) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(labels = c("Sunday" = "Sun",
                              "Monday" = "Mon",
                              "Tuesday" = "Tue",
                              "Wednesday" = "Wed",
                              "Thursday" = "Thu",
                              "Friday" = "Fri",
                              "Saturday" = "Sat")) +
  
  labs (x = "Week Day", y = "Messages Per Week Day", fill = "Month") +
  coord_flip() +
  ggtitle("WhatsApp Messages Per Week Day", "Number of Messages Grouped by Month") +
  theme_minimal() +
  theme(legend.title = element_text(color = "blue", size = 12, face = "bold"),
        legend.text = element_text(color = 'red', size = 9 ),
        legend.position = "bottom",
        axis.title = element_text(colour = 'red', size = 10),
        axis.text = element_text(colour = 'blue', size = 13),
        plot.title = element_text(color = 'red',size = 18),
        plot.subtitle = element_text(colour = 'red'),
        plot.margin = unit(c(1,1,1,1), 'cm'),
        plot.background = element_rect( fill = 'grey' ,color = 264653))

  




  





  