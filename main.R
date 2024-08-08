library(DaisTheme)
library(ggplot2)
library(stringr)
library(data.table)
library(dplyr)
library(ggrepel)

graph.spreadsheet <- fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Graph_spreadsheet.csv")

figure.1.data <- fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_1.csv")
figure.1.data[,Measure:=str_remove(Measure,"%")][,Measure:=as.numeric(Measure)]
figure.1.data[,Frequency:=as.factor(Frequency)][,Frequency:=reorder(Frequency,c(rep(7,17),rep(6,17),rep(5,17),rep(4,17),rep(3,17),rep(2,17),rep(1,17)))]
figure.1.data$Platform <- factor(figure.1.data$Platform, levels=rev(c("YouTube",
                                                    "Facebook",
                                                    "Facebook Messenger",
                                                    "Instagram",
                                                    "Pinterest",
                                                    "WhatsApp",
                                                    "LinkedIn",
                                                    "TikTok",
                                                    "Reddit",
                                                    "Twitter / X",
                                                    "Snapchat",
                                                    "ChatGPT",
                                                    "Discord",
                                                    "Microsoft Copilot",
                                                    "Telegram",
                                                    "Threads",
                                                    "WeChat / Weixin"
)))
figure.1 <- plot.column.dais(figure.1.data,
                             Measure, Platform,group.by=Frequency, stacked=TRUE,
                             label.unit = "%",
                             ) + 
  scale_fill_manual(values=c("A few times an hour"="#dd347a",
                             "A few times a day"="#e35892",
                             "A few times a week"="#e66b9e",
                             "A few times a month"="#ec90b6",
                             "A few times a year"="#f2b5ce",
                             "I don’t use this"="black",
                             "Unsure"="grey"))+
  coord_flip() + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Measure, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())
  

figure.2.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_2.csv")
figure.2.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.2.data[,Year:=as.character(Year)][,Year:=as.factor(Year)][,Year:=reorder(Year,c(rep(1,11),rep(2,11),rep(3,11)))]
figure.2.data$Platform <- factor(figure.2.data$Platform, levels=(c("YouTube",
                                                                      "Facebook",
                                                                      "Facebook Messenger",
                                                                      "Instagram",
                                                                      "Pinterest",
                                                                      "WhatsApp",
                                                                      "LinkedIn",
                                                                      "TikTok",
                                                                      "Reddit",
                                                                      "Twitter / X",
                                                                      "Snapchat",
                                                                      "ChatGPT",
                                                                      "Discord",
                                                                      "Microsoft Copilot",
                                                                      "Telegram",
                                                                      "Threads",
                                                                      "WeChat / Weixin"
)))
figure.2 <- plot.column.dais(figure.2.data,
                             Share, Platform, group.by=Year,
                             label.unit = "%") +
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,100), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_dodge(width = 0.6),
            vjust = -0.5)
  


figure.3.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_3.csv")
figure.3.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.3.data[,Age:=as.factor(Age)][,Age:=reorder(Age,c(rep(1,17),rep(2,17),rep(3,17),rep(4,17)))]
figure.3.data$Platform <- factor(figure.3.data$Platform, levels=(c("YouTube",
                                                                   "Facebook",
                                                                   "Facebook Messenger",
                                                                   "Instagram",
                                                                   "Pinterest",
                                                                   "WhatsApp",
                                                                   "LinkedIn",
                                                                   "TikTok",
                                                                   "Reddit",
                                                                   "Twitter / X",
                                                                   "Snapchat",
                                                                   "ChatGPT",
                                                                   "Discord",
                                                                   "Microsoft Copilot",
                                                                   "Telegram",
                                                                   "Threads",
                                                                   "WeChat/Weixin"
)))
figure.3 <- plot.column.dais(figure.3.data,
                             Share,Platform, group.by=Age,stacked=FALSE,order.bar="descending",
                             label.unit="%") +
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,100), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_dodge(width = 0.6),
            vjust = -0.5)

figure.4.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_4.csv")
figure.4.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.4.data$Platform <- factor(figure.4.data$Platform, levels=(c("News on TV",
                                                                   "News websites",
                                                                   "News on the radio",
                                                                   "Search engines, such as Google",
                                                                   "Facebook",
                                                                   "Messages from friends, family, etc",
                                                                   "YouTube",
                                                                   "Instagram",
                                                                   "Print newspapers",
                                                                   "News alerts on your mobile device",
                                                                   "Twitter / X",
                                                                   "Email newsletters",
                                                                   "TikTok",
                                                                   "Facebook Messenger",
                                                                   "Podcasts",
                                                                   "Reddit",
                                                                   "Print magazines",
                                                                   "LinkedIn",
                                                                   "WhatsApp",
                                                                   "Snapchat",
                                                                   "Pinterest",
                                                                   "Telegram",
                                                                   "ChatGPT",
                                                                   "Threads",
                                                                   "Discord",
                                                                   "Microsoft Copilot",
                                                                   "WeChat / Weixin",
                                                                   "Other",
                                                                   "None of the above"
                                                                   
)))
figure.4 <- plot.column.dais(figure.4.data,
                             Share,Platform,label.unit="%")+
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,100), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            vjust = -0.5)


figure.5.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_5.csv")
figure.5.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.5.data[,Year:=as.factor(Year)][,Year:=reorder(Year, c(rep(1,14),rep(2,14),rep(3,14),rep(4,14)))]
figure.5.data[,Platform:=str_wrap(Platform, 30)]

figure.5 <- plot.column.dais(figure.5.data,
                             Share,Platform,group.by=Year,order.bar="descending") +
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,80), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_dodge(width = 0.6),
            vjust = -0.5)

figure.6.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_6.csv")
figure.6.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.6.data[,Age:=as.factor(Age)][,Age:=reorder(Age,c(rep(1,29),rep(2,29),rep(3,29),rep(4,29)))]
figure.6.data$Platform <- factor(figure.6.data$Platform, levels=(rev(c("News on TV",
                                                                   "News websites",
                                                                   "News on the radio",
                                                                   "Search engines, such as Google",
                                                                   "Facebook",
                                                                   "Messages from friends, etc",
                                                                   "YouTube",
                                                                   "Instagram",
                                                                   "Print newspapers",
                                                                   "News alerts on your mobile device",
                                                                   "Twitter / X",
                                                                   "Email newsletters",
                                                                   "TikTok",
                                                                   "Facebook Messenger",
                                                                   "Podcasts",
                                                                   "Reddit",
                                                                   "Print magazines",
                                                                   "LinkedIn",
                                                                   "WhatsApp",
                                                                   "Snapchat",
                                                                   "Pinterest",
                                                                   "Telegram",
                                                                   "ChatGPT",
                                                                   "Threads",
                                                                   "Discord",
                                                                   "Microsoft Copilot",
                                                                   "WeChat / Weixin",
                                                                   "Other",
                                                                   "None of the above"
                                                                   
))))
figure.6 <- ggplot(figure.6.data,aes(Platform,Share,colour=Age)) + dais.base.theme() +
  geom_point() +
  scale_color_manual(values = c(set.colours(4))) + coord_flip() + #Change colours here
  theme(panel.grid.major.y = element_line(colour=set.colours(1,categorical.choice="grey"),linewidth=0.4),
        panel.grid.minor.y = element_line(colour=set.colours(1,categorical.choice="grey"),linewidth=0.4)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

figure.7.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_7.csv")
figure.7.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.7.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,c(rep(1,15),rep(2,15)))]
figure.7.data$Answer <- factor(figure.7.data$Answer, levels=c("Trust", "Distrust"))
figure.7.data$Platform <- factor(figure.7.data$Platform, levels=c("News on TV",
                                                                   "News websites",
                                                                   "News on the radio",
                                                                   "Search engines",
                                                                   "YouTube",
                                                                   "Print newspapers",
                                                                   "Messages from friends, etc",
                                                                   "Twitter / X",
                                                                   "Facebook",
                                                                   "Instagram",
                                                                   "News alerts on your phone",
                                                                   "TikTok",
                                                                   "Other",
                                                                   "None of the above",
                                                                   "Don't know"

                                                                       
))
figure.7 <- plot.column.dais(figure.7.data,Share,Platform,group.by=Answer,stacked=FALSE) +  
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits=c(0,40), expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_dodge(width = 0.6),
            vjust = -0.5) +
  theme(legend.direction = "vertical")

figure.8.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_8.csv")
figure.8.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.8.data[Platform=="Very negative impact",order:=1][Platform=="Somewhat negative impact",order:=2][Platform=="No impact",order:=3][Platform=="Somewhat positive impact",order:=4][Platform=="Very positive impact",order:=5][Platform=="Don't know",order:=6]
figure.8.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,order)]
figure.8 <- plot.column.dais(figure.8.data,Trust,Platform,
                             colours=set.colours(6,categorical.choice = c("hot.pink","orange","black","light.blue","blue","grey"))) +
  annotate("text",x=1.5,y=42,label="Total Negative: 41%",family="Replica-Bold") +
  annotate("text",x=4.5,y=15,label="Total Positive: 6%",family="Replica-Bold") +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=2.5,ymin=0,ymax=40,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA) +
  annotate("rect",xmin=3.5,xmax=5.5,ymin=0,ymax=12,alpha=1,colour=set.colours(1,categorical.choice = "blue"),fill=NA)+
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,60), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Trust, 0), "%")), 
            vjust = -0.5)


figure.9.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_9.csv")
figure.9.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.9.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(c(5,4,3,2,1),3))]
figure.9.data[,Answer:=as.factor(Answer)][,Answer:=reorder(Answer,c(rep(3,5),rep(2,5),rep(1,5)))]
figure.9 <- plot.column.dais(figure.9.data,Share,Statement,order.bar = "ascending",group.by=Answer,stacked=TRUE,
                             colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip() + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())

figure.10.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_10.csv")
figure.10.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.10.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,rep(seq(17,1),4))]
figure.10.data[,Trust:=as.factor(Trust)][,Trust:=reorder(Trust,c(rep(2,17),rep(3,17),rep(4,17),rep(1,17)))]
figure.10.data <- figure.10.data %>%
  group_by(Platform) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup()
figure.10 <- plot.column.dais(figure.10.data,Share,Platform,group.by=Trust,stacked=TRUE,
                             colours = c("grey",rev(set.colours(3,type="gradient")))
                             ) + coord_flip() + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())

#Figure 11 does last
figure.11.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_11.csv")
figure.11.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.11.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,c(rep(3,16),rep(2,16),rep(1,12)))]
figure.11.data[,Trust:=as.factor(Trust)][,Trust:=reorder(Trust,c(rep(c(rep(2,4),rep(3,4),rep(4,4),rep(1,4)),2),
                                                                 rep(2,3),rep(3,3),rep(4,3),rep(1,3)))]
figure.11.data[,Year:=as.character(Year)]
figure.11 <- ggplot(figure.11.data,aes(Year,Share,fill=Trust)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Platform~.,switch = "y",scales = "free_y",space="free_y") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0.05,0.6)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside") +coord_flip() +
  scale_fill_manual(values = c("grey",rev(set.colours(3,type="gradient")))) + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())

figure.12.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_12.csv")
figure.12.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.12.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,rep(seq(8,1),4))]
figure.12.data[,Trust:=as.factor(Trust)][,Trust:=reorder(Trust,c(rep(2,8),rep(3,8),rep(4,8),rep(1,8)))]
figure.12 <- plot.column.dais(figure.12.data,Share,Platform,group.by=Trust,stacked=TRUE,
                              colours = c("grey",rev(set.colours(3,type="gradient")))
) + coord_flip()+ 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())

figure.13.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_13.csv")
figure.13.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.13.data[,Source:=as.factor(Source)][,Source:=reorder(Source,rep(seq(7,1),4))]
figure.13.data[,Trust:=as.factor(Trust)][,Trust:=reorder(Trust,c(rep(4,7),rep(3,7),rep(2,7),rep(1,7)))]
figure.13.data <- figure.13.data %>%
  group_by(Source) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup()
figure.13 <- plot.column.dais(figure.13.data,Share,Source,group.by=Trust,stacked=TRUE,
                              colours = c("grey","black",rev(set.colours(2,type="gradient")))
) + coord_flip() + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())


figure.14.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_14.csv")
figure.14.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.14.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(6,1),6))]
figure.14.data[,Frequency:=as.factor(Frequency)][,Frequency:=reorder(Frequency,c(rep(6,6),rep(5,6),rep(4,6),rep(3,6),rep(2,6),rep(1,6)))]
figure.14.data <- figure.14.data %>%
  group_by(Statement) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup()
figure.14 <- plot.column.dais(figure.14.data,Share,Statement,group.by=Frequency,stacked=TRUE,
                              colours = c("grey","black",rev(set.colours(4,type="gradient")))
) + coord_flip()+ 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())


figure.15.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_15.csv")
figure.15.data <- figure.15.data[Year!=2021]
figure.15.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.15.data$Statement <- factor(figure.15.data$Statement, levels=(rev(c("Information about the news or current events that you immediately suspect to be false",
                                                                       "Information about the news or current events that you believe to be true and later find out is false",
                                                                       "Hate speech that deliberately promotes hatred against an identifiable group",
                                                                       "Promotion or encouragement of physical violence",
                                                                       "Identity fraud or impersonation"
                                                                       
))))
figure.15.data[,Statement:=str_wrap(Statement,20)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,c(rep(5,9),rep(4,6),rep(3,9),rep(2,9),rep(1,6)))]
figure.15.data$Statement <- factor(figure.15.data$Statement, levels=rev(levels(figure.15.data$Statement)))
figure.15.data[,Frequency:=as.factor(Frequency)][,Frequency:=reorder(Frequency,c(rep(3,3),rep(2,3),rep(1,3),
                                                                                 rep(3,2),rep(2,2),rep(1,2),
                                                                                 rep(3,3),rep(2,3),rep(1,3),
                                                                                 rep(3,3),rep(2,3),rep(1,3),
                                                                                 rep(3,2),rep(2,2),rep(1,2)))]
figure.15.data[,Year:=as.character(Year)]
figure.15 <- ggplot(figure.15.data,aes(Year,Share,fill=Frequency)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Statement~.,switch = "y",scales = "free_y",space="free_y") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0.05,1)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle=0)) +coord_flip() +
  scale_fill_manual(values = rev(set.colours(3,type="gradient"))) + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())


figure.16.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_16.csv")
figure.16.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.16.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(3,1),3))]
figure.16.data[,Answer:=as.factor(Answer)][,Answer:=reorder(Answer,c(rep(3,3),rep(2,3),rep(1,3)))]
figure.16 <- plot.column.dais(figure.16.data,Share,Statement,group.by=Answer,stacked=TRUE,
                              colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip()+ 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())



figure.18.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_18.csv")
figure.18.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.18.data[,Statement:=str_wrap(Statement,30)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(1,5),2))]
figure.18.data[,Age:=as.factor(Age)][,Age:=reorder(Age,c(rep(1,5),rep(2,5)))]
figure.18 <- plot.column.dais(figure.18.data,Share,Statement,group.by=Age) +
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,45), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_dodge(width = 0.6),
            vjust = -0.5)


figure.19.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_19.csv")
figure.19.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.19.data[,Statement:=str_wrap(Statement,30)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(4,1),3))]
figure.19.data[,Answer:=as.factor(Answer)][,Answer:=reorder(Answer,c(rep(3,4),rep(2,4),rep(1,4)))]
figure.19 <- plot.column.dais(figure.19.data,Share,Statement,group.by=Answer,stacked=TRUE,
                              colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip() +
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())

figure.20.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_20.csv")
figure.20.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.20.data[,Point:=str_wrap(Point,15)][,Point:=as.factor(Point)][,Point:=reorder(Point,seq(1,10))]
figure.20 <- plot.column.dais(figure.20.data,Trust,Point,
                             colours=c(rep(set.colours(1,categorical.choice = "hot.pink"),9),"grey")) +
  annotate("text",x=2,y=23,label="Total Ineffective: 28%",family="Replica-Bold") +
  annotate("text",x=8,y=27,label="Total Effective: 34%",family="Replica-Bold") +
  scale_y_continuous(limits=c(0,24),expand=c(0,0)) +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=3.5,ymin=0,ymax=25,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA) +
  annotate("rect",xmin=6.5,xmax=9.5,ymin=0,ymax=29,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA) +
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,40), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Trust, 0), "%")), 
            vjust = -0.5)


figure.21.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_21.csv")
figure.21.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.21 <- plot.column.dais(figure.21.data,Trust,Point) +
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,27), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Trust, 0), "%")),
            vjust = -0.5)


figure.22.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_22.csv")
figure.22.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.22.data <- figure.22.data %>%
  group_by(Statement) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup() %>%
  as.data.table
figure.22.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(8,1),5))]
figure.22.data[,Truth:=as.factor(Truth)][,Truth:=reorder(Truth,c(rep(5,8),rep(4,8),rep(3,8),rep(2,8),rep(1,8)))]
figure.22 <- plot.column.dais(figure.22.data,Share,Statement,group.by=Truth,stacked=TRUE,
                              colours = set.colours(5,categorical.choice = c("grey","hot.pink","orange","light.blue","blue"))
) + coord_flip() + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())


figure.23.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_23.csv")
figure.23.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.23.data[,Type:=as.factor(Type)][,Type:=reorder(Type,c(rep(1,3),rep(2,18),rep(3,12),rep(4,9),rep(5,9),rep(6,9)))]
figure.23.data$Category <- factor(figure.23.data$Category, levels=c("Overall",
                                                                    
                                                                    "BC/YK",
                                                                    "AB/NWT",
                                                                    "MB/SK/NU",
                                                                    "ON",
                                                                    "QC",
                                                                    "Atlantic",
                                                                    
                                                                    "16-29",
                                                                    "30-44",
                                                                    "45-59",
                                                                    "60+",
                                                                    
                                                                    "Woman",
                                                                    "Man",
                                                                    
                                                                    "High school or less",
                                                                    "Certificate/ college",
                                                                    "University",
                                                                    
                                                                    "<$50K",
                                                                    "$50K-<$100K",
                                                                    "$100K+"
))
figure.23.data$Type <- factor(figure.23.data$Type, levels=c("Overall",
                                                            "Province",
                                                            "Age",
                                                            "Gender",
                                                            "Education",
                                                            "Income"
))
figure.23.data[,Correct:=as.factor(Correct)][,Correct:=reorder(Correct,rep(seq(3,1),19))]

figure.23 <- ggplot(figure.23.data,aes(Category,Share,fill=Correct)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Type~.,switch = "y",scales = "free_y",space="free_y") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0.05,0.7)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_blank()) +coord_flip() +
  scale_fill_manual(values = (set.colours(3,type="gradient"))) + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())

figure.25.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_25.csv")
figure.25.data <- figure.25.data[Belief!="Total"]
figure.25.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.25.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,rep(seq(14,1),3))]
figure.25.data[,Belief:=as.factor(Belief)][,Belief:=reorder(Belief,c(rep(3,14),rep(2,14),rep(1,14)))]
figure.25 <- plot.column.dais(figure.25.data,Share,Platform,group.by=Belief,stacked=FALSE,
                              colours = set.colours(3,categorical.choice = c("gold","black","hot.pink"))
) + coord_flip() + guides(fill=guide_legend(reverse=TRUE)) +
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,100), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_dodge(width = 0.6),
            hjust = -0.5)

figure.26.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_26.csv")
figure.26.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.26.data[,Source:=str_wrap(Source,30)][,Source:=as.factor(Source)][,Source:=reorder(Source,rep(seq(1,5),2))]
figure.26.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,c(rep(1,5),rep(2,5)))]
figure.26 <- plot.column.dais(figure.26.data,Share,Source,group.by=Statement,stacked=FALSE) +  
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits=c(0,60), expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_dodge(width = 0.6),
            vjust = -0.5) +
  guides(fill=guide_legend(ncol=2))

figure.27.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_27.csv")

figure.28.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_28.csv")
figure.28.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.28.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(12,1),6))]
figure.28.data[,Support:=as.factor(Support)][,Support:=reorder(Support,c(rep(6,12),rep(5,12),rep(4,12),rep(3,12),rep(2,12),rep(1,12)))]
figure.28 <- plot.column.dais(figure.28.data,Share,Statement,group.by=Support,stacked=TRUE,
                              colours = set.colours(6,categorical.choice = c("grey","hot.pink","orange","teal","light.blue","blue"))
) + coord_flip() + 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())





figure.29.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_29.csv")
figure.29.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.29.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,c(rep(12,4),
                                                                                                                     rep(11,4),
                                                                                                                     rep(10,8),
                                                                                                                     rep(9,4),
                                                                                                                     rep(8,8),
                                                                                                                     rep(7,4),
                                                                                                                     rep(6,4),
                                                                                                                     rep(5,8),
                                                                                                                     rep(4,4),
                                                                                                                     rep(3,4),
                                                                                                                     rep(2,4),
                                                                                                                     rep(1,4)))]
figure.29.data[,Year:=as.character(Year)]
figure.29.data[,Support:=as.factor(Support)][,Support:=reorder(Support,c(2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,
                                                                         2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,
                                                                         2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,
                                                                         2,2,1,1,2,2,1,1,2,2,1,1))]
figure.29 <- ggplot(figure.29.data,aes(Year,Share,fill=Support)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Statement~.,switch = "y",scales = "free_y",space="free_y") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0.05,0.7)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left  = element_text(angle=0)) + coord_flip() +
  scale_fill_manual(values = rev(set.colours(2,type="gradient")))+ 
  ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE)) + 
  theme(axis.title.y = element_blank())



figure.30.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_30.csv")
figure.30.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.30.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,seq(1,4))]
figure.30 <- plot.column.dais(figure.30.data,Share,Statement,colours=set.colours(4,categorical.choice = c("hot.pink","gold","black","grey"))) +
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,70), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            vjust = -0.5)




figure.31.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_31.csv")
figure.31.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.31.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,seq(1,6))]

figure.31 <- plot.column.dais(figure.31.data,Share,Statement,
                             colours=set.colours(6,categorical.choice = c("blue","light.blue","teal","orange","hot.pink","grey"))) +
  annotate("text",x=1.5,y=45,label="Total Support: 61%",family="Replica-Bold") +
  annotate("text",x=4.5,y=22,label="Total Oppose: 20%",family="Replica-Bold") +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=2.5,ymin=0,ymax=48,alpha=1,colour=set.colours(1,categorical.choice = "blue"),fill=NA) +
  annotate("rect",xmin=3.5,xmax=5.5,ymin=0,ymax=25,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA)+
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,60), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            vjust = -0.5)



figure.32.data <-fread("C:/Users/alockhart/Desktop/online-harms-2024-main/Figure_32.csv")
figure.32.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.32.data[,Statement:=str_wrap(Statement,30)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,seq(1,4))]
figure.32 <- plot.column.dais(figure.32.data,Share,Statement,
                              colours=set.colours(4,categorical.choice = c("hot.pink","hot.pink","hot.pink","grey")))+
  ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,40), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), 
            vjust = -0.5)


