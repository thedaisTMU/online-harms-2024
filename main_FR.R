library(DaisTheme)
library(ggplot2)
library(stringr)
library(data.table)
library(dplyr)
library(ggrepel)

graph.spreadsheet <- fread("Graph_spreadsheet.csv")

figure.1.data <- fread("Figure_1.csv")
figure.1.data[,Measure:=str_remove(Measure,"%")][,Measure:=as.numeric(Measure)]
figure.1.data[,Frequency:=as.factor(Frequency_FR)][,Frequency:=reorder(Frequency_FR,c(rep(7,17),rep(6,17),rep(5,17),rep(4,17),rep(3,17),rep(2,17),rep(1,17)))]
figure.1.data$Platform_FR <- factor(figure.1.data$Platform_FR, levels=rev(c("YouTube",
                                                    "Facebook",
                                                    "Facebook Messenger",
                                                    "Instagram",
                                                    "Pinterest",
                                                    "WhatsApp",
                                                    "LinkedIn",
                                                    "TikTok",
                                                    "Reddit",
                                                    "Twitter/X",
                                                    "Snapchat",
                                                    "ChatGPT",
                                                    "Discord",
                                                    "Microsoft Copilot",
                                                    "Telegram",
                                                    "Threads",
                                                    "WeChat/Weixin"
)))
figure.1.data[,text.colour:="black.c"]
figure.1.data[Frequency_FR=="Je ne m’en sers pas",text.colour:="white.c"]
figure.1.data[Measure>=3,label.b:=str_c(round(Measure,0),"%")]
figure.1 <- plot.column.dais(figure.1.data,
                             Measure, Platform_FR,group.by=Frequency_FR, stacked=TRUE,
                             label.unit = "%",
                             ) +
  scale_fill_manual(values=c("Quelques fois par heure"="#7f3051",
                             "Quelques fois par jour"="#ab2f63",
                             "Quelques fois par semaine"="#dd347a",
                             "Quelques fois par mois"="#e66b9e",
                             "Quelques fois par année"="#f2b5ce",
                             "Je ne m’en sers pas"="#3b3b3b",
                             "Incertaine ou incertain"="grey"))+
  coord_flip() +
  ##ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = label.b,colour=Frequency_FR),
            position = position_stack(vjust = 0.5),family="Replica-Light") +
  scale_colour_manual(values=c("Quelques fois par heure"="black",
                             "Quelques fois par jour"="black",
                             "Quelques fois par semaine"="black",
                             "Quelques fois par mois"="black",
                             "Quelques fois par année"="black",
                             "Je ne m’en sers pas"="white",
                             "Incertaine ou incertain"="black"))+
  guides(fill=guide_legend(reverse=TRUE, title=NULL,),colour="none") +
  theme(axis.title.y = element_blank())


figure.2.data <-fread("Figure_2.csv")
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
  ylab(NULL) +
  geom_text(aes(y=Share+4,label = paste0(round(Share, 0), "%")),
            position = position_dodge(width = 0.6),angle=90,family="Replica-Light",
            hjust = 0.5)



figure.3.data <-fread("Figure_3.csv")
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
                             label.unit="%")+
  geom_text(aes(y=Share+3,label = paste0(round(Share, 0), "%")), size=2.3,
            position = position_dodge(width = 0.6),family="Replica-Light",angle=90,
            hjust = 0.5) +coord_cartesian(clip = 'off')

figure.4.data <-fread("Figure_4.csv")
figure.4.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.4.data$Platform_FR <- factor(figure.4.data$Platform_FR, levels=(c("Nouvelles à la télévision",
                                                                   "Sites Web de nouvelles",
                                                                   "Nouvelles à la radio",
                                                                   "Moteurs de recherche",
                                                                   "Facebook",
                                                                   "Messages d’amies ou d’amis, etc",
                                                                   "YouTube",
                                                                   "Instagram",
                                                                   "Journaux imprimés",
                                                                   "Alertes d’information sur votre appareil mobile",
                                                                   "Twitter/X",
                                                                   "Infolettres par courriel",
                                                                   "TikTok",
                                                                   "Facebook Messenger",
                                                                   "Balados",
                                                                   "Reddit",
                                                                   "Magazines imprimés",
                                                                   "LinkedIn",
                                                                   "WhatsApp",
                                                                   "Snapchat",
                                                                   "Pinterest",
                                                                   "Telegram",
                                                                   "ChatGPT",
                                                                   "Threads",
                                                                   "Discord",
                                                                   "Microsoft Copilot",
                                                                   "WeChat/Weixin",
                                                                   "Autres",
                                                                   "Aucune de ces réponses"

)))
figure.4 <- plot.column.dais(figure.4.data,
                             Share,Platform_FR,label.unit="%")+
  ylab(NULL) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",size=3,
            vjust = -0.5)


figure.5.data <-fread("Figure_5.csv")
figure.5.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.5.data[,Year:=as.factor(Year)][,Year:=reorder(Year, c(rep(1,14),rep(2,14),rep(3,14),rep(4,14)))]
figure.5.data[,Platform_FR:=str_wrap(Platform_FR, 30)]

figure.5 <- plot.column.dais(figure.5.data,
                             Share,Platform_FR,group.by=Year,order.bar="descending",label.unit="%",
                             colours = set.colours(4,categorical.choice = c("hot.pink","black","gold","blue"))) +
  ylab(NULL) + scale_y_continuous(expand=c(0,0),limits=c(0,75),breaks=c(0,15,30,45,60,75),labels=c("0%","15%","30%","45%","60%","75%"))+
  geom_text(aes(y=Share+3,label = paste0(round(Share, 0), "%")), size=2.3,
            position = position_dodge(width = 0.6),family="Replica-Light",angle=90,
            hjust = 0.5) +coord_cartesian(clip = 'off')

figure.6.data <-fread("Figure_6.csv")
figure.6.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.6.data[,Age:=as.factor(Âge)][,Âge:=reorder(Âge,c(rep(1,29),rep(2,29),rep(3,29),rep(4,29)))]
figure.6.data$Platform_FR <- factor(figure.6.data$Platform_FR, levels=(rev(c("Nouvelles à la télévision",
                                                                             "Sites Web de nouvelles",
                                                                             "Nouvelles à la radio",
                                                                             "Moteurs de recherche",
                                                                             "Facebook",
                                                                             "Messages d’amies ou d’amis, etc",
                                                                             "YouTube",
                                                                             "Instagram",
                                                                             "Journaux imprimés",
                                                                             "Alertes d’information sur votre appareil mobile",
                                                                             "Twitter/X",
                                                                             "Infolettres par courriel",
                                                                             "TikTok",
                                                                             "Facebook Messenger",
                                                                             "Balados",
                                                                             "Reddit",
                                                                             "Magazines imprimés",
                                                                             "LinkedIn",
                                                                             "WhatsApp",
                                                                             "Snapchat",
                                                                             "Pinterest",
                                                                             "Telegram",
                                                                             "ChatGPT",
                                                                             "Threads",
                                                                             "Discord",
                                                                             "Microsoft Copilot",
                                                                             "WeChat/Weixin",
                                                                             "Autres",
                                                                             "Aucune de ces réponses"

))))
figure.6 <- ggplot(figure.6.data,aes(Platform_FR,Share,colour=Âge)) + dais.base.theme() +
  geom_point() +
  scale_color_manual(values = c(set.colours(4))) + coord_flip() + #Change colours here
  theme(panel.grid.major.y = element_line(colour=set.colours(1,categorical.choice="grey"),linewidth=0.4),
        panel.grid.minor.y = element_line(colour=set.colours(1,categorical.choice="grey"),linewidth=0.4)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

figure.7.data <-fread("Figure_7.csv")
figure.7.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.7.data[,Platform_FR:=as.factor(Platform_FR)][,Platform_FR:=reorder(Platform_FR,c(rep(1,15),rep(2,15)))]
figure.7.data$Answer_FR <- factor(figure.7.data$Answer_FR, levels=c("Le plus fiable", "Le moins fiable"))
figure.7.data$Platform_FR <- factor(figure.7.data$Platform_FR, levels=c("Nouvelles à la télévision",
                                                                   "Sites Web de nouvelles",
                                                                   "Nouvelles à la radio",
                                                                   "Moteurs de recherche, comme Google",
                                                                   "YouTube",
                                                                   "Journaux imprimés",
                                                                   "Messages d’amies ou d’amis, etc",
                                                                   "Twitter/X",
                                                                   "Facebook",
                                                                   "Instagram",
                                                                   "Alertes d’information sur votre appareil mobile",
                                                                   "TikTok",
                                                                   "Autres",
                                                                   "Aucune de ces réponses",
                                                                   "Ne sais pas"


))
figure.7 <- plot.column.dais(figure.7.data,Share,Platform_FR,group.by=Answer_FR,stacked=FALSE,label.unit="%") +
  ylab(NULL) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), size=2.6,
            position = position_dodge(width = 0.6),family="Replica-Light",
            vjust = -0.5) +
  theme(legend.direction = "vertical")

figure.8.data <-fread("Figure_8.csv")
figure.8.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.8.data[Platform_FR=="Incidence très négative",order:=1][Platform_FR=="Incidence quelque peu négative",order:=2][Platform_FR=="Aucune incidence",order:=3][Platform_FR=="Incidence quelque peu positive",order:=4][Platform_FR=="Incidence très positive",order:=5][Platform_FR=="Je ne sais pas",order:=6]
figure.8.data[,Platform_FR:=as.factor(Platform_FR)][,Platform_FR:=reorder(Platform_FR,order)]
figure.8 <- plot.column.dais(figure.8.data,Trust,Platform_FR,
                             colours=set.colours(6,categorical.choice = c("hot.pink","orange","black","light.blue","blue","grey")),label.unit="%") +
  annotate("text",x=1.5,y=35,label="Total négatif: 41%",family="Replica-Bold") +
  annotate("text",x=4.5,y=10,label="Total positif: 6%",family="Replica-Bold") +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=2.5,ymin=0,ymax=40,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA) +
  annotate("rect",xmin=3.5,xmax=5.5,ymin=0,ymax=12,alpha=1,colour=set.colours(1,categorical.choice = "blue"),fill=NA)+
  ylab(NULL) +
  geom_text(aes(label = paste0(round(Trust, 0), "%")), family="Replica-Light",
            vjust = -0.5)


figure.9.data <-fread("Figure_9.csv")
figure.9.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.9.data[,Statement_FR:=str_wrap(Statement_FR,40)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,rep(c(5,4,3,2,1),3))]
figure.9.data[,Answer_FR:=as.factor(Answer_FR)][,Answer_FR:=reorder(Answer_FR,c(rep(3,5),rep(2,5),rep(1,5)))]
figure.9 <- plot.column.dais(figure.9.data,Share,Statement_FR,order.bar = "ascending",group.by=Answer_FR,stacked=TRUE,
                             colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip() +
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%"),colour = Answer_FR), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  scale_color_manual(values=c("Oui"="white","Non"="white","Incertaine ou incertain"="black")) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL),color="none") +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))


figure.10.data <-fread("Figure_10.csv")
figure.10.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.10.data[,Platform_FR:=as.factor(Platform_FR)][,Platform_FR:=reorder(Platform_FR,rep(seq(17,1),4))]
figure.10.data[,Trust_FR:=as.factor(Trust_FR)][,Trust_FR:=reorder(Trust_FR,c(rep(2,17),rep(3,17),rep(4,17),rep(1,17)))]
figure.10.data <- figure.10.data %>%
  group_by(Platform_FR) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup()
figure.10 <- plot.column.dais(figure.10.data,Share,Platform_FR,group.by=Trust_FR,stacked=TRUE,
                             colours = c("grey",rev(set.colours(3,type="gradient")))
                             ) + coord_flip() +
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL)) +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))


figure.11.data <-fread("Figure_11.csv")
figure.11.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.11.data[,Platform_FR:=as.factor(Platform_FR)][,Platform_FR:=reorder(Platform_FR,c(rep(3,16),rep(2,16),rep(1,12)))]
figure.11.data[,Trust_FR:=as.factor(Trust_FR)][,Trust_FR:=reorder(Trust_FR,c(rep(c(rep(2,4),rep(3,4),rep(4,4),rep(1,4)),2),
                                                                 rep(2,3),rep(3,3),rep(4,3),rep(1,3)))]
figure.11.data[,Year:=as.character(Year)]
figure.11 <- ggplot(figure.11.data,aes(Year,Share,fill=Trust_FR)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Platform_FR~.,switch = "y",scales = "free_y",space="free_y") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0.05,0.6)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside") +coord_flip() +
  scale_fill_manual(values = c("grey",rev(set.colours(3,type="gradient")))) +
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL)) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))

figure.12.data <-fread("Figure_12.csv")
figure.12.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.12.data <- figure.12.data %>%
  group_by(Platform_FR) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup() %>%
  as.data.table()
figure.12.data[,Platform_FR:=as.factor(Platform_FR)][,Platform_FR:=reorder(Platform_FR,rep(seq(8,1),4))]
figure.12.data[,Trust_FR:=as.factor(Trust_FR)][,Trust_FR:=reorder(Trust_FR,c(rep(2,8),rep(3,8),rep(4,8),rep(1,8)))]
figure.12 <- plot.column.dais(figure.12.data,Share,Platform_FR,group.by=Trust_FR,stacked=TRUE,
                              colours = c("grey",rev(set.colours(3,type="gradient")))
) + coord_flip()+
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%"),colour=Trust_FR), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  scale_colour_manual(values=c("Haut (7 à 9)"="white","Moyen (4 à 6)"="black","Faible (1 à 3)"="black","Je ne sais pas"="black")) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL),colours="none") +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))

figure.13.data <-fread("Figure_13.csv")
figure.13.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.13.data[,Source_FR:=as.factor(Source_FR)][,Source_FR:=reorder(Source_FR,rep(seq(7,1),4))]
figure.13.data[,Trust_FR:=as.factor(Trust_FR)][,Trust_FR:=reorder(Trust_FR,c(rep(4,7),rep(3,7),rep(2,7),rep(1,7)))]
figure.13.data <- figure.13.data %>%
  group_by(Source_FR) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup()
figure.13 <- plot.column.dais(figure.13.data,Share,Source_FR,group.by=Trust_FR,stacked=TRUE,
                              colours = c("grey","black",rev(set.colours(2,type="gradient")))
) + coord_flip() +
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%"),colour = Trust_FR), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  scale_color_manual(values=c("Fortement confiance"="white","Un peu confiance"="black","Pas confiance"="white","Je ne sais pas"="black")) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL),colour="none") +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))


figure.14.data <-fread("Figure_14.csv")
figure.14.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.14.data[,Statement_FR:=str_wrap(Statement_FR,40)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,rep(seq(6,1),6))]
figure.14.data[,Frequency_FR:=as.factor(Frequency_FR)][,Frequency_FR:=reorder(Frequency_FR,c(rep(6,6),rep(5,6),rep(4,6),rep(3,6),rep(2,6),rep(1,6)))]
figure.14.data <- figure.14.data %>%
  group_by(Statement_FR) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup() %>%
  as.data.table()
figure.14 <- plot.column.dais(figure.14.data,Share,Statement_FR,group.by=Frequency_FR,stacked=TRUE,
                              colours = c("grey","black",rev(set.colours(4,type="gradient")))
) + coord_flip()+
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%"),colour=Frequency_FR), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  scale_color_manual(values=c("Quelques fois par jour"="white","Quelques fois par mois"="black","Jamais"="white",
                              "Quelques fois par semaine"="black","Quelques fois par année"="black","Incertaine ou incertain"="black")) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL),colour="none") +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))


figure.15.data <-fread("Figure_15.csv")
figure.15.data <- figure.15.data[Year!=2021]
figure.15.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.15.data$Statement_FR <- factor(figure.15.data$Statement_FR, levels=(rev(c("Informations sur les nouvelles ou les événements actuels que vous soupçonnez immédiatement d’être fausses",
                                                                       "Informations sur les nouvelles ou les événements actuels que vous croyez vraies et qui s’avèrent fausses par la suite",
                                                                       "Discours haineux qui fomente délibérément la haine contre un groupe identifiable",
                                                                       "Promotion de la violence physique ou incitation à la violence physique",
                                                                       "Fraude d’identité ou usurpation d’identité"

))))
figure.15.data[,Statement_FR:=str_wrap(Statement_FR,20)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,c(rep(5,9),rep(4,6),rep(3,9),rep(2,9),rep(1,6)))]
figure.15.data$Statement_FR <- factor(figure.15.data$Statement_FR, levels=rev(levels(figure.15.data$Statement_FR)))
figure.15.data[,Frequency_FR:=as.factor(Frequency_FR)][,Frequency_FR:=reorder(Frequency_FR,c(rep(3,3),rep(2,3),rep(1,3),
                                                                                 rep(3,2),rep(2,2),rep(1,2),
                                                                                 rep(3,3),rep(2,3),rep(1,3),
                                                                                 rep(3,3),rep(2,3),rep(1,3),
                                                                                 rep(3,2),rep(2,2),rep(1,2)))]
figure.15.data[,Year:=as.character(Year)]
figure.15 <- ggplot(figure.15.data,aes(Year,Share,fill=Frequency_FR)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Statement_FR~.,switch = "y",scales = "free_y",space="free_y") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0.05,1)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle=0)) +coord_flip() +
  scale_fill_manual(values = rev(set.colours(3,type="gradient"))) +
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL)) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,85),breaks=c(0,20,40,60,80),labels=c("0%","20%","40%","60%","80%"))


figure.16.data <-fread("Figure_16.csv")
figure.16.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.16.data[,Statement_FR:=str_wrap(Statement_FR,40)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,rep(seq(3,1),3))]
figure.16.data[,Answer_FR:=as.factor(Answer_FR)][,Answer_FR:=reorder(Answer_FR,c(rep(3,3),rep(2,3),rep(1,3)))]
figure.16 <- plot.column.dais(figure.16.data,Share,Statement_FR,group.by=Answer_FR,stacked=TRUE,
                              colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip()+
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%"),colour=Answer_FR), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  scale_colour_manual(values=c("Oui"="white","Non"="white","Incertaine ou incertain"="black"))+
  guides(fill=guide_legend(reverse=TRUE, title=NULL),colour="none") +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))



figure.18.data <-fread("Figure_18.csv")
figure.18.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.18.data[,Statement_FR:=str_wrap(Statement_FR,30)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,rep(seq(1,5),2))]
figure.18.data[,Age_FR:=as.factor(Age_FR)][,Age_FR:=reorder(Age_FR,c(rep(1,5),rep(2,5)))]
figure.18 <- plot.column.dais(figure.18.data,Share,Statement_FR,group.by=Age_FR,label.unit="%") +
  ylab(NULL) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            position = position_dodge(width = 0.6),
            vjust = -0.5) +
  scale_y_continuous(expand=c(0,0),limits=c(0,40),breaks=c(0,10,20,30,40),labels=c("0%","10%","20%","30%","40%"))


figure.19.data <-fread("Figure_19.csv")
figure.19.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.19.data[,Statement_FR:=str_wrap(Statement_FR,30)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,rep(seq(4,1),3))]
figure.19.data[,Answer_FR:=as.factor(Answer_FR)][,Answer_FR:=reorder(Answer_FR,c(rep(3,4),rep(2,4),rep(1,4)))]
figure.19 <- plot.column.dais(figure.19.data,Share,Statement_FR,group.by=Answer_FR,stacked=TRUE,
                              colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip() +
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%"),colour=Answer_FR), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  scale_colour_manual(values=c("Oui"="white","Non"="white","Incertaine ou incertain"="black"))+
  guides(fill=guide_legend(reverse=TRUE, title=NULL),colour="none") +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))

figure.20.data <-fread("Figure_20.csv")
figure.20.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.20.data[,Point_FR:=str_wrap(Point_FR,15)][,Point:=as.factor(Point_FR)][,Point:=reorder(Point_FR,seq(1,10))]
figure.20 <- plot.column.dais(figure.20.data,Trust,Point_FR,
                             colours=c(rep(set.colours(1,categorical.choice = "hot.pink"),9),"grey")) +
  annotate("text",x=2,y=16,label="Total inefficace: 28%",family="Replica-Bold") +
  annotate("text",x=8,y=18,label="Total efficace: 34%",family="Replica-Bold") +
  scale_y_continuous(limits=c(0,21),expand=c(0,0),breaks = c(0,5,10,15,20),labels=c("0%","5%","10%","15%","20%")) +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=3.5,ymin=0,ymax=19,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA) +
  annotate("rect",xmin=6.5,xmax=9.5,ymin=0,ymax=20,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA) +
  ylab(NULL) +
  geom_text(aes(label = paste0(round(Trust, 0), "%")), family="Replica-Light",
            vjust = -0.5)


figure.21.data <-fread("Figure_21.csv")
figure.21.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.21 <- plot.column.dais(figure.21.data,Trust,Point_FR,label.unit="%") +
  ylab(NULL) +
  geom_text(aes(label = paste0(round(Trust, 0), "%")),family="Replica-Light",
            vjust = -0.5)


figure.22.data <-fread("Figure_22.csv")
figure.22.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.22.data <- figure.22.data %>%
  group_by(Statement_FR) %>%
  mutate(Share = Share / sum(Share) * 100) %>%
  ungroup() %>%
  as.data.table
figure.22.data[,Statement_FR:=str_wrap(Statement_FR,40)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,rep(seq(8,1),5))]
figure.22.data[,Truth_FR:=as.factor(Truth_FR)][,Truth_FR:=reorder(Truth_FR,c(rep(5,8),rep(4,8),rep(3,8),rep(2,8),rep(1,8)))]
figure.22.data[Share>=4,label.b:=str_c(round(Share,0),"%")]
figure.22 <- plot.column.dais(figure.22.data,Share,Statement_FR,group.by=Truth_FR,stacked=TRUE,
                              colours = set.colours(5,categorical.choice = c("grey","hot.pink","orange","light.blue","blue"))
) + coord_flip() +
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = label.b,colour=Truth_FR), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  scale_color_manual(values=c("Tout à fait vrai"="white","Plus ou moins vrai"="black","Plutôt faux"="black",
                              "Tout à fait faux"="white","Je ne sais pas"="black"))  +
  guides(fill=guide_legend(reverse=TRUE,title=NULL,ncol=3),colour="none") +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,101),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))


figure.23.data <-fread("Figure_23.csv")
figure.23.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.23.data$Category_FR <- factor(figure.23.data$Category_FR, levels=c("Dans l’ensemble",

                                                                    "C.-B./Yukon",
                                                                    "Alberta/T. N.-O.",
                                                                    "Manitoba/Sask/Nunavut",
                                                                    "Ontario",
                                                                    "Québec",
                                                                    "Atlantique",

                                                                    "16-29",
                                                                    "30-44",
                                                                    "45-59",
                                                                    "60+",

                                                                    "Femme",
                                                                    "Homme",
                                                                    "École secondaire ou moins",
                                                                    "Certificat/collège",
                                                                    "Université",
                                                                    "<50 000$",
                                                                    "50 000$ à <100 000$",
                                                                    "100 000$ et plus"
))
figure.23.data$Type <- factor(figure.23.data$Type, levels=c("Overall",
                                                            "Province",
                                                            "Age",
                                                            "Gender",
                                                            "Education",
                                                            "Income"
))
figure.23.data[,Correct_FR:=as.factor(Correct_FR)][,Correct_FR:=reorder(Correct_FR,rep(seq(3,1),19))]

figure.23 <- ggplot(figure.23.data,aes(Category_FR,Share,fill=Correct_FR)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Type~.,switch = "y",scales = "free_y",space="free_y") +
  scale_x_discrete(expand=c(0.05,0.7)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_blank()) +coord_flip() +
  scale_fill_manual(values = (set.colours(3,type="gradient"))) +
  #ylab(NULL) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL)) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,101),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))

figure.25.data <-fread("Figure_25.csv")
figure.25.data <- figure.25.data[Belief!="Total"]
figure.25.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.25.data[,Platform_FR:=as.factor(Platform_FR)][,Platform_FR:=reorder(Platform_FR,rep(seq(14,1),3))]
figure.25.data[,Belief_FR:=as.factor(Belief_FR)][,Belief_FR:=reorder(Belief_FR,c(rep(3,14),rep(2,14),rep(1,14)))]
figure.25 <- plot.column.dais(figure.25.data,Share,Platform_FR,group.by=Belief_FR,stacked=FALSE,
                              colours = set.colours(3,categorical.choice = c("gold","black","hot.pink"))
) + coord_flip() + 
  #ylab(NULL) + scale_y_continuous(breaks = NULL, limits = c(0,100), expand=c(0,0)) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",size=2.7,
            position = position_dodge(width = 0.6),
            hjust = -0.5) + 
  guides(fill=guide_legend(reverse=TRUE,title=NULL,ncol=2),colour="none") +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,80),breaks=c(0,20,40,60,80),labels=c("0%","20%","40%","60%","80%"))

figure.26.data <-fread("Figure_26.csv")
figure.26.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.26.data[,Source_FR:=str_wrap(Source_FR,30)][,Source_FR:=as.factor(Source_FR)][,Source_FR:=reorder(Source_FR,rep(seq(1,5),2))]
figure.26.data[,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,c(rep(1,5),rep(2,5)))]
figure.26 <- plot.column.dais(figure.26.data,Share,Source_FR,group.by=Statement_FR,stacked=FALSE,label.unit="%") +
  ylab(NULL) + scale_y_continuous(expand=c(0,0),limits=c(0,60),breaks=c(0,10,20,30,40,50),labels=c("0%","10%","20%","30%","40%","50%")) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            position = position_dodge(width = 0.6),
            vjust = -0.5) +
  guides(fill=guide_legend(ncol=2, title=NULL))


figure.28.data <-fread("Figure_28.csv")
figure.28.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.28.data[,Statement_FR:=str_wrap(Statement_FR,40)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,rep(seq(12,1),6))]
figure.28.data[,Support_FR:=as.factor(Support_FR)][,Support_FR:=reorder(Support_FR,c(rep(6,12),rep(5,12),rep(4,12),rep(3,12),rep(2,12),rep(1,12)))]
figure.28 <- plot.column.dais(figure.28.data,Share,Statement_FR,group.by=Support_FR,stacked=TRUE,
                              colours = set.colours(6,categorical.choice = c("grey","hot.pink","orange","teal","light.blue","blue"))
) + coord_flip() +
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(data=figure.28.data[Support_FR %in% c("Soutien ferme","Soutien quelconque","Neutre")],aes(label = paste0(round(Share, 0), "%"),colour=Support_FR), family="Replica-Light",
            position = position_stack(vjust = 0.5)) +
  scale_color_manual(values=c("Soutien ferme"="white","Soutien quelconque"="black","Neutre"="black")) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL),color="none") +
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,101),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))





figure.29.data <-fread("Figure_29.csv")
figure.29.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.29.data[,Statement_FR:=str_wrap(Statement_FR,40)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,c(rep(12,4),
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
figure.29.data[,Support_FR:=as.factor(Support_FR)][,Support_FR:=reorder(Support_FR,c(2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,
                                                                         2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,
                                                                         2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,
                                                                         2,2,1,1,2,2,1,1,2,2,1,1))]
figure.29 <- ggplot(figure.29.data,aes(Year,Share,fill=Support_FR)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Statement_FR~.,switch = "y",scales = "free_y",space="free_y") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0.05,0.7)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left  = element_text(angle=0)) + coord_flip() +
  scale_fill_manual(values = rev(set.colours(2,type="gradient")))+
  #ylab(NULL) + scale_y_discrete(breaks = NULL, expand=c(0,0))+
  geom_text(aes(label = paste0(round(Share, 0), "%"),colour=Support_FR), family="Replica-Light",size=2.7,
            position = position_stack(vjust = 0.5)) +
  scale_colour_manual(values=c("Soutien ferme"="white","Soutien quelconque"="black")) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL),colour="none") +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  scale_y_continuous(expand=c(0,0),limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))



figure.30.data <-fread("Figure_30.csv")
figure.30.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.30.data[,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,seq(1,4))]
figure.30 <- plot.column.dais(figure.30.data,Share,Statement_FR,
                              colours=set.colours(4,categorical.choice = c("hot.pink","gold","black","grey")),
                              label.unit="%") +
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            vjust = -0.5) +
  ylab(NULL)




figure.31.data <-fread("Figure_31.csv")
figure.31.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.31.data[,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,seq(1,6))]

figure.31 <- plot.column.dais(figure.31.data,Share,Statement_FR,
                             colours=set.colours(6,categorical.choice = c("blue","light.blue","teal","orange","hot.pink","grey")),label.unit="%") +
  annotate("text",x=1.5,y=37,label="Soutien total: 61%",family="Replica-Bold") +
  annotate("text",x=4.5,y=19,label="Opposition totale: 20%",family="Replica-Bold") +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=2.5,ymin=0,ymax=39,alpha=1,colour=set.colours(1,categorical.choice = "blue"),fill=NA) +
  annotate("rect",xmin=3.5,xmax=5.5,ymin=0,ymax=21,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA)+
  ylab(NULL) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            vjust = -0.5)



figure.32.data <-fread("Figure_32.csv")
figure.32.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.32.data[,Statement_FR:=str_wrap(Statement_FR,30)][,Statement_FR:=as.factor(Statement_FR)][,Statement_FR:=reorder(Statement_FR,seq(1,4))]
figure.32 <- plot.column.dais(figure.32.data,Share,Statement_FR,
                              colours=set.colours(4,categorical.choice = c("hot.pink","hot.pink","hot.pink","grey")),label.unit="%")+
  ylab(NULL) + scale_y_continuous(expand=c(0,0),limits=c(0,35),breaks = c(0,10,20,30),labels=c("0%","10%","20%","30%")) +
  geom_text(aes(label = paste0(round(Share, 0), "%")), family="Replica-Light",
            vjust = -0.5)


export.dais.plot("Exported/French/Figure_1_FR.pdf",figure.1,p.height = 6.5)
export.dais.plot("Exported/French/Figure_1_FR.svg",figure.1,p.height = 6.5,type="svg")

export.dais.plot("Exported/French/Figure_2_FR.pdf",figure.2,p.height = 6.5)
export.dais.plot("Exported/French/Figure_2_FR.svg",figure.2,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_3_FR.pdf",figure.3,p.height = 6.5)
export.dais.plot("Exported/French/Figure_3_FR.svg",figure.3,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_4_FR.pdf",figure.4,p.height = 6.5)
export.dais.plot("Exported/French/Figure_4_FR.svg",figure.4,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_5_FR.pdf",figure.5,p.height = 6.5)
export.dais.plot("Exported/French/Figure_5_FR.svg",figure.5,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_6_FR.pdf",figure.6,p.height = 6.5)
export.dais.plot("Exported/French/Figure_6_FR.svg",figure.6,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_7_FR.pdf",figure.7,p.height = 6.5)
export.dais.plot("Exported/French/Figure_7_FR.svg",figure.7,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_8_FR.pdf",figure.8,p.height = 6.5)
export.dais.plot("Exported/French/Figure_8_FR.svg",figure.8,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_9_FR.pdf",figure.9,p.height = 6.5)
export.dais.plot("Exported/French/Figure_9_FR.svg",figure.9,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_10_FR.pdf",figure.10,p.height = 6.5)
export.dais.plot("Exported/French/Figure_10_FR.svg",figure.10,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_11_FR.pdf",figure.11,p.height = 6.5)
export.dais.plot("Exported/French/Figure_11_FR.svg",figure.11,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_12_FR.pdf",figure.12,p.height = 6.5)
export.dais.plot("Exported/French/Figure_12_FR.svg",figure.12,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_13_FR.pdf",figure.13,p.height = 6.5)
export.dais.plot("Exported/French/Figure_13_FR.svg",figure.13,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_14_FR.pdf",figure.14,p.height = 6.5)
export.dais.plot("Exported/French/Figure_14_FR.svg",figure.14,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_15_FR.pdf",figure.15,p.height = 6.5)
export.dais.plot("Exported/French/Figure_15_FR.svg",figure.15,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_16_FR.pdf",figure.16,p.height = 6.5)
export.dais.plot("Exported/French/Figure_16_FR.svg",figure.16,p.height = 6.5,type="svg")

export.dais.plot("Exported/French/Figure_18_FR.pdf",figure.18,p.height = 6.5)
export.dais.plot("Exported/French/Figure_18_FR.svg",figure.18,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_19_FR.pdf",figure.19,p.height = 6.5)
export.dais.plot("Exported/French/Figure_19_FR.svg",figure.19,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_20_FR.pdf",figure.20,p.height = 6.5)
export.dais.plot("Exported/French/Figure_20_FR.svg",figure.20,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_21_FR.pdf",figure.21,p.height = 6.5)
export.dais.plot("Exported/French/Figure_21_FR.svg",figure.21,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_22_FR.pdf",figure.22,p.height = 6.5)
export.dais.plot("Exported/French/Figure_22_FR.svg",figure.22,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_23_FR.pdf",figure.23,p.height = 6.5)
export.dais.plot("Exported/French/Figure_23_FR.svg",figure.23,p.height = 6.5,type="svg")

export.dais.plot("Exported/French/Figure_25_FR.pdf",figure.25,p.height = 6.5)
export.dais.plot("Exported/French/Figure_25_FR.svg",figure.25,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_26_FR.pdf",figure.26,p.height = 6.5)
export.dais.plot("Exported/French/Figure_26_FR.svg",figure.26,p.height = 6.5,type="svg")

export.dais.plot("Exported/French/Figure_28_FR.pdf",figure.28,p.height = 6.5)
export.dais.plot("Exported/French/Figure_28_FR.svg",figure.28,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_29_FR.pdf",figure.29,p.height = 6.5)
export.dais.plot("Exported/French/Figure_29_FR.svg",figure.29,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_30_FR.pdf",figure.30,p.height = 6.5)
export.dais.plot("Exported/French/Figure_30_FR.svg",figure.30,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_31_FR.pdf",figure.31,p.height = 6.5)
export.dais.plot("Exported/French/Figure_31_FR.svg",figure.31,p.height = 6.5,type="svg")
export.dais.plot("Exported/French/Figure_32_FR.pdf",figure.32,p.height = 6.5)
export.dais.plot("Exported/French/Figure_32_FR.svg",figure.32,p.height = 6.5,type="svg")
