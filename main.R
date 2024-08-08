library(DaisTheme)
library(ggplot2)
library(stringr)
library(data.table)

graph.spreadsheet <- fread("Graph_spreadsheet.csv")

figure.1.data <- fread("Figure_1.csv")
figure.1.data[,Measure:=str_remove(Measure,"%")][,Measure:=as.numeric(Measure)]
figure.1.data[,Frequency:=as.factor(Frequency)][,Frequency:=reorder(Frequency,c(rep(7,17),rep(6,17),rep(5,17),rep(4,17),rep(3,17),rep(2,17),rep(1,17)))]
figure.1 <- plot.column.dais(figure.1.data,
                             Measure, Platform,group.by=Frequency, stacked=TRUE,
                             label.unit = "%",
                             ) + coord_flip()

figure.2.data <-fread("Figure_2.csv")
figure.2.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.2.data[,Year:=as.character(Year)][,Year:=as.factor(Year)][,Year:=reorder(Year,c(rep(1,11),rep(2,11),rep(3,11)))]
figure.2 <- plot.column.dais(figure.2.data,
                             Share, Platform, group.by=Year,
                             label.unit = "%")


figure.3.data <-fread("Figure_3.csv")
figure.3.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.3.data[,Age:=as.factor(Age)][,Age:=reorder(Age,c(rep(1,17),rep(2,17),rep(3,17),rep(4,17)))]

figure.3 <- plot.column.dais(figure.3.data,
                             Share,Platform, group.by=Age,stacked=FALSE,order.bar="descending",
                             label.unit="%")

figure.4.data <-fread("Figure_4.csv")
figure.4.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.4.data[,Platform:=str_wrap(Platform, 30)]

figure.4 <- plot.column.dais(figure.4.data,
                             Share,Platform, order.bar = "descending",label.unit="%")


figure.5.data <-fread("Figure_5.csv")
figure.5.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.5.data[,Year:=as.factor(Year)][,Year:=reorder(Year, c(rep(1,14),rep(2,14),rep(3,14),rep(4,14)))]
figure.5.data[,Platform:=str_wrap(Platform, 30)]

figure.5 <- plot.column.dais(figure.5.data,
                             Share,Platform,group.by=Year,order.bar="descending")

figure.6.data <-fread("Figure_6.csv")
figure.6.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.6.data[,Age:=as.factor(Age)][,Age:=reorder(Age,c(rep(1,29),rep(2,29),rep(3,29),rep(4,29)))]
figure.6.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,Share)]
figure.6 <- ggplot(figure.6.data,aes(Platform,Share,colour=Age)) + dais.base.theme() +
  geom_point() +
  scale_color_manual(values = c(set.colours(4))) + coord_flip() + #Change colours here
  theme(panel.grid.major.y = element_line(colour=set.colours(1,categorical.choice="grey"),linewidth=0.4),
        panel.grid.minor.y = element_line(colour=set.colours(1,categorical.choice="grey"),linewidth=0.4))

figure.7.data <-fread("Figure_7.csv")
figure.7.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.7 <- plot.pyramid.dais(figure.7.data,Share,Answer,Platform)

figure.8.data <-fread("Figure_8.csv")
figure.8.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.8.data[Platform=="Very negative impact",order:=1][Platform=="Somewhat negative impact",order:=2][Platform=="No impact",order:=3][Platform=="Somewhat positive impact",order:=4][Platform=="Very positive impact",order:=5][Platform=="Don't know",order:=6]
figure.8.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,order)]
figure.8 <- plot.column.dais(figure.8.data,Trust,Platform,
                             colours=set.colours(6,categorical.choice = c("hot.pink","orange","black","light.blue","blue","grey"))) +
  annotate("text",x=1.5,y=32,label="Total Negative: 41%",family="Replica-Bold") +
  annotate("text",x=4.5,y=10,label="Total Positive: 6%",family="Replica-Bold") +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=2.5,ymin=0,ymax=30,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA) +
  annotate("rect",xmin=3.5,xmax=5.5,ymin=0,ymax=8,alpha=1,colour=set.colours(1,categorical.choice = "blue"),fill=NA)


figure.9.data <-fread("Figure_9.csv")
figure.9.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.9.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(c(5,4,3,2,1),3))]
figure.9.data[,Answer:=as.factor(Answer)][,Answer:=reorder(Answer,c(rep(3,5),rep(2,5),rep(1,5)))]
figure.9 <- plot.column.dais(figure.9.data,Share,Statement,order.bar = "ascending",group.by=Answer,stacked=TRUE,
                             colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip()

figure.10.data <-fread("Figure_10.csv")
figure.10.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.10.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,rep(seq(17,1),4))]
figure.10.data[,Trust:=as.factor(Trust)][,Trust:=reorder(Trust,c(rep(2,17),rep(3,17),rep(4,17),rep(1,17)))]
figure.10 <- plot.column.dais(figure.10.data,Share,Platform,group.by=Trust,stacked=TRUE,
                             colours = c("grey",rev(set.colours(3,type="gradient")))
                             ) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))

#Figure 11 does last
figure.11.data <-fread("Figure_11.csv")
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
  scale_fill_manual(values = c("grey",rev(set.colours(3,type="gradient"))))

figure.12.data <-fread("Figure_12.csv")
figure.12.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.12.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,rep(seq(8,1),4))]
figure.12.data[,Trust:=as.factor(Trust)][,Trust:=reorder(Trust,c(rep(2,8),rep(3,8),rep(4,8),rep(1,8)))]
figure.12 <- plot.column.dais(figure.12.data,Share,Platform,group.by=Trust,stacked=TRUE,
                              colours = c("grey",rev(set.colours(3,type="gradient")))
) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))

figure.13.data <-fread("Figure_13.csv")
figure.13.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.13.data[,Source:=as.factor(Source)][,Source:=reorder(Source,rep(seq(7,1),4))]
figure.13.data[,Trust:=as.factor(Trust)][,Trust:=reorder(Trust,c(rep(4,7),rep(3,7),rep(2,7),rep(1,7)))]
figure.13 <- plot.column.dais(figure.13.data,Share,Source,group.by=Trust,stacked=TRUE,
                              colours = c("grey","black",rev(set.colours(2,type="gradient")))
) + coord_flip() + guides(fill=guide_legend(reverse=TRUE)) +
  annotate()


figure.14.data <-fread("Figure_14.csv")
figure.14.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.14.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(6,1),6))]
figure.14.data[,Frequency:=as.factor(Frequency)][,Frequency:=reorder(Frequency,c(rep(6,6),rep(5,6),rep(4,6),rep(3,6),rep(2,6),rep(1,6)))]
figure.14 <- plot.column.dais(figure.14.data,Share,Statement,group.by=Frequency,stacked=TRUE,
                              colours = c("grey","black",rev(set.colours(4,type="gradient")))
) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))


figure.15.data <-fread("Figure_15.csv")
figure.15.data <- figure.15.data[Year!=2021]
figure.15.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.15.data[,Statement:=str_wrap(Statement,20)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,c(rep(5,9),rep(4,6),rep(3,9),rep(2,9),rep(1,6)))]
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
  scale_fill_manual(values = rev(set.colours(3,type="gradient")))


figure.16.data <-fread("Figure_16.csv")
figure.16.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.16.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(3,1),3))]
figure.16.data[,Answer:=as.factor(Answer)][,Answer:=reorder(Answer,c(rep(3,3),rep(2,3),rep(1,3)))]
figure.16 <- plot.column.dais(figure.16.data,Share,Statement,group.by=Answer,stacked=TRUE,
                              colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))



figure.18.data <-fread("Figure_18.csv")
figure.18.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.18.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(1,5),2))]
figure.18.data[,Age:=as.factor(Age)][,Age:=reorder(Age,c(rep(1,5),rep(2,5)))]
figure.18 <- plot.column.dais(figure.18.data,Share,Statement,group.by=Age)


figure.19.data <-fread("Figure_19.csv")
figure.19.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.19.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(4,1),3))]
figure.19.data[,Answer:=as.factor(Answer)][,Answer:=reorder(Answer,c(rep(3,4),rep(2,4),rep(1,4)))]
figure.19 <- plot.column.dais(figure.19.data,Share,Statement,group.by=Answer,stacked=TRUE,
                              colours = set.colours(3,categorical.choice=c("grey","black","hot.pink"))) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))

figure.20.data <-fread("Figure_20.csv")
figure.20.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.20.data[,Point:=str_wrap(Point,15)][,Point:=as.factor(Point)][,Point:=reorder(Point,seq(1,10))]
figure.20 <- plot.column.dais(figure.20.data,Trust,Point,
                             colours=c(rep(set.colours(1,categorical.choice = "hot.pink"),9),"grey")) +
  annotate("text",x=2,y=16,label="Total Ineffective: 28%",family="Replica-Bold") +
  annotate("text",x=8,y=20,label="Total Effective: 34%",family="Replica-Bold") +
  scale_y_continuous(limits=c(0,24),expand=c(0,0)) +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=3.5,ymin=0,ymax=18,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA) +
  annotate("rect",xmin=6.5,xmax=9.5,ymin=0,ymax=22,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA)


figure.21.data <-fread("Figure_21.csv")
figure.21.data[,Trust:=str_remove(Trust,"%")][,Trust:=as.numeric(Trust)]
figure.21 <- plot.column.dais(figure.21.data,Trust,Point)


figure.22.data <-fread("Figure_22.csv")
figure.22.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.22.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(8,1),5))]
figure.22.data[,Truth:=as.factor(Truth)][,Truth:=reorder(Truth,c(rep(5,8),rep(4,8),rep(3,8),rep(2,8),rep(1,8)))]
figure.22 <- plot.column.dais(figure.22.data,Share,Statement,group.by=Truth,stacked=TRUE,
                              colours = set.colours(5,categorical.choice = c("grey","hot.pink","orange","light.blue","blue"))
) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))


figure.23.data <-fread("Figure_23.csv")
figure.23.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.23.data[,Type:=as.factor(Type)][,Type:=reorder(Type,c(rep(1,3),rep(2,18),rep(3,12),rep(4,9),rep(5,9),rep(6,9)))]
figure.23.data[,Category:=as.factor(Category)][,Category:=reorder(Category,c(rep(20,3),
                                                                             rep(19,3),
                                                                             rep(18,3),
                                                                             rep(17,3),
                                                                             rep(16,3),
                                                                             rep(15,3),
                                                                             rep(14,3),
                                                                             rep(13,3),
                                                                             rep(12,3),
                                                                             rep(11,3),
                                                                             rep(10,3),
                                                                             rep(9,3),
                                                                             rep(8,3),
                                                                             rep(7,3),
                                                                             rep(6,3),
                                                                             rep(5,3),
                                                                             rep(4,3),
                                                                             rep(3,3),
                                                                             rep(2,3),
                                                                             rep(1,3)))]
figure.23.data[,Correct:=as.factor(Correct)][,Correct:=reorder(Correct,rep(seq(3,1),20))]

figure.23 <- ggplot(figure.23.data,aes(Category,Share,fill=Correct)) + dais.base.theme() +
  geom_col(width=0.6,position="stack") +
  facet_grid(Type~.,switch = "y",scales = "free_y",space="free_y") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0.05,0.7)) +
  theme(panel.spacing = unit(0,"lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_blank()) +coord_flip() +
  scale_fill_manual(values = rev(set.colours(3,type="gradient")))

figure.25.data <-fread("Figure_25.csv")
figure.25.data <- figure.25.data[Belief!="Total"]
figure.25.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.25.data[,Platform:=as.factor(Platform)][,Platform:=reorder(Platform,rep(seq(17,1),3))]
figure.25.data[,Belief:=as.factor(Belief)][,Belief:=reorder(Belief,c(rep(3,17),rep(2,17),rep(1,17)))]
figure.25 <- plot.column.dais(figure.25.data,Share,Platform,group.by=Belief,stacked=FALSE,
                              colours = set.colours(3,categorical.choice = c("gold","black","hot.pink"))
) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))

figure.26.data <-fread("Figure_26.csv")
figure.26.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.26.data[,Source:=str_wrap(Source,30)][,Source:=as.factor(Source)][,Source:=reorder(Source,rep(seq(1,5),2))]
figure.26.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,c(rep(1,5),rep(2,5)))]
figure.26 <- plot.column.dais(figure.26.data,Share,Source,group.by=Statement,stacked=FALSE)

figure.27.data <-fread("Figure_27.csv")

figure.28.data <-fread("Figure_28.csv")
figure.28.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.28.data[,Statement:=str_wrap(Statement,40)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,rep(seq(12,1),6))]
figure.28.data[,Support:=as.factor(Support)][,Support:=reorder(Support,c(rep(6,12),rep(5,12),rep(4,12),rep(3,12),rep(2,12),rep(1,12)))]
figure.28 <- plot.column.dais(figure.28.data,Share,Statement,group.by=Support,stacked=TRUE,
                              colours = set.colours(6,categorical.choice = c("grey","hot.pink","orange","teal","light.blue","blue"))
) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))





figure.29.data <-fread("Figure_29.csv")
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
        strip.text.y.left  = element_text(angle=0)) +coord_flip() +
  scale_fill_manual(values = rev(set.colours(3,type="gradient")))



figure.30.data <-fread("Figure_30.csv")
figure.30.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.30.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,seq(1,4))]
figure.30 <- plot.column.dais(figure.30.data,Share,Statement,colours=set.colours(4,categorical.choice = c("hot.pink","gold","black","grey")))




figure.31.data <-fread("Figure_31.csv")
figure.31.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.31.data[,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,seq(1,6))]

figure.31 <- plot.column.dais(figure.31.data,Share,Statement,
                             colours=set.colours(6,categorical.choice = c("blue","light.blue","teal","orange","hot.pink","grey"))) +
  annotate("text",x=1.5,y=37,label="Total Support: 61%",family="Replica-Bold") +
  annotate("text",x=4.5,y=14,label="Total Oppose: 20%",family="Replica-Bold") +
  coord_cartesian(clip = 'off') +
  annotate("rect",xmin=0.5,xmax=2.5,ymin=0,ymax=40,alpha=1,colour=set.colours(1,categorical.choice = "blue"),fill=NA) +
  annotate("rect",xmin=3.5,xmax=5.5,ymin=0,ymax=17,alpha=1,colour=set.colours(1,categorical.choice = "hot.pink"),fill=NA)



figure.32.data <-fread("Figure_32.csv")
figure.32.data[,Share:=str_remove(Share,"%")][,Share:=as.numeric(Share)]
figure.32.data[,Statement:=str_wrap(Statement,30)][,Statement:=as.factor(Statement)][,Statement:=reorder(Statement,seq(1,4))]
figure.32 <- plot.column.dais(figure.32.data,Share,Statement,
                              colours=set.colours(4,categorical.choice = c("hot.pink","hot.pink","hot.pink","grey")))


