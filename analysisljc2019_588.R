library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(metR)

dtraw<-read.csv("./ljcdb.csv",stringsAsFactors = FALSE)
dtraw$D13<-as.numeric(dtraw$D13)
dt3438<-dtraw %>% filter(.,!is.na(D13),y1>=34)
dt2933<-dtraw %>% filter(.,!is.na(D13),y1>=29 & y1<=33 & x1>=49)
dt1725<-dtraw %>% filter(.,!is.na(D13),y1>=17 & y1<=25)
dt<-bind_rows(dt1725,dt2933,dt3438)
dt$D18<-as.numeric(dt$D18)
dt<-filter(dt,!is.na(D18))

#樣方總數
plotnum<-dt %>%
  distinct(x1,y1)
plotha<-nrow(plotnum)/100

#植株(主幹)新增&存活
dtb0<-dt %>%
  filter(.,b==0) %>%
  mutate(.,new97=ifelse(D91==-4 & D97>0,1,0)) %>%
  mutate(.,new05=ifelse(D97==-4 & D05>0,1,0)) %>%
  mutate(.,new13=ifelse(D05==-4 & D13>0,1,0)) %>%
  mutate(.,new18=ifelse(D13==-4 & D18>0,1,0)) %>%
  mutate(.,live91=ifelse(D91==0 | D91==-1 | D91==-4,0,1)) %>%
  mutate(.,live97=ifelse(D97==0 | D97==-1 | D97==-4,0,1)) %>%
  mutate(.,live05=ifelse(D05==0 | D05==-1 | D05==-4,0,1)) %>%
  mutate(.,live13=ifelse(D13==0 | D13==-1 | D13==-4,0,1)) %>%
  mutate(.,live18=ifelse(status==0 | status==-1,0,1))

#植株總數
dtstem<-dtb0 %>%
  summarise(.,stem97=sum(live97),stem05=sum(live05),stem13=sum(live13),stem18=sum(live18))

#各物種株數
dtstemsp<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(.,stem97=sum(live97),stem05=sum(live05),stem13=sum(live13),stem18=sum(live18)) %>%
  arrange(.,desc(stem18))

#植株密度(stems ha^-1)
dtstemha<-dtb0 %>%
  summarise(.,stem97=sum(live97)/plotha,stem05=sum(live05)/plotha,stem13=sum(live13)/plotha,stem18=sum(live18)/plotha)

#各物種植株密度(stems ha^-1)
dtstemspha<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(.,stem97=sum(live97)/plotha,stem05=sum(live05)/plotha,stem13=sum(live13)/plotha,stem18=sum(live18)/plotha) %>%
  arrange(.,desc(stem18))

#各物種相對密度
rdtstemspha<-dtstemspha %>%
  group_by(.,sp) %>%
  summarise(.,rstem97=stem97/sum(dtstemspha$stem97)*100,rstem05=stem05/sum(dtstemspha$stem05)*100,rstem13=stem13/sum(dtstemspha$stem13)*100,rstem18=stem18/sum(dtstemspha$stem18)*100) %>%
  arrange(.,sp)

#年新增率
newrate<-function(nf,ns,t){
  nrate<-(log((nf/ns),base = exp(1))/t)*100
  return(nrate)
}
alivestem<-dtb0 %>%
  mutate(.,alive2005=ifelse(live97==1 & live05==1,1,0),
            alive2013=ifelse(live05==1 & live13==1,1,0),
            alive2018=ifelse(live13==1 & live18==1,1,0))
newyearrange<-c("1997-2005","2005-2013","2013-2018")
newratedt<-c(newrate(sum(alivestem$live05),sum(alivestem$alive2005),8),
             newrate(sum(alivestem$live13),sum(alivestem$alive2013),8),
             newrate(sum(alivestem$live18),sum(alivestem$alive2018),5))
newratedf<-t(data.frame(newyearrange,年新增率=newratedt,row.names = 1))
newratedf


#胸高斷面積
dtba<-dt %>%
  mutate(.,ba91=ifelse(D91>=0,pi*(D91/100/2)^2,0)) %>%
  mutate(.,ba97=ifelse(D97>=0,pi*(D97/100/2)^2,0)) %>%
  mutate(.,ba05=ifelse(D05>=0,pi*(D05/100/2)^2,0)) %>%
  mutate(.,ba13=ifelse(D13>=0,pi*(D13/100/2)^2,0)) %>%
  mutate(.,ba18=ifelse(D18>=0,pi*(D18/100/2)^2,0))

#植株胸斷面積
dtbab0<-dtba %>%
  group_by(.,tag) %>%
  mutate(.,sumba97=sum(ba97),sumba05=sum(ba05),sumba13=sum(ba13),sumba18=sum(ba18)) %>%
  semi_join(.,dtb0)

#各物種胸高斷面積
dtbab0sp<-dtbab0 %>%
  group_by(.,sp) %>%
  summarise(.,sumba1997=sum(sumba97),sumba2005=sum(sumba05),sumba2013=sum(sumba13),sumba2018=sum(sumba18)) %>%
  arrange(.,sp)

#各物種相對胸高斷面積
rdtbasp<-dtbab0sp %>%
  group_by(.,sp) %>%
  summarise(.,ba97=sumba1997/sum(dtbab0sp$sumba1997)*100,ba05=sumba2005/sum(dtbab0sp$sumba2005)*100,ba13=sumba2013/sum(dtbab0sp$sumba2013)*100,ba18=sumba2018/sum(dtbab0sp$sumba2018)*100) %>%
  arrange(.,sp)

#單位胸高斷面積(m^2 ha^-1)
dtbaha<-dtba %>%
  filter(.,sp!="台灣山桂花" | sp!="松田東青" | sp!="臺灣馬錢" | sp!="瓊南") %>%
  summarise(.,allba97=sum(ba97)/plotha,allba05=sum(ba05)/plotha,allba13=sum(ba13)/plotha,allba18=sum(ba18)/plotha)

#各物種單位胸高斷面積(m^2 ha^-1)
dtbaspha<-dtba %>%
  group_by(.,sp) %>%
  summarise(.,allba97=sum(ba97)/plotha,allba05=sum(ba05)/plotha,allba13=sum(ba13)/plotha,allba18=sum(ba18)/plotha) %>%
  semi_join(.,dtbab0sp,by="sp") %>%
  arrange(.,sp)

#IVI
ivi<-bind_cols(rdtstemspha,rdtbasp) %>%
  select(.,-6) %>%
  mutate(.,ivi97=rstem97+ba97,ivi05=rstem05+ba05,ivi13=rstem13+ba13,ivi18=rstem18+ba18) %>%
  arrange(.,desc(ivi18))

#各物種新增植株數量
newstemsp<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(new2005=sum(new05),new2013=sum(new13),new2018=sum(new18),) %>%
  arrange(.,desc(new2018))

#新增植株數量變化圖
newyear<-c("2005","2013","2018")
newnum<-c(sum(dtb0$new05),sum(dtb0$new13),sum(dtb0$new18))
newdt<-data.frame(年份=newyear,新增植株數量=newnum)
newdt
newplot<-ggplot(newdt,aes(x=newyear,y=newnum)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x="年份",y="新增植株數量(株)") + 
  theme(axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8))
#ggsave(filename = "newstemnum.png",width = 10,height = 10,units = "cm")
newplot

#各物種死亡植株數量
deadstemsp<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(dead2005=sum(live97==1 & live05==0),dead2013=sum(live05==1 & live13==0),dead2018=sum(live13==1 & live18==0)) %>%
  arrange(.,desc(dead2018))

#存活植株數量變化圖
liveyear<-c("2005","2013","2018")
livenum<-c(sum(dtb0$live97==1 & dtb0$live05==1),sum(dtb0$live05==1 & dtb0$live13==1),sum(dtb0$live13==1 & dtb0$live18==1))
livedt<-data.frame(liveyear,livenum)
liveplot<-ggplot(livedt,aes(x=liveyear,y=livenum)) +
  geom_bar(stat = "identity") + 
  theme_classic() +
  labs(x="年份",y="植株數量(株)") + 
  theme(axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8))
#ggsave(filename = "livestemnum.png",width = 10,height = 10,units = "cm")
liveplot

#匯入環境資料
env<-read.csv("./欖仁溪環境資料.csv")


#各樣方植株數量
livegradient<-dtb0 %>%
  group_by(.,x1,y1) %>%
  summarise(.,livenum97=sum(live97),livenum05=sum(live05),livenum13=sum(live13),livenum18=sum(live18)) %>%
  arrange(.,y1,x1)

p<-ggplot(data = livegradient,aes(x=x1,y=y1,fill=livegradient$livenum18))+
  geom_raster(hjust = 1,vjust = 1)+
  scale_fill_gradient(low = "white",high = "black",name="植株數量(株)")+
  labs(x="Quadrat",y="Quadrat")+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "gray97"),
        axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = -1,size = 8))+
  # geom_contour(data = envf,aes(x=X,y=Y,z=envf$Eleva),color="black")+
  # geom_text_contour(data = envf,aes(x=X,y=Y,z = envf$Eleva),inherit.aes = FALSE,stroke = 0.1,size=3)+
  scale_y_continuous(breaks = seq(17,39,1),limits = c(17,39))+
  scale_x_continuous(breaks = seq(26,56,1))+
  coord_fixed()
#ggsave(filename = "livestemgradient2018.png",width = 20,height = 20,units = "cm")
p
#dev.off()

#各樣方胸高斷面積
bagradient<-dtbab0 %>%
  group_by(.,x1,y1) %>%
  summarise(.,sumba1997=sum(sumba97),sumba2005=sum(sumba05),sumba2013=sum(sumba13),sumba2018=sum(sumba18)) %>%
  arrange(.,y1,x1)

pba<-ggplot(data = bagradient,aes(x=x1,y=y1,fill=bagradient$sumba2018))+
  geom_raster(hjust = 1,vjust = 1)+
  scale_fill_gradient(low = "white",high = "black",name=expression("BA(m"^"2"*")"))+
  labs(x="Quadrat",y="Quadrat")+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "gray97"),
        axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0,size = 8))+
  # geom_contour(data = envf,aes(x=X,y=Y,z=envf$Eleva),color="black")+
  # geom_text_contour(data = envf,aes(x=X,y=Y,z = envf$Eleva),inherit.aes = FALSE,stroke = 0.1,size=3)+
  scale_y_continuous(breaks = seq(17,39,1),limits = c(17,39))+
  scale_x_continuous(breaks = seq(26,56,1))+
  coord_fixed()
#ggsave(filename = "bagradient2018.png",width = 20,height = 20,units = "cm")
pba
#dev.off()

#地形圖
pmap<-ggplot(data = env,aes(x=X,y=Y,z=env$Eleva))+
  geom_contour(color="black")+
  labs(x="Quadrat",y="Quadrat")+
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8))+
  geom_text_contour(data = env,aes(x=X,y=Y,z = env$Eleva),inherit.aes = FALSE,stroke = 0.3,size=3)+
  scale_y_continuous(breaks = seq(17,39,1),limits = c(17,39))+
  scale_x_continuous(breaks = seq(26,56,1))+
  coord_fixed()
#ggsave(filename = "ljccontour.png",width = 20,height = 20,units = "cm")
pmap
#dev.off()


