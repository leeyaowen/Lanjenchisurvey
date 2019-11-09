library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(metR)
library(reshape2)
library(vegan)
library(stringr)
library(sqldf)
# library(adespatial)

# 開始 ---------------------------------------------------------------------
dttype<-"all"
dtraw<-read.csv("./All300.csv",stringsAsFactors = FALSE)
splist<-read.csv("./欖仁溪名錄.csv",stringsAsFactors = FALSE)
# dtraw<-read.csv("./ljcdb.csv",stringsAsFactors = FALSE)
# dtraw$D13<-as.numeric(dtraw$D13)
dt<-dtraw %>% filter(.,!is.na(D13),y1>=29)
# dt33<-dtraw %>% filter(.,!is.na(D13),y1==33 & (x1==36 | x1>=38))
# dt32<-dtraw %>% filter(.,!is.na(D13),y1==32 & x1>=36)
# dt31<-dtraw %>% filter(.,!is.na(D13),y1==31 & ((x1>=36 & x1<=39) | x1>=41))
# dt30<-dtraw %>% filter(.,!is.na(D13),y1==30 & ((x1>=36 & x1<=40) | x1>=42))
# dt29<-dtraw %>% filter(.,!is.na(D13),y1==29 & x1>=36)
# dt28<-dtraw %>% filter(.,!is.na(D13),y1==28 & x1>=37)
# dt27<-dtraw %>% filter(.,!is.na(D13),y1==27 & x1>=37)
# dt26<-dtraw %>% filter(.,!is.na(D13),y1==26 & (x1==26 | x1>=39))
# dt1725<-dtraw %>% filter(.,!is.na(D13),y1<=25)
# dt<-bind_rows(dt3438,dt33,dt32,dt31,dt30,dt29,dt28,dt27,dt26,dt1725)
dt$D18<-as.numeric(dt$D18)
dt<-filter(dt,!is.na(D18) & sp!="un" & sp!="" & sp!="UNKNOWN" & sp!="野牡丹藤" & sp!="臺灣雞屎樹" & sp!="三刈葉" & sp!="校力坪鐵色" & sp!="南嶺蕘花" & sp!="石子木") %>%
  mutate(.,sp=ifelse(sp=="台灣蕘花","臺灣蕘花",sp),
         sp=ifelse(sp=="台灣楊桐","臺灣楊桐",sp),
         sp=ifelse(sp=="杏葉石礫","杏葉石櫟",sp),
         sp=ifelse(sp=="白匏仔","白匏子",sp),
         sp=ifelse(sp=="南仁鐵色","交力坪鐵色",sp),
         sp=ifelse(sp=="南仁灰木","希蘭灰木",sp),
         sp=ifelse(sp=="紅花八角","臺灣八角",sp),
         sp=ifelse(sp=="長尾栲","長尾尖葉櫧",sp),
         sp=ifelse(sp=="廣東瓊楠","華河瓊楠",sp),
         sp=ifelse(sp=="松田冬青","松田氏冬青",sp),
         sp=ifelse(sp=="嶺南椆","嶺南青剛櫟",sp),
         sp=ifelse(sp=="江某","鵝掌柴",sp),
         sp=ifelse(sp=="鐵雨傘","雨傘仔",sp),
         sp=ifelse(sp=="倒卵葉楠","恆春楨楠",sp),
         sp=ifelse(sp=="紅楠","豬腳楠",sp),
         sp=ifelse(sp=="黑星櫻","墨點櫻桃",sp),
         sp=ifelse(sp=="金平冬青","倒卵葉冬青",sp),
         sp=ifelse(sp=="臺灣柯" | sp=="台灣柯","臺灣石櫟",sp),
         sp=ifelse(sp=="銹葉野牡丹","大野牡丹",sp),
         sp=ifelse(sp=="水同木","豬母乳",sp),
         sp=ifelse(sp=="短尾柯","短尾葉石櫟",sp),
         sp=ifelse(sp=="長葉厚殼樹","長花厚殼樹",sp),
         sp=ifelse(sp=="臺灣梭羅木","臺灣梭羅樹",sp),
         sp=ifelse(sp=="印度栲","印度苦櫧",sp),
         sp=ifelse(sp=="山漆","木蠟樹",sp),
         sp=ifelse(sp=="海南厚殼桂","土楠",sp),
         sp=ifelse(sp=="茄苳","茄冬",sp),
         sp=ifelse(sp=="恆春紫珠","疏齒紫珠",sp),
         sp=ifelse(sp=="島榕","白肉榕",sp),
         sp=ifelse(sp=="臺灣栲","臺灣苦櫧",sp),
         sp=ifelse(sp=="金斗椆","捲斗櫟",sp),
         sp=ifelse(sp=="小梗木薑子","黃肉樹",sp),
         sp=ifelse(sp=="三叉虎","三腳虌",sp)) %>%
  mutate(.,ba91=ifelse(D91>=0,pi*(D91/100/2)^2,0)) %>%
  mutate(.,ba97=ifelse(D97>=0,pi*(D97/100/2)^2,0)) %>%
  mutate(.,ba05=ifelse(D05>=0,pi*(D05/100/2)^2,0)) %>%
  mutate(.,ba13=ifelse(D13>=0,pi*(D13/100/2)^2,0)) %>%
  mutate(.,ba18=ifelse(D18>=0,pi*(D18/100/2)^2,0)) %>%
  group_by(.,tag) %>%
  mutate(.,sumba91=sum(ba91),sumba97=sum(ba97),sumba05=sum(ba05),sumba13=sum(ba13),sumba18=sum(ba18)) %>%
  ungroup(.) %>%
  left_join(.,splist,by=c("sp"="中名"))

# 資料分型 ---------------------------------------------------------------------
dtsplit<-read.csv("./LanjenchiHabitat_588.csv",stringsAsFactors = F)

dt$x1<-as.character(dt$x1)
dt$y1<-as.character(dt$y1)
dt<-dt %>%
  mutate(.,x1y1=paste0(x1,y1))
dt$x1y1<-as.integer(dt$x1y1)
dt<-left_join(dt,dtsplit,by=c("x1y1"="quadrat"))
dtwind<-dt %>%
  filter(.,habitat==0)
dtvalley<-dt %>%
  filter(.,habitat==1)


# 植株(主幹)新增&存活 ---------------------------------------------------------------------
dtb0fun<-function(type="all"){
  if(type=="all"){
    dtb0<-dt %>%
      filter(.,b==0) %>%
      mutate(.,new97=ifelse(D91==-4 & D97>0,1,0)) %>%
      mutate(.,new05=ifelse(D97==-4 & D05>0,1,0)) %>%
      mutate(.,new13=ifelse(D05==-4 & D13>0,1,0)) %>%
      mutate(.,new18=ifelse(D13==-4 & D18>0,1,0)) %>%
      mutate(.,live91=ifelse(D91==0 | D91==-1 | D91==-4 | D91==-5,0,1)) %>%
      mutate(.,live97=ifelse(D97==0 | D97==-1 | D97==-4 | D97==-5,0,1)) %>%
      mutate(.,live05=ifelse(D05==0 | D05==-1 | D05==-4 | D05==-5,0,1)) %>%
      mutate(.,live13=ifelse(D13==0 | D13==-1 | D13==-4 | D13==-5,0,1)) %>%
      mutate(.,live18=ifelse(status==0 | status==-1,0,1))
    assign("dtb0",value = dtb0,envir = .GlobalEnv)
    plotnum<-dt %>%
      distinct(x1,y1)
    plotha<-nrow(plotnum)/100
    assign("plotha",value = plotha,envir = .GlobalEnv)
  }else if(type=="wind"){
    dtb0<-dtwind %>%
      filter(.,b==0) %>%
      mutate(.,new97=ifelse(D91==-4 & D97>0,1,0)) %>%
      mutate(.,new05=ifelse(D97==-4 & D05>0,1,0)) %>%
      mutate(.,new13=ifelse(D05==-4 & D13>0,1,0)) %>%
      mutate(.,new18=ifelse(D13==-4 & D18>0,1,0)) %>%
      mutate(.,live91=ifelse(D91==0 | D91==-1 | D91==-4 | D91==-5,0,1)) %>%
      mutate(.,live97=ifelse(D97==0 | D97==-1 | D97==-4 | D97==-5,0,1)) %>%
      mutate(.,live05=ifelse(D05==0 | D05==-1 | D05==-4 | D05==-5,0,1)) %>%
      mutate(.,live13=ifelse(D13==0 | D13==-1 | D13==-4 | D13==-5,0,1)) %>%
      mutate(.,live18=ifelse(status==0 | status==-1,0,1))
    assign("dtb0",value = dtb0,envir = .GlobalEnv)
    plotnum<-dtwind %>%
      distinct(x1y1)
    plotha<-nrow(plotnum)/100
    assign("plotha",value = plotha,envir = .GlobalEnv)
  }else if(type=="valley"){
    dtb0<-dtvalley %>%
      filter(.,b==0) %>%
      mutate(.,new97=ifelse(D91==-4 & D97>0,1,0)) %>%
      mutate(.,new05=ifelse(D97==-4 & D05>0,1,0)) %>%
      mutate(.,new13=ifelse(D05==-4 & D13>0,1,0)) %>%
      mutate(.,new18=ifelse(D13==-4 & D18>0,1,0)) %>%
      mutate(.,live91=ifelse(D91==0 | D91==-1 | D91==-4 | D91==-5,0,1)) %>%
      mutate(.,live97=ifelse(D97==0 | D97==-1 | D97==-4 | D97==-5,0,1)) %>%
      mutate(.,live05=ifelse(D05==0 | D05==-1 | D05==-4 | D05==-5,0,1)) %>%
      mutate(.,live13=ifelse(D13==0 | D13==-1 | D13==-4 | D13==-5,0,1)) %>%
      mutate(.,live18=ifelse(status==0 | status==-1,0,1))
    assign("dtb0",value = dtb0,envir = .GlobalEnv)
    plotnum<-dtvalley %>%
      distinct(x1,y1)
    plotha<-nrow(plotnum)/100
    assign("plotha",value = plotha,envir = .GlobalEnv)
  }else{
    
  }
}
dtb0fun(dttype)

# 植株總數 ---------------------------------------------------------------------
dtstem<-dtb0 %>%
  summarise(.,stem91=sum(live91),stem97=sum(live97),stem05=sum(live05),stem13=sum(live13),stem18=sum(live18))

# 各樣方植株總數/TBI ---------------------------------------------------------------------
splistdis<-distinct(dtb0,sp) %>%
  arrange(.,sp)
plotspnum1991<-dtb0 %>%
  mutate(.,x1y1=paste0(x1,y1)) %>%
  dcast(formula = x1y1~sp,value.var = "live91",fun.aggregate = sum)
rownames(plotspnum1991)<-plotspnum1991[,1]
plotspnum1991<-plotspnum1991[,-1]
plotspnum1991<-decostand(plotspnum1991,method = "hellinger")

plotspnum1997<-dtb0 %>%
  mutate(.,x1y1=paste0(x1,y1)) %>%
  dcast(formula = x1y1~sp,value.var = "live97",fun.aggregate = sum)
rownames(plotspnum1997)<-plotspnum1997[,1]
plotspnum1997<-plotspnum1997[,-1]
plotspnum1997<-decostand(plotspnum1997,method = "hellinger")

plotspnum2005<-dtb0 %>%
  mutate(.,x1y1=paste0(x1,y1)) %>%
  dcast(formula = x1y1~sp,value.var = "live05",fun.aggregate = sum)
rownames(plotspnum2005)<-plotspnum2005[,1]
plotspnum2005<-plotspnum2005[,-1]
plotspnum2005<-decostand(plotspnum2005,method = "hellinger")

plotspnum2013<-dtb0 %>%
  mutate(.,x1y1=paste0(x1,y1)) %>%
  dcast(formula = x1y1~sp,value.var = "live13",fun.aggregate = sum)
rownames(plotspnum2013)<-plotspnum2013[,1]
plotspnum2013<-plotspnum2013[,-1]
plotspnum2013<-decostand(plotspnum2013,method = "hellinger")

plotspnum2019<-dtb0 %>%
  mutate(.,x1y1=paste0(x1,y1)) %>%
  dcast(formula = x1y1~sp,value.var = "live18",fun.aggregate = sum)
rownames(plotspnum2019)<-plotspnum2019[,1]
plotspnum2019<-plotspnum2019[,-1]
plotspnum2019<-decostand(plotspnum2019,method = "hellinger")

# resTBI<-TBI(plotspnum1997[1:nrow(plotspnum1997),], plotspnum2019[1:nrow(plotspnum2019),],
#             method="%diff", nperm=999,test.BC=TRUE, test.t.perm=TRUE, clock = T)
# resTBI$BCD.summary
# #png("背風型BC.png",width=400,height=400,unit="px")
# plot(resTBI,xlim = c(-0.1,0.6),ylim = c(-0.1,0.6))
# axis(1,at=seq(-0.1,0.6,0.1))
# axis(2,at=seq(-0.1,0.6,0.1))
# #dev.off()

# resmodel5df<-bind_rows(plotspnum1997,plotspnum2019)
# resmodel5<-stimodels(resmodel5df,S=214,Ti=2)
# 
resmodel5df<-bind_rows(plotspnum1997,plotspnum2005,plotspnum2013,plotspnum2019)
resmodel5dft<-t(resmodel5df)
resmodel5dft<-as.data.frame(t(resmodel5df))
resmodel5dft %<>% mutate(.,sp=rownames(.))
dtstemsp %<>% filter(.,stem97!=0 | stem05!=0 | stem13!=0 | stem18 !=0)
df<-inner_join(resmodel5dft,dtstemsp,by="sp")
rownames(df)<-df[,2353]
df<-df[,-c(2353:2357)]
df<-as.data.frame(t(df),row.names=F)

codt<-matrix(data = c(rep(1997,588),rep(2005,588),rep(2013,588),rep(2019,588)))
resmodel5<-stimodels(df,S=588,Ti=4,nperm = 99,COD.T = codt)

A<-gl(4,588)
helm4<-model.matrix(~A,contrasts=list(A="contr.helmert"))[,-1]
resmodel5<-stimodels(df,S=588,Ti=4,model="5",nperm = 99,COD.T = helm4)


# 各物種株數 ---------------------------------------------------------------------
dtstemsp<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(.,stem91=sum(live91),stem97=sum(live97),stem05=sum(live05),stem13=sum(live13),stem18=sum(live18)) %>%
  arrange(.,sp)


# 科屬種數 ---------------------------------------------------------------------
dtspeciesnum<-dtstemsp %>%
  summarise(.,sp91=sum(stem91),sp97=sum(stem97>0),sp05=sum(stem05>0),sp13=sum(stem13>0),sp18=sum(stem18>0))
dtstemfamily<-dtb0 %>%
  group_by(.,科名) %>%
  summarise(.,stem91=sum(live91),stem97=sum(live97),stem05=sum(live05),stem13=sum(live13),stem18=sum(live18)) %>%
  filter(.,stem91>0 | stem97>0 | stem05>0 | stem13>0 | stem18>0)
dtfamilynum<-dtstemfamily %>%
  summarise(.,family91=sum(stem91>0),family97=sum(stem97>0),family05=sum(stem05>0),family13=sum(stem13>0),family18=sum(stem18>0))
dtstemgenus<-dtb0 %>%
  group_by(.,Genus) %>%
  summarise(.,stem91=sum(live91),stem97=sum(live97),stem05=sum(live05),stem13=sum(live13),stem18=sum(live18)) %>%
  filter(.,stem91 | stem97>0 | stem05>0 | stem13>0 | stem18>0)
dtgenusnum<-dtstemgenus %>%
  summarise(.,genus91=sum(stem91>0),genus97=sum(stem97>0),genus05=sum(stem05>0),genus13=sum(stem13>0),genus18=sum(stem18>0))



# 生物多樣性指數 ---------------------------------------------------------------------
diver<-data.frame(t(dtstemsp),stringsAsFactors = F)
colnames(diver)<-diver[1,]
diveryear=c("1991","1997","2005","2013","2019")
diversimpson<-c(indexsimpson91=diversity(as.numeric(diver[-c(1,3:6),]),index = "simpson"),
                indexsimpson97=diversity(as.numeric(diver[-c(1:2,4:6),]),index = "simpson"),
                indexsimpson05=diversity(as.numeric(diver[-c(1:3,5:6),]),index = "simpson"),
                indexsimpson13=diversity(as.numeric(diver[-c(1:4,6),]),index = "simpson"),
                indexsimpson18=diversity(as.numeric(diver[-c(1:5),]),index = "simpson"))
divershannon<-c(indexshannon91=diversity(as.numeric(diver[-c(1,3:6),]),index = "shannon"),
                indexshannon97=diversity(as.numeric(diver[-c(1:2,4:6),]),index = "shannon"),
                indexshannon05=diversity(as.numeric(diver[-c(1:3,5:6),]),index = "shannon"),
                indexshannon13=diversity(as.numeric(diver[-c(1:4,6),]),index = "shannon"),
                indexshannon18=diversity(as.numeric(diver[-c(1:5),]),index = "shannon"))
diverdf<-data.frame("年分"=diveryear,"indexsimpson"=diversimpson,"indexshannon"=divershannon,row.names=1)


# 植株密度(stems ha^-1) ---------------------------------------------------------------------
dtstemha<-dtb0 %>%
  summarise(.,stem91=sum(live91)/plotha,stem97=sum(live97)/plotha,stem05=sum(live05)/plotha,stem13=sum(live13)/plotha,stem18=sum(live18)/plotha)


# 各物種植株密度(stems ha^-1) ---------------------------------------------------------------------
dtstemspha<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(.,stem91=sum(live91)/plotha,stem97=sum(live97)/plotha,stem05=sum(live05)/plotha,stem13=sum(live13)/plotha,stem18=sum(live18)/plotha) %>%
  arrange(.,sp)


# 各物種相對密度 ---------------------------------------------------------------------
rdtstemspha<-dtstemspha %>%
  group_by(.,sp) %>%
  summarise(.,rstem91=stem91/sum(dtstemspha$stem91)*100,rstem97=stem97/sum(dtstemspha$stem97)*100,rstem05=stem05/sum(dtstemspha$stem05)*100,rstem13=stem13/sum(dtstemspha$stem13)*100,rstem18=stem18/sum(dtstemspha$stem18)*100) %>%
  arrange(.,sp)


# 年新增率 ---------------------------------------------------------------------
newrate<-function(nf,ns,t){
  nrate<-(log((nf/ns),base = exp(1))/t)*100
  return(nrate)
}
survivestem<-dtb0 %>%
  mutate(.,survive1997=ifelse(live91==1 & live97==1,1,0),
          survive2005=ifelse(live97==1 & live05==1,1,0),
          survive2013=ifelse(live05==1 & live13==1,1,0),
          survive2019=ifelse(live13==1 & live18==1,1,0))
newyearrange<-c("1991-1997","1997-2005","2005-2013","2013-2019")
newratedt<-c(newrate(sum(survivestem$live97),sum(survivestem$survive1997),6),
             newrate(sum(survivestem$live05),sum(survivestem$survive2005),8),
             newrate(sum(survivestem$live13),sum(survivestem$survive2013),8),
             newrate(sum(survivestem$live18),sum(survivestem$survive2019),6))
newratedf<-t(data.frame(newyearrange,年新增率=newratedt,row.names = 1))
newratedf


# 年死亡率 ---------------------------------------------------------------------
deadratem<-function(ns,n0,t){
  dratem<-(1-(ns/n0)^(1/t))*100
  return(dratem)
}
deadratelambda<-function(ns,n0,t){
  dratelambda<-(log((n0/ns),base = exp(1))/t)*100
  return(dratelambda)
}
survivestem<-dtb0 %>%
  mutate(.,survive1997=ifelse(live91==1 & live97==1,1,0),
         survive2005=ifelse(live97==1 & live05==1,1,0),
         survive2013=ifelse(live05==1 & live13==1,1,0),
         survive2019=ifelse(live13==1 & live18==1,1,0))
deadyearrange<-c("1991-1997","1997-2005","2005-2013","2013-2019")
deadratemdt<-c(deadratem(sum(survivestem$survive1997),sum(survivestem$live91),6),
               deadratem(sum(survivestem$survive2005),sum(survivestem$live97),8),
               deadratem(sum(survivestem$survive2013),sum(survivestem$live05),8),
               deadratem(sum(survivestem$survive2019),sum(survivestem$live13),6))
deadratemdf<-t(data.frame(deadyearrange,"年死亡率(m)"=deadratemdt,row.names = 1,check.names=F))
deadratemdf

deadratelambdadt<-c(deadratelambda(sum(survivestem$survive1997),sum(survivestem$live91),6),
                    deadratelambda(sum(survivestem$survive2005),sum(survivestem$live97),8),
                    deadratelambda(sum(survivestem$survive2013),sum(survivestem$live05),8),
                    deadratelambda(sum(survivestem$survive2019),sum(survivestem$live13),6))
deadratelambdadf<-t(data.frame(deadyearrange,"年死亡率(λ)"=deadratelambdadt,row.names = 1,check.names=F))
deadratelambdadf


# 變動率 ---------------------------------------------------------------------
changerate<-function(n0,nt,t) {
  crate<-((log(n0,base = 10)-log(nt,base = 10))/t)*100
  return(crate)
}
changeyearrange<-c("1991-1997","1997-2005","2005-2013","2013-2019")
changeratedt<-c(changerate(sum(dtb0$live91),sum(dtb0$live97),6),
                changerate(sum(dtb0$live97),sum(dtb0$live05),8),
                changerate(sum(dtb0$live05),sum(dtb0$live13),8),
                changerate(sum(dtb0$live13),sum(dtb0$live18),6))
changeratedf<-data.frame(changeyearrange,changeratedt)
changeratedf


# 胸高斷面積總和 ---------------------------------------------------------------------
bayear<-c("1991","1997","2005","2013","2019")
basumdt<-c(sum(dtb0$sumba91),sum(dtb0$sumba97),sum(dtb0$sumba05),sum(dtb0$sumba13),sum(dtb0$sumba18))
basumdf<-data.frame(bayear,basumdt)


# 各物種胸高斷面積 ---------------------------------------------------------------------
dtbab0sp<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(.,sumba1991=sum(sumba91),sumba1997=sum(sumba97),sumba2005=sum(sumba05),sumba2013=sum(sumba13),sumba2018=sum(sumba18)) %>%
  arrange(.,sp)


# 各物種相對胸高斷面積 ---------------------------------------------------------------------
rdtbasp<-dtbab0sp %>%
  group_by(.,sp) %>%
  summarise(.,ba91=sumba1991/sum(dtbab0sp$sumba1991)*100,ba97=sumba1997/sum(dtbab0sp$sumba1997)*100,ba05=sumba2005/sum(dtbab0sp$sumba2005)*100,ba13=sumba2013/sum(dtbab0sp$sumba2013)*100,ba18=sumba2018/sum(dtbab0sp$sumba2018)*100) %>%
  arrange(.,sp)


# 單位胸高斷面積(m^2 ha^-1) ---------------------------------------------------------------------
dtbaha<-basumdf %>%
  mutate(.,basumdtha=basumdt/plotha)


# 各物種單位胸高斷面積(m^2 ha^-1) ---------------------------------------------------------------------
dtbaspha<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(.,allba91=sum(sumba91)/plotha,allba97=sum(sumba97)/plotha,allba05=sum(sumba05)/plotha,allba13=sum(sumba13)/plotha,allba18=sum(sumba18)/plotha) %>%
  ungroup(.) %>%
  arrange(.,sp)


# IVI ---------------------------------------------------------------------
iv<-bind_cols(dtstemspha,dtbaspha,rdtstemspha,rdtbasp) %>%
  mutate(.,iv91=(rstem91+ba91)/2,iv97=(rstem97+ba97)/2,iv05=(rstem05+ba05)/2,iv13=(rstem13+ba13)/2,iv19=(rstem18+ba18)/2) %>%
  select(.,-c(7,13:24)) %>%
  left_join(.,splist,by=c("sp"="中名")) %>%
  select(.,1,20,2:16)
#write.csv(iv,file = "物種組成背風型.csv",row.names=F)


# 死亡底面積 ---------------------------------------------------------------------
dtdeadbaha<-dtb0 %>%
  mutate(.,dba97=ifelse(live91==1 & live97==0,sumba97-sumba91,0)) %>%
  mutate(.,dba05=ifelse(live97==1 & live05==0,sumba05-sumba97,0)) %>%
  mutate(.,dba13=ifelse(live05==1 & live13==0,sumba13-sumba05,0)) %>%
  mutate(.,dba18=ifelse(live13==1 & live18==0,sumba18-sumba13,0)) %>%
  summarise(.,deadba97=sum(dba97)/plotha,deadba05=sum(dba05)/plotha,deadba13=sum(dba13)/plotha,deadba18=sum(dba18)/plotha)


# 各物種新增植株數量 ---------------------------------------------------------------------
newstemsp<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(new1997=sum(new97),new2005=sum(new05),new2013=sum(new13),new2019=sum(new18)) %>%
  arrange(.,desc(new2019))


# 各物種死亡植株數量 ---------------------------------------------------------------------
deadstemsp<-dtb0 %>%
  group_by(.,sp) %>%
  summarise(dead1997=sum(live91==1 & live97==0),dead2005=sum(live97==1 & live05==0),dead2013=sum(live05==1 & live13==0),dead2019=sum(live13==1 & live18==0)) %>%
  arrange(.,desc(dead2019))


# 年平均直徑生長 ---------------------------------------------------------------------
survive2005<-dtb0 %>%
  filter(.,sumba97>0 & sumba05>0) %>%
  mutate(.,fdbh97=sqrt(sumba97/pi)*2*100,fdbh05=sqrt(sumba05/pi)*2*100) %>%
  mutate(.,difdbh=fdbh05-fdbh97)
survive2013<-dtb0 %>%
  filter(.,sumba05>0 & sumba13>0) %>%
  mutate(.,fdbh05=sqrt(sumba05/pi)*2*100,fdbh13=sqrt(sumba13/pi)*2*100) %>%
  mutate(.,difdbh=fdbh13-fdbh05)
survive2018<-dtb0 %>%
  filter(.,sumba13>0 & sumba18>0) %>%
  mutate(.,fdbh13=sqrt(sumba13/pi)*2*100,fdbh18=sqrt(sumba18/pi)*2*100) %>%
  mutate(.,difdbh=fdbh18-fdbh13)
incredbhyear<-c("1997-2005","2005-2013","2013-2019")
incredbhdt<-c(sum(survive2005$difdbh)/nrow(survive2005)/8,
              sum(survive2013$difdbh)/nrow(survive2013)/8,
              sum(survive2018$difdbh)/nrow(survive2018)/6)
incredbhdf<-data.frame(incredbhyear,incredbhdt)


survive2005list<-sqldf("select distinct sp from dtb0 where sumba97>0 and sumba05>0 order by sp")
incredbhdf2005<-data.frame()
for (i in 1:nrow(survive2005list)) {
  survive2005loop<-dtb0 %>%
    filter(.,sumba97>0 & sumba05>0) %>%
    filter(.,sp==survive2005list[i,1]) %>%
    mutate(.,fdbh97=sqrt(sumba97/pi)*2*100,fdbh05=sqrt(sumba05/pi)*2*100) %>%
    mutate(.,difdbh=fdbh05-fdbh97)
  incredbhdt<-sum(survive2005loop$difdbh)/nrow(survive2005loop)/8
  incredbh2005<-data.frame(sp=survive2005list[i,1],cm2005=incredbhdt,stringsAsFactors = F)
  incredbhdf2005<-bind_rows(incredbhdf2005,incredbh2005)
  print(paste0("加入",survive2005list[i,1]))
}

survive2013list<-sqldf("select distinct sp from dtb0 where sumba05>0 and sumba13>0 order by sp")
incredbhdf2013<-data.frame()
for (i in 1:nrow(survive2013list)) {
  survive2013loop<-dtb0 %>%
    filter(.,sumba05>0 & sumba13>0) %>%
    filter(.,sp==survive2013list[i,1]) %>%
    mutate(.,fdbh05=sqrt(sumba05/pi)*2*100,fdbh13=sqrt(sumba13/pi)*2*100) %>%
    mutate(.,difdbh=fdbh13-fdbh05)
  incredbhdt<-sum(survive2013loop$difdbh)/nrow(survive2013loop)/8
  incredbh2013<-data.frame(sp=survive2013list[i,1],cm2013=incredbhdt,stringsAsFactors = F)
  incredbhdf2013<-bind_rows(incredbhdf2013,incredbh2013)
  print(paste0("加入",survive2013list[i,1]))
}

survive2018list<-sqldf("select distinct sp from dtb0 where sumba13>0 and sumba18>0 order by sp")
incredbhdf2018<-data.frame()
for (i in 1:nrow(survive2018list)) {
  survive2018loop<-dtb0 %>%
    filter(.,sumba13>0 & sumba18>0) %>%
    filter(.,sp==survive2018list[i,1]) %>%
    mutate(.,fdbh13=sqrt(sumba13/pi)*2*100,fdbh18=sqrt(sumba18/pi)*2*100) %>%
    mutate(.,difdbh=fdbh18-fdbh13)
  incredbhdt<-sum(survive2018loop$difdbh)/nrow(survive2018loop)/6
  incredbh2018<-data.frame(sp=survive2018list[i,1],cm2018=incredbhdt,stringsAsFactors = F)
  incredbhdf2018<-bind_rows(incredbhdf2018,incredbh2018)
  print(paste0("加入",survive2018list[i,1]))
}

incredbh0513<-full_join(incredbhdf2005,incredbhdf2013,by="sp")
incredbh0518<-full_join(incredbh0513,incredbhdf2018,by="sp") %>%
  arrange(.,desc(cm2018))
#write.csv(incredbh0518,file = "整體物種生長量.csv",row.names=F)


# 徑級結構 --------------------------------------------------------------------
fba<-dtb0 %>%
  mutate(.,fdbh97=sqrt(sumba97/pi)*2*100,fdbh05=sqrt(sumba05/pi)*2*100,fdbh13=sqrt(sumba13/pi)*2*100,fdbh18=sqrt(sumba18/pi)*2*100)

sp50_1997<-dtstemsp %>%
  filter(.,stem97>=50) %>%
  mutate(.,sp=ifelse(sp=="白?","白臼",sp))
sp50_2005<-dtstemsp %>%
  filter(.,stem05>=50) %>%
  mutate(.,sp=ifelse(sp=="白?","白臼",sp))
sp50_2013<-dtstemsp %>%
  filter(.,stem13>=50) %>%
  mutate(.,sp=ifelse(sp=="白?","白臼",sp))
sp50_2018<-dtstemsp %>%
  filter(.,stem18>=50) %>%
  mutate(.,sp=ifelse(sp=="白?","白臼",sp))
sp50list<-list(sp50_1997,sp50_2005,sp50_2013,sp50_2018)
sp50year=c("1997","2005","2013","2019")
sp50plotf<-fba %>%
  mutate(.,sp=ifelse(sp=="白?","白臼",sp))

for (j in 1:length(sp50list)) {
  for (i in 1:nrow(sp50list[[j]])) {
    sp50plot<-sp50plotf %>%
      filter(.,sp==as.character(sp50list[[j]][i,1]))
    m<-as.integer(5*log10(as.integer(sp50list[[j]][i,j+1])))
    if(sp50year[j]=="1997"){
      sp50plot<-sp50plot %>% filter(.,fdbh97>0)
      p50<-as.data.frame(table(cut(sp50plot$fdbh97,breaks = m)),stringsAsFactors = F)
    }else if(sp50year[j]=="2005"){
      sp50plot<-sp50plot %>% filter(.,fdbh05>0)
      p50<-as.data.frame(table(cut(sp50plot$fdbh05,breaks = m)),stringsAsFactors = F)
    }else if(sp50year[j]=="2013"){
      sp50plot<-sp50plot %>% filter(.,fdbh13>0)
      p50<-as.data.frame(table(cut(sp50plot$fdbh13,breaks = m)),stringsAsFactors = F)
    }else if(sp50year[j]=="2019"){
      sp50plot<-sp50plot %>% filter(.,fdbh18>0)
      p50<-as.data.frame(table(cut(sp50plot$fdbh18,breaks = m)),stringsAsFactors = F)
    }else{
      
    }
    p50strslipt<-data.frame(str_split_fixed(p50[,1],",",2),stringsAsFactors = F)
    colnames(p50strslipt)[1:2]<-c("lower","upper")
    p50strslipt[,1]<-str_sub(p50strslipt[,1],2,-1)
    p50strslipt[,2]<-str_sub(p50strslipt[,2],1,-2)
    p50<-bind_cols(p50,p50strslipt)
    p50$lower<-as.numeric(p50$lower)
    p50$upper<-as.numeric(p50$upper)
    spname<-as.character(sp50list[[j]][i,1])
    p<-ggplot(data = p50,aes(x=lower,y=Freq))+
      geom_bar(stat="identity")+
      labs(x="DBH(cm)",y="株數(株)",title = paste0(sp50year[j],as.character(sp50list[[j]][i,1]),"徑級結構(背風型生育地)"))+
      theme_classic()+
      scale_x_continuous(breaks = round(p50$lower,digits = 2))
    if(Encoding(as.character(sp50list[[j]][i,1]))=="UTF-8"){
      spname<-iconv(as.character(sp50list[[j]][i,1]),"UTF-8","CP950")
    }else{
      
    }
    ggsave(filename = paste0(sp50year[j],spname,"徑級結構(背風型).png"),width = 20,height = 20,units = "cm")
    print(paste0(sp50year[j],as.character(sp50list[[j]][i,1]),"徑級結構"))
  }
}


# 新增植株數量變化圖 ---------------------------------------------------------------------
newyear<-c("2005","2013","2019")
newnum<-c(sum(dtb0$new05),sum(dtb0$new13),sum(dtb0$new18))
newdt<-data.frame(newyear,newnum)
newdt
newplot<-ggplot(newdt,aes(x=newyear,y=newnum)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x="年份",y="新增植株數量(株)") + 
  theme(axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8))
#ggsave(filename = "newstemnum.png",width = 10,height = 10,units = "cm")
newplot


# 死亡植株數量圖 ---------------------------------------------------------------------
deadyear<-c("2005","2013","2019")
deadnum<-c(sum(dtb0$live97==1 & dtb0$live05==0),sum(dtb0$live05==1 & dtb0$live13==0),sum(dtb0$live13==1 & dtb0$live18==0))
deaddt<-data.frame(deadyear,deadnum)
deaddt
deadplot<-ggplot(deaddt,aes(x=deadyear,y=deadnum)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x="年份",y="死亡植株數量(株)") + 
  theme(axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8))
#ggsave(filename = "deadstemnum.png",width = 10,height = 10,units = "cm")
deadplot


# 存活植株數量變化圖(上次活著，這次也活著的) ---------------------------------------------------------------------
liveyear<-c("2005","2013","2019")
livenum<-c(sum(dtb0$live97==1 & dtb0$live05==1),sum(dtb0$live05==1 & dtb0$live13==1),sum(dtb0$live13==1 & dtb0$live18==1))
livedt<-data.frame(liveyear,livenum)
liveplot<-ggplot(livedt,aes(x=liveyear,y=livenum)) +
  geom_bar(stat = "identity") + 
  theme_classic() +
  labs(x="年份",y="存活植株數量(株)") + 
  theme(axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8))
#ggsave(filename = "livestemnum.png",width = 10,height = 10,units = "cm")
liveplot


# 匯入環境資料 ---------------------------------------------------------------------
env<-read.csv("./欖仁溪環境資料.csv")


# 各樣方植株數量 ---------------------------------------------------------------------
dtb0$x1<-as.numeric(dtb0$x1)
dtb0$y1<-as.numeric(dtb0$y1)
livegradient<-dtb0 %>%
  group_by(.,x1,y1) %>%
  summarise(.,livenum97=sum(live97),livenum05=sum(live05),livenum13=sum(live13),livenum18=sum(live18)) %>%
  arrange(.,y1,x1)

pstem<-ggplot(data = livegradient,aes(x=x1,y=y1,fill=livegradient$livenum18))+
  geom_raster(hjust = 1,vjust = 1)+
  scale_fill_gradient(low = "blue4",high = "yellow",name="植株數量(株)")+
  labs(x="Quadrat",y="Quadrat")+
  theme_linedraw()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = -1,size = 10))+
  # geom_contour(data = envf,aes(x=X,y=Y,z=envf$Eleva),color="black")+
  # geom_text_contour(data = envf,aes(x=X,y=Y,z = envf$Eleva),inherit.aes = FALSE,stroke = 0.1,size=3)+
  scale_y_continuous(breaks = seq(17,39,1))+
  scale_x_continuous(breaks = seq(26,56,1))+
  coord_fixed()
#ggsave(filename = "livestemgradient2018.png",width = 20,height = 20,units = "cm")
pstem
#dev.off()


# 各樣方胸高斷面積 ---------------------------------------------------------------------
dtb0$x1<-as.numeric(dtb0$x1)
dtb0$y1<-as.numeric(dtb0$y1)
bagradient<-dtb0 %>%
  group_by(.,x1,y1) %>%
  summarise(.,sumba1997=sum(sumba97),sumba2005=sum(sumba05),sumba2013=sum(sumba13),sumba2018=sum(sumba18)) %>%
  arrange(.,y1,x1)

pba<-ggplot(data = bagradient,aes(x=x1,y=y1,fill=bagradient$sumba2018))+
  geom_raster(hjust = 1,vjust = 1)+
  scale_fill_gradient(low = "white",high = "blue",name=expression("BA(m"^"2"*")"))+
  labs(x="Quadrat",y="Quadrat")+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "red"),
        axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0,size = 8))+
  # geom_contour(data = envf,aes(x=X,y=Y,z=envf$Eleva),color="black")+
  # geom_text_contour(data = envf,aes(x=X,y=Y,z = envf$Eleva),inherit.aes = FALSE,stroke = 0.1,size=3)+
  scale_y_continuous(breaks = seq(17,39,1))+
  scale_x_continuous(breaks = seq(26,56,1))+
  coord_fixed()
#ggsave(filename = "bagradient2018.png",width = 20,height = 20,units = "cm")
pba
#dev.off()


# 地形圖 ---------------------------------------------------------------------
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


# 名錄 ---------------------------------------------------------------------
kdt<-read.csv("./All300.csv",stringsAsFactors = F)
redlist<-read.csv("./台灣植物紅皮書.csv",stringsAsFactors = F)
clist<-kdt %>% distinct(.,sp) %>% arrange(.,sp) %>% filter(.,sp!="un" & sp!="UNKNOWN")
dt<-filter(clist,sp!="un" & sp!="" & sp!="UNKNOWN" & sp!="野牡丹藤") %>%
  mutate(.,sp=ifelse(sp=="台灣蕘花","臺灣蕘花",sp),
         sp=ifelse(sp=="台灣楊桐","臺灣楊桐",sp),
         sp=ifelse(sp=="杏葉石礫","杏葉石櫟",sp),
         sp=ifelse(sp=="南仁灰木","希蘭灰木",sp),
         sp=ifelse(sp=="白匏仔","白匏子",sp),
         sp=ifelse(sp=="紅花八角","臺灣八角",sp),
         sp=ifelse(sp=="長尾栲","長尾尖葉櫧",sp),
         sp=ifelse(sp=="廣東瓊楠","華河瓊楠",sp),
         sp=ifelse(sp=="松田冬青","松田氏冬青",sp),
         sp=ifelse(sp=="嶺南椆","嶺南青剛櫟",sp),
         sp=ifelse(sp=="江某","鵝掌柴",sp),
         sp=ifelse(sp=="鐵雨傘","雨傘仔",sp),
         sp=ifelse(sp=="倒卵葉楠","恆春楨楠",sp),
         sp=ifelse(sp=="紅楠","豬腳楠",sp),
         sp=ifelse(sp=="黑星櫻","墨點櫻桃",sp),
         sp=ifelse(sp=="金平冬青","倒卵葉冬青",sp),
         sp=ifelse(sp=="臺灣柯","臺灣石櫟",sp),
         sp=ifelse(sp=="銹葉野牡丹","大野牡丹",sp),
         sp=ifelse(sp=="水同木","豬母乳",sp),
         sp=ifelse(sp=="短尾柯","短尾葉石櫟",sp),
         sp=ifelse(sp=="南仁鐵色","交力坪鐵色",sp),
         sp=ifelse(sp=="長葉厚殼樹","長花厚殼樹",sp),
         sp=ifelse(sp=="臺灣梭羅木","臺灣梭羅樹",sp),
         sp=ifelse(sp=="印度栲","印度苦櫧",sp),
         sp=ifelse(sp=="山漆","木蠟樹",sp),
         sp=ifelse(sp=="海南厚殼桂","土楠",sp),
         sp=ifelse(sp=="茄苳","茄冬",sp),
         sp=ifelse(sp=="恆春紫珠","疏齒紫珠",sp),
         sp=ifelse(sp=="島榕","白肉榕",sp),
         sp=ifelse(sp=="臺灣栲","臺灣苦櫧",sp),
         sp=ifelse(sp=="金斗椆","捲斗櫟",sp),
         sp=ifelse(sp=="小梗木薑子","黃肉樹",sp),
         sp=ifelse(sp=="三叉虎","三腳虌",sp)) %>%
  distinct(.,sp,.keep_all = T)
dtname<-left_join(dt,redlist,by=c("sp"="zh_name"))
genus<-str_split_fixed(dtname$fullname," ",2)
colnames(genus)[1:2]<-c("Genus","other")
genus<-as.data.frame(genus,stringsAsFactors = F)
spname<-bind_cols(dtname,genus) %>%
  select(.,sp,family_zh,family,Genus,fullname)
colnames(spname)<-c("中名","科名","Family","Genus","Species")
write.csv(spname,file = "欖仁溪名錄.csv",row.names=F)

