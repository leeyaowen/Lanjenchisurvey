library(benthos)

plotspnum1991<-dtb0 %>%
  dcast(formula = sp~x1y1,value.var = "live91",fun.aggregate = sum)

plotspnum1997<-dtb0 %>%
  dcast(formula = sp~x1y1,value.var = "live97",fun.aggregate = sum)

plotspnum2005<-dtb0 %>%
  dcast(formula = sp~x1y1,value.var = "live05",fun.aggregate = sum)

plotspnum2013<-dtb0 %>%
  dcast(formula = sp~x1y1,value.var = "live13",fun.aggregate = sum)

plotspnum2019<-dtb0 %>%
  dcast(formula = sp~x1y1,value.var = "live18",fun.aggregate = sum)

rqdf1991<-data.frame(s=NA)
rqdf1997<-data.frame(s=NA)
rqdf2005<-data.frame(s=NA)
rqdf2013<-data.frame(s=NA)
rqdf2019<-data.frame(s=NA)

for (i in 1:300) {
  rqdf1991<-rbind(rqdf1991,hurlbert(plotspnum1991,taxon = sp,count = plotspnum1991[,i+1],n = 10))
}
for (i in 1:300) {
  rqdf1997<-rbind(rqdf1997,hurlbert(plotspnum1997,taxon = sp,count = plotspnum1997[,i+1],n = 10))
}
for (i in 1:300) {
  rqdf2005<-rbind(rqdf2005,hurlbert(plotspnum2005,taxon = sp,count = plotspnum2005[,i+1],n = 10))
}
for (i in 1:300) {
  rqdf2013<-rbind(rqdf2013,hurlbert(plotspnum2013,taxon = sp,count = plotspnum2013[,i+1],n = 10))
}
for (i in 1:300) {
  rqdf2019<-rbind(rqdf2019,hurlbert(plotspnum2019,taxon = sp,count = plotspnum2019[,i+1],n = 10))
}

rqdf1991 %<>% filter(.,!is.na(s)) %>%
  bind_cols(.,data.frame(year=rep("1991",300),stringsAsFactors = F))
rqdf1997 %<>% filter(.,!is.na(s)) %>%
  bind_cols(.,data.frame(year=rep("1997",300),stringsAsFactors = F))
rqdf2005 %<>% filter(.,!is.na(s)) %>%
  bind_cols(.,data.frame(year=rep("2005",300),stringsAsFactors = F))
rqdf2013 %<>% filter(.,!is.na(s)) %>%
  bind_cols(.,data.frame(year=rep("2013",300),stringsAsFactors = F))
rqdf2019 %<>% filter(.,!is.na(s)) %>%
  bind_cols(.,data.frame(year=rep("2019",300),stringsAsFactors = F))

rqdfall<-bind_rows(rqdf1991,rqdf1997,rqdf2005,rqdf2013,rqdf2019) 

rqdfmean<-rqdfall %>% 
  group_by(.,year) %>%
  summarise(.,S=mean(s))

rqlm<-lm(s~year,rqdfall)
rqout<-summary(rqlm)
rqstd<-data.frame(std=rqout$coefficients[,2],year=c("1991","1997","2005","2013","2019"),stringsAsFactors = F)
rownames(rqstd)<-NULL
rqstd<-full_join(rqstd,rqdfmean,by="year")

ggplot(data = rqdfmean,aes(x=year,y=S,group=1))+
  geom_smooth(method = "lm",se=F,color="black")+
  geom_point()+
  geom_errorbar(data = rqstd,aes(x=year,ymin=S-std,ymax=S+std),inherit.aes = F,width=0.2)+
  theme_classic()