library(gridExtra)
library(grid)
library(ReporteRs)
library(dplyr)
dt<-read.csv("./Lanjenchi5.88survey.csv")


#輸出資料表
outputdata<-function(x1,y1){
  
  #選擇樣方，重組資料
  q11<-filter(dt,X1==x1,Y1==y1,X2==1,Y2==1)
  q12<-filter(dt,X1==x1,Y1==y1,X2==1,Y2==2)
  q22<-filter(dt,X1==x1,Y1==y1,X2==2,Y2==2)
  q21<-filter(dt,X1==x1,Y1==y1,X2==2,Y2==1)
  qall<-bind_rows(q11,q12,q22,q21)
  dttable<-select(qall,-X1,-Y1)
  
  #建立docx
  doc <- docx(template = "./調查紀錄紙-範本.docx",empty_template = TRUE)
  mytable<-FlexTable(dttable,header.columns = FALSE,body.par.props = parProperties(text.align = "right"),body.text.props = textProperties(font.size = 10,font.family = "Calibri"),header.text.props = textProperties(font.size = 10,font.weight = "normal",font.family = "Calibri"))
  mytable[,1:2]=parProperties(text.align = "center")
  mytable[,13]=textProperties(font.size = 6)
  mytable[,14]=textProperties(font.size = 6)
  my_text = pot(paste("(",x1,",",y1,")",sep=""), textProperties(color='black', font.size = 20,font.family = "Calibri") )
  mytable<-addHeaderRow(mytable,value = c("","調查者：________________________  紀錄者：___________  調查日期：__________\n電子輸入1：__________(      /   /   )   電子輸入2：__________(      /   /   )"),colspan=c(3,11),cell.properties=cellProperties(border.width = 0),text.properties = textProperties(font.size = 12,font.family = "Calibri"))
  mytable<-addHeaderRow(mytable,value = names(dttable),cell.properties = cellProperties(background.color = "gray70"))
  mytable[ 1, 1, to = "header"] = my_text
  mytable = setColumnsColors(mytable,j=6:7,colors = "gray80")
  mytable<-setFlexTableWidths(mytable,widths = c(0.2,0.2,0.9,0.4,1.2,0.5,0.47,0.4,0.43,0.42,0.4,0.43,0.4,1.4))
  mytable<-setFlexTableBorders(mytable,outer.vertical = borderProperties(width = 3),outer.horizontal = borderProperties(width = 3))
  mytable[1,1:14, to = 'header', side = 'bottom']<-chprop(borderProperties(width = 3))
  mytable[1,1:14, to = 'header', side = 'top']<-chprop(borderProperties(width = 0))
  mytable[1,1:14, to = 'header', side = 'right']<-chprop(borderProperties(width = 0))
  mytable[1,1:14, to = 'header', side = 'left']<-chprop(borderProperties(width = 0))
  doc<-addFlexTable(doc,mytable)
  writeDoc(doc,file = paste("survey(",x1,",",y1,").docx",sep=""))
  print(paste("正在產生(",x1,",",y1,")",sep=""))
}


#北方
outputNX1<-matrix(c(26:55))
outputNY1<-matrix(c(29:38))
for(i in 1:length(outputNX1)){
  for(j in 1:length(outputNY1)){
    outputdata(outputNX1[i],outputNY1[j])
  }
}

#南方
outputSX1<-matrix(c(26:49))
outputSY1<-matrix(c(17:28))
for(i in 1:length(outputSX1)){
  for(j in 1:length(outputSY1)){
    outputdata(outputSX1[i],outputSY1[j])
  }
}