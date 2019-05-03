library(officer)
library(flextable)
library(magrittr)
library(dplyr)
dt<-read.csv("./Lanjenchi5.88survey.csv")


outputdata<-function(x1,y1){

  q11<-filter(dt,X1==x1,Y1==y1,X2==1,Y2==1)
  q12<-filter(dt,X1==x1,Y1==y1,X2==1,Y2==2)
  q22<-filter(dt,X1==x1,Y1==y1,X2==2,Y2==2)
  q21<-filter(dt,X1==x1,Y1==y1,X2==2,Y2==1)
  qall<-bind_rows(q11,q12,q22,q21)
  dttable<-select(qall,-X1,-Y1)

  ft<-flextable(dttable) %>%
    border_inner_h(border = fp_border()) %>%
    border_inner_v(border = fp_border()) %>%
    border_outer(border = fp_border(width = 1.5)) %>%
    align(align = "right",part = "body") %>%
    align(j=1:2,align = "center") %>%
    fontsize(size = 10,part = "all") %>%
    fontsize(j=13:14,size = 6) %>%
    add_header_row(values = c(paste0("(",x1,",",y1,")"),"調查者：________________________  紀錄者：___________  調查日期：__________\n電子輸入1：__________(      /   /   )   電子輸入2：__________(      /   /   )"), colwidths=c(3,11)) %>%
    fontsize(i=1,j=1,size = 20,part = "header") %>%
    fontsize(i=1,j=4,size = 12,part = "header") %>%
    align(i=2,align = "left",part = "header") %>%
    bg(j=6:7,bg="gray80",part = "body") %>%
    bg(i=2,bg="gray70",part = "header") %>%
    padding(padding.top = 2.8,padding.bottom = 2.8,part = "body") %>%
    padding(i=1,padding.left = 3.5,part = "header") %>%
    padding(i=2,padding.top = 0,padding.bottom = 0,part = "header") %>%
    font(fontname = "Calibri",part = "all") %>%
    width(j=1:14,width = c(0.2,0.2,0.8,0.4,1.2,0.5,0.47,0.4,0.43,0.52,0.4,0.45,0.4,1.4))

  doc<-read_docx(path = "./調查紀錄紙-範本.docx") %>%
    cursor_begin() %>%
    body_remove() %>%
    body_add_flextable(value = ft,pos = "on",align = "left") %>%
    print(paste0("./survey",y1,"(",x1,",",y1,").docx"))
}
