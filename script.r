rm(list=ls())



library(dplyr)
library(tidyr)
library(ggplot2)
library(animation)


# connexion à la base de données

library(RODBC)

#you have to fill in user and pwd & find the connection between Oracle/SQL & R

Connex= odbcConnect(dsn=as.character(SGBD[1,1]),uid=as.character(user),pwd=as.character(pwd))

TTM=paste("select codestat, to_char(avg(valeur_n), '9999.9'),to_char (dateobse, 'MM'), to_char (dateobse, 'YYYY')  from observation where  
codestat = ", namestation,
"and codeelem in (Tmincode,Tmaxcode)",
"and extract (year from dateobse) between ", 1850, "and", 2021,
"group by   to_char (dateobse, 'MM'),to_char (dateobse, 'YYYY'),codestat, to_char (dateobse, 'YYYY')", sep = " ")
B <- sqlQuery(Connex,TTM)
colnames(B)=c("namestation","VALUE","month","year")

t1=B %>% filter(year>=1991 & year<=2020 & month==1)
moy1=round(mean(t1$VALUE),1)

t2=B %>% filter(year>=1991 & year<=2020 & month==2)
moy2=round(mean(t2$VALUE),1)

t3=B %>% filter(year>=1991 & year<=2020 & month==3)
moy3=round(mean(t3$VALUE),1)

t4=B %>% filter(year>=1991 & year<=2020 & month==4)
moy4=round(mean(t4$VALUE),1)

t5=B %>% filter(year>=1991 & year<=2020 & month==5)
moy5=round(mean(t5$VALUE),1)

t6=B %>% filter(year>=1991 & year<=2020 & month==6)
moy6=round(mean(t6$VALUE),1)

t7=B %>% filter(year>=1991 & year<=2020 & month==7)
moy7=round(mean(t7$VALUE),1)

t8=B %>% filter(year>=1991 & year<=2020 & month==8)
moy8=round(mean(t8$VALUE),1)

t9=B %>% filter(year>=1991 & year<=2020 & month==9)
moy9=round(mean(t9$VALUE),1)

t10=B %>% filter(year>=1991 & year<=2020 & month==10)
moy10=round(mean(t10$VALUE),1)

t11=B %>% filter(year>=1991 & year<=2020 & month==11)
moy11=round(mean(t11$VALUE),1)

t12=B %>% filter(year>=1991 & year<=2020 & month==12)
moy12=round(mean(t12$VALUE),1)

if (B[,3] == 1){
	a1=B %>% filter(month==1)
	a1$VALUE=a1$VALUE-moy1
} else if ( B[,3]==2){
	a2=B %>% filter(month==2)
	a2$VALUE=a2$VALUE-moy2
} else if ( B[,3] ==3){
	a3=B %>% filter(month==3)
	a3$VALUE=a3$VALUE-moy3
} else if ( B[,3] ==4){
	a4=B %>% filter(month==4)
	a4$VALUE=a4$VALUE-moy4
} else if ( B[,3] ==5){
	a5=B %>% filter(month==5)
	a5$VALUE=a5$VALUE-moy5
} else if ( B[,3] ==6){
	a6=B %>% filter(month==6)
	a6$VALUE=a6$VALUE-moy6
} else if ( B[,3] ==7){
	a7=B %>% filter(month==7)
	a7$VALUE=a7$VALUE-moy7
} else if (B[,3] ==8){
	a8=B %>% filter(month==8)
	a8$VALUE=a8$VALUE-moy8
} else if ( B[,3] ==9){
	a9=B %>% filter(month==9)
	a9$VALUE=a9$VALUE-moy9
} else if ( B[,3] ==10){
	a10=B %>% filter(month==10)
	a10$VALUE=a10$VALUE-moy10
} else if ( B[,3] ==11){
	a11=B %>% filter(month==11)
	a11$VALUE=a11$VALUE-moy11
} else if (B[,3] ==12){
	a12=B %>% filter(month==12)
	a12$VALUE=a12$VALUE-moy12
}

mat=rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
colnames(mat)=c("CODESTAT","anomaly","month","year")
saveGIF({
  for(i in 1951:2021){
print(ggplot(MM %>% filter(year <= i), 
        aes(x=month, y=anomaly, color=year, group=year)) +
        geom_line() +
        scale_color_gradient(low="blue", high="red", limits=c(1951, 2021), guide="none") +
        geom_hline(yintercept=1.5, color="black", lty=2) +
        geom_hline(yintercept=2, color="black", lty=2) +
        coord_polar() +
        annotate(x=1, y=-1.5, geom="text", label=i) +
        annotate(x=1, y=1.5, geom="label", label="1.5C", fill="white", label.size=0) +
        annotate(x=1, y=2, geom="label", label="2.0C", fill="white", label.size=0) +
        ggtitle(expression(atop("Global Temperature Change 1850-2021", atop(italic("Data Source"), "")))) +
        scale_x_continuous(labels=mo, breaks=1:13) +
        scale_y_continuous(labels=NULL, breaks=NULL) +
        ylab("") + xlab("")
       
  )}
}, interval=0.1)