library(DBI)
library(tidyverse)
library(lubridate)
library(magrittr)
library(zoo)

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


result_day <- dbGetQuery(con2,"

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),

SETOR AS (SELECT ZOCODIGO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28)),

ENDE AS (SELECT ENDCODIGO,CLICODIGO FROM ENDCLI INNER JOIN SETOR ON ENDCLI.ZOCODIGO=SETOR.ZOCODIGO WHERE ENDFAT='S'),

PED AS (SELECT ID_PEDIDO,PEDID.CLICODIGO,PEDDTEMIS FROM PEDID INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
 LEFT JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
  WHERE PEDDTEMIS BETWEEN '01.05.2022' AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))

SELECT PEDDTEMIS,SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
 FROM PDPRD PD
  INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
   GROUP BY 1

") 
## filter weekends
result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7))%>% View()


## moving average
result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  mutate(AVG=rollmean(VRVENDA,3,fill=NA)) %>% View()


## chart
result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  mutate(AVG=round(rollmean(VRVENDA,5,fill=NA),0)) %>% 
    ggplot(.,aes(PEDDTEMIS,AVG)) + geom_line() + geom_text(aes(label=format(AVG,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
   theme(panel.background = )








