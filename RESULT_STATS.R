library(DBI)
library(tidyverse)
library(lubridate)
library(magrittr)
library(reshape2)
library(zoo)

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


result_day <- dbGetQuery(con2,"

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),

       CLI AS (SELECT DISTINCT C.CLICODIGO,
                       CLINOMEFANT,
                        ENDCODIGO,
                         SETOR
                          FROM CLIEN C
                           LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                            LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                             WHERE CLICLIENTE='S'),

         PED AS (SELECT ID_PEDIDO,
                         P.CLICODIGO,
                          SETOR,
                           PEDDTEMIS 
                            FROM PEDID P
                             INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
                              INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                               WHERE PEDDTEMIS BETWEEN '01.06.2022' AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))

        SELECT PEDDTEMIS,
                CLICODIGO,
                 SETOR,
                  SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                   FROM PDPRD PD
                    INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                     GROUP BY 1,2,3")

View(result_day)

result_day %>% summarize(v=sum(VRVENDA))

## filter weekends
result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7))%>% View()


result_day %>% group_by(PEDDTEMIS) %>% summarize(v=sum(VRVENDA)) %>% View()


result_day %>% group_by(PEDDTEMIS) %>% summarize(v=sum(VRVENDA)) %>% View()


## moving average

## method 1 zoo
result <- result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% summarize(V=sum(VRVENDA)) %>% as.data.frame() %>% mutate(AVG=rollmeanr(V,3,fill=NA)) 

View(result)


## method 2 function
moving_average <- function(x, n = 3) {    
  stats::filter(x, rep(1 / n, n), sides = 2)
}

 result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% summarize(V=sum(VRVENDA)) %>% as.data.frame() %>% mutate(AVG=moving_average(V)) 



## chart
result %>% melt(id.vars="PEDDTEMIS") %>% 
    ggplot(.,aes(x=PEDDTEMIS,y=AVG)) + geom_line() +
  geom_text(aes(label=format(AVG,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
   theme(panel.background = element_rect(fill = "#0c1839"),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_line(colour = "#15295f"))








