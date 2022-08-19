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
                               WHERE PEDDTEMIS >= DATEADD(-30 DAY TO CURRENt_DATE ) AND PEDDTEMIS<='YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))

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
  group_by(PEDDTEMIS) %>% summarize(V=sum(VRVENDA)) %>% as.data.frame() %>% mutate(AVG=rollmeanr(V,7,fill=NA)) 

View(result)


## method 2 function
moving_average <- function(x, n = 3) {    
  stats::filter(x, rep(1 / n, n), sides = 2)
}

 result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% summarize(V=sum(VRVENDA)) %>% as.data.frame() %>% mutate(AVG=moving_average(V)) 



## chart
result %>% melt(id.vars="PEDDTEMIS") %>% 
    ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(round(value,0),big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
   theme(panel.background = element_rect(fill = "#0c1839"),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_line(colour = "#15295f"),
         legend.position = "top")

##=========================================================================================

## moving average

## method 1 zoo
result_setores <- result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(SETOR,PEDDTEMIS) %>% summarize(V=sum(VRVENDA)) 

View(result_setores)

result_setores <- result_day %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(SETOR,PEDDTEMIS) %>% summarize(V=sum(VRVENDA)) 




##=========================================================================================
## CALCULATE MONTHLY AVERAGE



result_lens <- dbGetQuery(con2,"

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),

       CLI AS (SELECT DISTINCT C.CLICODIGO,
                       CLINOMEFANT,
                        ENDCODIGO,
                         SETOR
                          FROM CLIEN C
                           LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                            LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                             WHERE CLICLIENTE='S' ),

         PED AS (SELECT ID_PEDIDO,
                         TPCODIGO,
                          P.CLICODIGO,
                           SETOR,
                            PEDDTEMIS 
                             FROM PEDID P
                              INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
                               INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                                WHERE  
                                 PEDDTEMIS >= DATEADD(-30 DAY TO CURRENt_DATE ) AND 
                                  PEDDTEMIS<='YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')
                                   ),
            
                               
        PROD AS  (SELECT PROCODIGO FROM PRODU WHERE PROTIPO IN ('P','F','E')) ,
        
        VLX AS  (SELECT PROCODIGO FROM PRODU WHERE MARCODIGO=57),
        
        KDK AS  (SELECT PROCODIGO FROM PRODU WHERE MARCODIGO=24),
        
        LA AS  (SELECT PROCODIGO FROM PRODU WHERE GR1CODIGO=2),
        
        TRANS AS  (SELECT PROCODIGO FROM PRODU WHERE (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%'))  

        SELECT PEDDTEMIS,
                CLICODIGO,
                 PR.PROCODIGO,
                  PDPDESCRICAO,
                   SETOR,
                    CASE 
                     WHEN VX.PROCODIGO IS NOT NULL THEN 'VARILUX'
                      WHEN KD.PROCODIGO IS NOT NULL THEN 'KODAK' 
                       ELSE '' END MARCA,
                        IIF (T.PROCODIGO IS NOT NULL,'TRANSITIONS','') TRANSITIONS,
                         CASE 
                          WHEN L.PROCODIGO IS NOT NULL THEN 'LA'
                            ELSE '' END TIPO,
                             SUM(PDPQTDADE)QTD,
                              SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                               FROM PDPRD PD
                                INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                                 INNER JOIN PROD PR ON PD.PROCODIGO=PR.PROCODIGO
                                  LEFT JOIN VLX VX ON PD.PROCODIGO=VX.PROCODIGO
                                   LEFT JOIN KDK KD ON PD.PROCODIGO=KD.PROCODIGO
                                    LEFT JOIN TRANS T ON PD.PROCODIGO=T.PROCODIGO
                                     LEFT JOIN LA L ON PD.PROCODIGO=L.PROCODIGO
                                      GROUP BY 1,2,3,4,5,6,7,8")


View(result_lens)

## ====================================================================================================


## TOTAL LENS
lens <- result_lens %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% summarize(qtd=sum(QTD)) %>% as.data.frame() %>% mutate(AVG=rollmeanr(qtd,7,fill=NA)) 


View(lens)


lens %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top")



## ====================================================================================================

## TOTAL VARILUX
lens_vlx <- result_lens %>% filter(MARCA=='VARILUX') %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% summarize(qtd=sum(QTD)) %>% as.data.frame() %>% mutate(AVG=rollmeanr(qtd,5,fill=NA)) 


View(lens_vlx)


## TOTAL KODAK
lens_kdk <- result_lens %>% filter(str_detect(MARCA,"KODAK")) %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% summarize(qtd=sum(QTD)) %>% as.data.frame() %>% mutate(AVG=rollmeanr(qtd,5,fill=NA)) 


View(lens_kdk)


lens_kdk %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top")

## TOTAL LA

lens_la <- result_lens %>% filter(str_detect(TIPO,"LA")) %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% summarize(qtd=sum(QTD)) %>% as.data.frame() %>% mutate(AVG=rollmeanr(qtd,3,fill=NA)) 


View(lens_la)


lens_la %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top")


## PEDIDOS  =============================================================================================

result_pedidos <- dbGetQuery(con2,"

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
                               WHERE PEDDTEMIS BETWEEN '25.06.2022' AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))

        SELECT PEDDTEMIS,
                PD.ID_PEDIDO,
                 CLICODIGO,
                  SETOR,
                   SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                    FROM PDPRD PD
                     INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                      GROUP BY 1,2,3,4")

## method 1 zoo
result_pedidos2 <- result_pedidos %>% mutate(WKD=wday(PEDDTEMIS)) %>% filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% summarize(q=n_distinct(ID_PEDIDO)) %>% as.data.frame() %>% mutate(AVG=rollmeanr(q,5,fill=NA)) 

View(result_pedidos2)


## chart
result_pedidos2 %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top")
