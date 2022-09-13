## HEATMAP

library(DBI)
library(tidyverse)
library(lubridate)
library(magrittr)
library(reshape2)
library(zoo)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


result_map <- dbGetQuery(con2,"

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),

       CLI AS (SELECT DISTINCT C.CLICODIGO,
                       CLINOMEFANT,
                        IIF(C.GCLCODIGO IS NULL,C.CLICODIGO || ' ' || CLINOMEFANT,'G' || C.GCLCODIGO || ' ' || GCLNOME) CLIENTE,
                         ENDCODIGO,
                          SETOR
                           FROM CLIEN C
                            LEFT JOIN GRUPOCLI GR ON C.GCLCODIGO=GR.GCLCODIGO
                             LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                              LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                               WHERE CLICLIENTE='S'),

         PED AS (SELECT ID_PEDIDO,
                         P.CLICODIGO,
                          CLINOMEFANT,
                           CLIENTE,
                            SETOR,
                             PEDDTEMIS 
                              FROM PEDID P
                               INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
                                INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                                 WHERE PEDDTEMIS BETWEEN DATEADD(-15 DAY TO CURRENT_DATE) AND 'TODAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))

        SELECT PEDDTEMIS,
                CLICODIGO,
                 CLINOMEFANT,
                  CLIENTE,
                   SETOR,
                    SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                     FROM PDPRD PD
                      INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                       GROUP BY 1,2,3,4,5")

View(result_map)

## TABULAR DATA

heatmap <- result_map %>% mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>%  
   dcast(CLIENTE ~ PEDDTEMIS,value.var = "VRVENDA",fun.aggregate=sum) %>% 
    rowwise() %>%  
     mutate(TOTALS= rowSums(across(where(is.numeric)),na.rm = TRUE))  %>% 
      mutate(across(where(is.numeric), na_if, 0)) %>%
       rowwise() %>%  
        mutate(TOTALM= rowMeans(across(2:(ncol(.)-1)),na.rm = TRUE)) %>% 
         arrange(desc(TOTALS))



View(heatmap)


range_write("1PEzq5MAD24WAINYDR0TeO9VvBI3XQwSaSKhRUlqBzpc",data=heatmap ,
              sheet = "15 DIAS",
               range = "A1",reformat = FALSE) 

## CREATE MAP

result_map %>% mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>%  ggplot(.,aes(x=PEDDTEMIS,y=CLIENTE,fill=VRVENDA)) + geom_tile(color="#ffffff") + 
        geom_text(aes(label=format(round(VRVENDA,0),big.mark = ","))) + 
         theme(legend.position = "none")  + 
          scale_x_discrete(expand = c(0,0)) + 
           scale_y_discrete(expand = c(0,0)) + 
             theme(axis.title = element_blank(),axis.text.x = element_text(size = 12,face = "bold"),axis.text.y = element_text(size = 12,face = "bold")) +
  scale_fill_gradientn(colors = c("#edfaf5","#dcedc1","#ffd3b6","#ffaaa5","#ff8b94"))





  




