## VOLUME DE PEDIDOS


library(DBI)
library(tidyverse)
library(lubridate)
library(magrittr)
library(reshape2)
library(zoo)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


vol_pedidos <- dbGetQuery(con2,"

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),

       CLI AS (SELECT DISTINCT C.CLICODIGO,
                       CLINOMEFANT,
                        ENDCODIGO,
                         SETOR
                          FROM CLIEN C
                           LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                            LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                             WHERE CLICLIENTE='S' )

      SELECT ID_PEDIDO,
                         TPCODIGO,
                           SETOR,
                            PEDDTEMIS,
                             IIF(FISCODIGO IS NOT NULL,1,0) VENDA
                             FROM PEDID P
                              LEFT JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
                               LEFT JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                                WHERE  
                                 PEDDTEMIS >= DATEADD(-120 DAY TO CURRENt_DATE ) AND 
                                  PEDDTEMIS<='YESTERDAY' AND PEDSITPED<>'C' 
                                  
")

View(vol_pedidos)


vol_pedidos %>%
        group_by(MES=floor_date(PEDDTEMIS,"month")) %>% 
            summarize(q=n_distinct(ID_PEDIDO)) %>% 
              View()


vol_pedidos %>%
  mutate(WKD=wday(PEDDTEMIS)) %>% 
   filter(!WKD %in% c(1,7)) %>% 
    filter(VENDA==1) %>% 
     group_by(PEDDTEMIS) %>%
      summarize(q=n_distinct(ID_PEDIDO)) %>% 
       as.data.frame() %>% 
        mutate(MEDIAMOVEL=round(rollmeanr(q,7,fill=NA),0)) %>% 
         View()


vol_pedidos %>%
  mutate(WKD=wday(PEDDTEMIS)) %>% 
   filter(!WKD %in% c(1,7)) %>% 
    filter(VENDA==1) %>% 
     group_by(SETOR,PEDDTEMIS) %>%
      summarize(q=n_distinct(ID_PEDIDO)) %>% 
       as.data.frame() %>% 
        mutate(MEDIAMOVEL=round(rollmeanr(q,7,fill=NA),0)) %>% 
         View()










