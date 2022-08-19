library(DBI)
library(tidyverse)
library(lubridate)
library(magrittr)
library(reshape2)
library(zoo)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)





result_lens_setores <- dbGetQuery(con2,"

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


result_lens_geral <- result_lens_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(QTD=sum(QTD)) %>% 
  as.data.frame() %>% 
  mutate(MEDIAMOVEL=round(rollmeanr(QTD,7,fill=NA),0))  


## chart
result_lens_geral %>% melt(.,id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(round(value,0),big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff")) +
  labs(title = "GERAL") +
  xlab("EMISSÃO") + ylab("VENDAS")


result_lens_setor1 <- result_lens_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 1 - GRANDE FLORIANOPOLIS') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(QTD=sum(QTD)) %>% 
  as.data.frame() %>% 
  mutate(MEDIAMOVEL=round(rollmeanr(QTD,7,fill=NA),0))  


## chart
result_lens_setor1 %>% melt(.,id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(round(value,0),big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff")) +
  labs(title = "SETOR 1") +
  xlab("EMISSÃO") + ylab("VENDAS")




result_lens_setor2 <- result_lens_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(QTD=sum(QTD)) %>% 
  as.data.frame() %>% 
  mutate(MEDIAMOVEL=round(rollmeanr(QTD,7,fill=NA),0))  


## chart
result_lens_setor2 %>% melt(.,id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(round(value,0),big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff")) +
  labs(title = "SETOR 2") +
  xlab("EMISSÃO") + ylab("VENDAS")


result_lens_setor3 <- result_lens_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 3 - CHAPECO-PLANAL-OESTE') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(QTD=sum(QTD)) %>% 
  as.data.frame() %>% 
  mutate(MEDIAMOVEL=round(rollmeanr(QTD,7,fill=NA),0))  


## chart
result_lens_setor3 %>% melt(.,id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(round(value,0),big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff")) +
  labs(title = "SETOR 3") +
  xlab("EMISSÃO") + ylab("VENDAS")



result_lens_setor4 <- result_lens_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 4 - JOINVILLE - NORTESC') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(QTD=sum(QTD)) %>% 
  as.data.frame() %>% 
  mutate(MEDIAMOVEL=round(rollmeanr(QTD,7,fill=NA),0))  


## chart
result_lens_setor4 %>% melt(.,id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(round(value,0),big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff")) +
  labs(title = "SETOR 4") +
  xlab("EMISSÃO") + ylab("VENDAS")



result_lens_setor5 <- result_lens_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(QTD=sum(QTD)) %>% 
  as.data.frame() %>% 
  mutate(MEDIAMOVEL=round(rollmeanr(QTD,7,fill=NA),0))  


## chart
result_lens_setor5 %>% melt(.,id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(round(value,0),big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff")) +
  labs(title = "SETOR 5") +
  xlab("EMISSÃO") + ylab("VENDAS")


result_lens_setor6 <- result_lens_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 6 - BALNEARIO-LITORAL') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(QTD=sum(QTD)) %>% 
  as.data.frame() %>% 
  mutate(MEDIAMOVEL=round(rollmeanr(QTD,7,fill=NA),0))  


## chart
result_lens_setor6 %>% melt(.,id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(round(value,0),big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff")) +
  labs(title = "SETOR 6") +
  xlab("EMISSÃO") + ylab("VENDAS")



