library(DBI)
library(tidyverse)
library(lubridate)
library(magrittr)
library(reshape2)
library(zoo)

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


result_day_setores <- dbGetQuery(con2,"

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


result_setor1 <- result_day_setores %>% 
                    mutate(WKD=wday(PEDDTEMIS)) %>% 
                     filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
                      filter(SETOR=='SETOR 1 - GRANDE FLORIANOPOLIS') %>% 
                      group_by(PEDDTEMIS) %>% 
                       summarize(V=sum(VRVENDA)) %>% 
                        as.data.frame() %>% 
                         mutate(AVG=rollmeanr(V,7,fill=NA))  

View(result_setores)

## chart
result_setor1 %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff"))


result_setor2 <- result_day_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(V=sum(VRVENDA)) %>% 
  as.data.frame() %>% 
  mutate(AVG=rollmeanr(V,7,fill=NA))  


## chart
result_setor2 %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff"))


result_setor3 <- result_day_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 3 - CHAPECO-PLANAL-OESTE') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(V=sum(VRVENDA)) %>% 
  as.data.frame() %>% 
  mutate(AVG=rollmeanr(V,7,fill=NA))  


## chart
result_setor3 %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff"))


result_setor4 <- result_day_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 4 - JOINVILLE - NORTESC') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(V=sum(VRVENDA)) %>% 
  as.data.frame() %>% 
  mutate(AVG=rollmeanr(V,7,fill=NA))  


## chart
result_setor4 %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff"))


result_setor5 <- result_day_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(V=sum(VRVENDA)) %>% 
  as.data.frame() %>% 
  mutate(AVG=rollmeanr(V,7,fill=NA))  


## chart
result_setor5 %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff"))



result_setor6 <- result_day_setores %>% 
  mutate(WKD=wday(PEDDTEMIS)) %>% 
  filter(!WKD %in% c(1,7)) %>% as.data.frame() %>% 
  filter(SETOR=='SETOR 6 - BALNEARIO-LITORAL') %>% 
  group_by(PEDDTEMIS) %>% 
  summarize(V=sum(VRVENDA)) %>% 
  as.data.frame() %>% 
  mutate(AVG=rollmeanr(V,7,fill=NA))  


## chart
result_setor6 %>% melt(id.vars="PEDDTEMIS") %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=value,color=variable)) + geom_line() +
  geom_text(aes(label=format(value,big.mark=","))) +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top") + scale_colour_manual(values = c("#5783ad", "#00ffff"))
