  library(DBI)
  library(magrittr)
  
  
  con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)
  
  
  #CURRENT MONTH
  
  dbGetQuery(con2,"
  
  WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
  
  SETOR AS (SELECT ZOCODIGO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28)),
  
  ENDE AS (SELECT ENDCODIGO,CLICODIGO FROM ENDCLI INNER JOIN SETOR ON ENDCLI.ZOCODIGO=SETOR.ZOCODIGO WHERE ENDFAT='S'),
  
  PEDEMIS AS (SELECT ID_PEDIDO,PEDID.CLICODIGO FROM PEDID
  
   INNER JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
    WHERE PEDDTEMIS BETWEEN (CURRENT_DATE-1) - EXTRACT(DAY FROM (CURRENT_DATE-1)) + 1 AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),
  
  PEDBAIXA AS (SELECT ID_PEDIDO,PEDID.CLICODIGO FROM PEDID INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
    INNER JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
     WHERE PEDDTBAIXA BETWEEN (CURRENT_DATE-1) - EXTRACT(DAY FROM (CURRENT_DATE-1)) + 1 AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),
  
  PDEMIS AS (SELECT SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
   FROM PDPRD PD
   INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
    INNER JOIN PEDEMIS ON PD.ID_PEDIDO=PEDEMIS.ID_PEDIDO),
  
  PDBAIXA AS (SELECT SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
   FROM PDPRD PD
   INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
    INNER JOIN PEDBAIXA ON PD.ID_PEDIDO=PEDBAIXA.ID_PEDIDO)
  
  SELECT VRVENDA,'EMISSAO' DATA
   FROM PDEMIS UNION
    SELECT VRVENDA,'BAIXA' DATA
     FROM PDBAIXA
  
  ") %>% format(.,big.mark=",")  %>% View()
  
  
  library(DBI)
  library(magrittr)
  
  
  con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)
  
  
  #CURRENT MONTH SETORES
  
  dbGetQuery(con2,"
  
  WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
  
  SETOR AS (SELECT ZOCODIGO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28)),
  
  ENDE AS (SELECT ENDCODIGO,CLICODIGO FROM ENDCLI INNER JOIN SETOR ON ENDCLI.ZOCODIGO=SETOR.ZOCODIGO WHERE ENDFAT='S'),
  
  PEDEMIS AS (SELECT ID_PEDIDO,PEDID.CLICODIGO FROM PEDID INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
   INNER JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
    WHERE PEDDTEMIS BETWEEN (CURRENT_DATE-1) - EXTRACT(DAY FROM (CURRENT_DATE-1)) + 1 AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),
  
  PEDBAIXA AS (SELECT ID_PEDIDO,PEDID.CLICODIGO FROM PEDID INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
    INNER JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
     WHERE PEDDTBAIXA BETWEEN (CURRENT_DATE-1) - EXTRACT(DAY FROM (CURRENT_DATE-1)) + 1 AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),
  
  PDEMIS AS (SELECT SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
   FROM PDPRD 
    INNER JOIN PEDEMIS ON PDPRD.ID_PEDIDO=PEDEMIS.ID_PEDIDO),
  
  PDBAIXA AS (SELECT SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
   FROM PDPRD 
    INNER JOIN PEDBAIXA ON PDPRD.ID_PEDIDO=PEDBAIXA.ID_PEDIDO)
  
  SELECT VRVENDA,'EMISSAO' DATA
   FROM PDEMIS UNION
    SELECT VRVENDA,'BAIXA' DATA
     FROM PDBAIXA
  
  ") %>% format(.,big.mark=",")  %>% View()
  
  


## last month

dbGetQuery(con2,"

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),

SETOR AS (SELECT ZOCODIGO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28)),

ENDE AS (SELECT ENDCODIGO,CLICODIGO FROM ENDCLI INNER JOIN SETOR ON ENDCLI.ZOCODIGO=SETOR.ZOCODIGO WHERE ENDFAT='S'),

PEDEMIS AS (SELECT ID_PEDIDO,PEDID.CLICODIGO FROM PEDID 
 INNER JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
  WHERE PEDDTEMIS BETWEEN '01.08.2023' AND '31.08.2023' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),

PEDBAIXA AS (SELECT ID_PEDIDO,PEDID.CLICODIGO FROM PEDID INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
  INNER JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
   WHERE PEDDTBAIXA BETWEEN  '01.08.2023' AND '31.08.2023'  AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),

PDEMIS AS (SELECT SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
 FROM PDPRD PD
  INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
  INNER JOIN PEDEMIS ON PD.ID_PEDIDO=PEDEMIS.ID_PEDIDO),

PDBAIXA AS (SELECT SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
 FROM PDPRD PD
  INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
  INNER JOIN PEDBAIXA ON PD.ID_PEDIDO=PEDBAIXA.ID_PEDIDO)

SELECT VRVENDA,'EMISSAO' DATA
 FROM PDEMIS UNION
  SELECT VRVENDA,'BAIXA' DATA
   FROM PDBAIXA

") %>% format(.,big.mark=",")  %>% View()



#YTD

dbGetQuery(con2,"

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),

SETOR AS (SELECT ZOCODIGO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28)),

ENDE AS (SELECT ENDCODIGO,CLICODIGO FROM ENDCLI INNER JOIN SETOR ON ENDCLI.ZOCODIGO=SETOR.ZOCODIGO WHERE ENDFAT='S'),

PEDEMIS AS (SELECT ID_PEDIDO,PEDID.CLICODIGO FROM PEDID INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
 INNER JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
  WHERE PEDDTEMIS BETWEEN '01.01.2023' AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),

PEDBAIXA AS (SELECT ID_PEDIDO,PEDID.CLICODIGO FROM PEDID INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
  INNER JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
   WHERE PEDDTBAIXA BETWEEN  '01.01.2023' AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')),

PDEMIS AS (SELECT SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
 FROM PDPRD 
  INNER JOIN PEDEMIS ON PDPRD.ID_PEDIDO=PEDEMIS.ID_PEDIDO),

PDBAIXA AS (SELECT SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
 FROM PDPRD 
  INNER JOIN PEDBAIXA ON PDPRD.ID_PEDIDO=PEDBAIXA.ID_PEDIDO)

SELECT VRVENDA,'EMISSAO' DATA
 FROM PDEMIS UNION
  SELECT VRVENDA,'BAIXA' DATA
   FROM PDBAIXA

") %>% format(.,big.mark=",")  %>% View()


