# Links para download
# geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_shp/rj/
# ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/

#-------------------------------------------------------------------------------
# Carregando os dados dos setores

# Mudando o diretório
setwd("C:/Users/dffil/OneDrive/Área de Trabalho/CursoR/Dados")

# Carregando o pacote readr (leitura de arquivo csv)
library("readr")

# Carregando os dados
RJ <- read_csv2('Basico_RJ.csv',locale=locale(encoding = "latin1"))
TR <- RJ[RJ$Nome_do_municipio=="TRÊS RIOS",]
TR$Cod_setor <- as.factor(TR$Cod_setor)

# Número de setores do arquivo de dados
length(levels(TR$Cod_setor)) # 120 de Três Rios e 9 de Bemposta
levels(TR$Cod_setor)

#-------------------------------------------------------------------------------
# Carregando o mapa (arquivo shape)

# Mudando  diretório
setwd("C:/Users/dffil/OneDrive/Área de Trabalho/CursoR/Mapas")

# Carregando o pacote sf (Trabalha com shape files)
library("sf") 

# Carregando o arquivo do mapa dos municípios do RJ
shape <- read_sf(dsn = ".", layer = "33SEE250GC_SIR",options = "ENCODING=Latin1")

# Carregando o pacote tidyverse (gráficos ggplot2 entre outros)
library("tidyverse")
library("ggspatial")

# Gerando o mapa dos municípios do RJ
Mapa_RJ <- ggplot(data=shape)+
  geom_sf(aes(fill=NM_MUNICIP),show.legend=FALSE)+
  xlab("Longitude") + ylab("Latitude")+
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

# Exibindo o mapa
x11()
Mapa_RJ

# Selecionando o município de Três Rios
tr_shape <- shape[shape$NM_MUNICIP=="TRÊS RIOS",]
length(levels(factor(tr_shape$CD_GEOCODI))) #122 de Três Rios e 9 de Bemposta
levels(factor(tr_shape$CD_GEOCODI))

#-------------------------------------------------------------------------------
# Gráficos dos setores (Zona rural e urbana)

# Informações dos limites dos polígonos
tr_shape$geometry

# Gerando o mapa de Três Rios
g0 <- ggplot(data=tr_shape)+
  geom_sf(aes(fill=TIPO),size=.15)+
  scale_fill_manual(values = c("#C6E5B1","#E1DDD0"),labels=c("Rural","Urbana"))+
  labs(fill="Zona")+xlab("Longitude") + ylab("Latitude") +
  geom_text(aes(x=-43.09687860830546,y=-22.14960849529603,label = "Bemposta"),size=3,color="black")+
  geom_text(aes(x=-43.20966887662217,y=-22.13925158846297,label = "Três Rios"),size=3,color="black")+
  ylim(-22.24,-22.03148)+
  annotation_scale(location = "br", width_hint = 0.4, pad_y = unit(0.15, "in")) + # Escala (em km)
  annotation_north_arrow(location = "br", which_north = "true",                   # Rosa dos ventos
                         pad_x = unit(0.1, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(1.4, "cm"), width = unit(1.4, "cm"))


# Exibindo o mapa
x11()
g0

# Mudando o diretório
setwd("C:/Users/dffil/OneDrive/Área de Trabalho/CursoR/Resultados")

# Salvando o mapa em pdf
pdf("Mapa_Tres_Rios.pdf",width = 8, height = 6)
g0
dev.off()

#-------------------------------------------------------------------------------
# Selecionando apenas a Zona urbana

TR_urbana <- TR[TR$Nome_do_distrito!="BEMPOSTA",]
tr_shape_urbana <- tr_shape[tr_shape$TIPO=="URBANO",]
tr_shape_urbana <- tr_shape_urbana[tr_shape_urbana$NM_DISTRIT!="BEMPOSTA",]

# Número de setores da zona urbana (arquivo shape)
length(levels(factor(tr_shape_urbana$CD_GEOCODI)))
levels(factor(tr_shape_urbana$CD_GEOCODI))

# Número de setores da zona urbana (arquivo de dados)
length(levels(factor(TR_urbana$Cod_setor)))
levels(factor(TR_urbana$Cod_setor))

# Verificando setores que não coincidem nos dois arquivos
setdiff(TR_urbana$Cod_setor,tr_shape_urbana$CD_GEOCODI)
setdiff(tr_shape_urbana$CD_GEOCODI,TR_urbana$Cod_setor)
set_nao_coinc <- setdiff(TR_urbana$Cod_setor,tr_shape_urbana$CD_GEOCODI)
set_nao_coinc

# Removendo setores não coincidentes do arquivo de dados
TR_urbana <- TR_urbana %>%
  filter(!Cod_setor %in% set_nao_coinc)

colnames(tr_shape_urbana)[2] <- "Cod_setor"

# Verificando setores que não coincidem nos dois arquivos
setdiff(TR_urbana$Cod_setor,tr_shape_urbana$Cod_setor)

# Verificando a ordem dos setores nos dois arquivos
cbind((tr_shape_urbana$Cod_setor),(as.character(TR_urbana$Cod_setor)))
cbind(as.numeric(tr_shape_urbana$Cod_setor),as.numeric(as.character(TR_urbana$Cod_setor)),as.numeric(tr_shape_urbana$Cod_setor)-as.numeric(as.character(TR_urbana$Cod_setor)))

# Ordenando os setores no arquivo shape
tr_shape_urbana <- tr_shape_urbana[order(tr_shape_urbana$Cod_setor),]

# Verificando a ordem dos setores nos dois arquivos
cbind((tr_shape_urbana$Cod_setor),(as.character(TR_urbana$Cod_setor)))
cbind(as.numeric(tr_shape_urbana$Cod_setor),as.numeric(as.character(TR_urbana$Cod_setor)),as.numeric(tr_shape_urbana$Cod_setor)-as.numeric(as.character(TR_urbana$Cod_setor)))

#-------------------------------------------------------------------------------
# Análise descritiva das variáveis V001, V002, V003 e V009

TR_urbana2 <- TR_urbana[,1:32]

dados <- merge(tr_shape_urbana,TR_urbana2,by="Cod_setor")

# Análise exploratoria da variável V001 (número de domicílios)
summary(dados$V001)
ggplot(data=dados)+
  geom_boxplot(aes(y=V001))

# Removendo as informações dos setores censitários que contém apenas 1 domicílio
# e que não tem informações (NA) de algumas das variáveis
dados[dados$V001==1,]
dados[is.na(dados$V001)==TRUE,]
which(colnames(dados)=="V001"); which(colnames(dados)=="V012")
dados[dados$Cod_setor=="330600805000090",c(33:44)] <- NA
dados[dados$Cod_setor=="330600805000100",c(33:44)] <- NA
dados[dados$Cod_setor=="330600805000102",c(33:44)] <- NA

# Análise exploratoria da variável V001 (número de domicílios)
# após limpeza dos dados
summary(dados$V001)
ggplot(data=dados)+
  geom_boxplot(aes(y=V001))

# Análise exploratoria da variável V002 (população)
summary(dados$V002)
ggplot(data=dados)+
  geom_boxplot(aes(y=V002))

# Análise exploratoria da variável V003 (número médio de pessoas por domicílio)
summary(dados$V003)
ggplot(data=dados)+
  geom_boxplot(aes(y=V003))

# Análise exploratoria da variável V009 (renda média por setor censitário)
summary(dados$V009)
ggplot(data=dados)+
  geom_boxplot(aes(y=V009))

#-------------------------------------------------------------------------------
# Gráficos da zona urbana de Três Rios

# Carregando o pacote ClassInt (separa os dados em classes)
library("classInt")

# Agrupando o número de domicílios em classes (Percentis: 0-20%, 20-40%, 40-60%, 60-80%, 80-100%)
breaks_qt <- classIntervals(c(min(dados$V001,na.rm=TRUE) - .00001, dados$V001), n = 5, style = "quantile")
breaks_qt

# Criando um novo conjunto de dados contendo a variável V001_cat
# contendo a classe (categoria) a que cada dado de V001 pertence
dados2 <- mutate(dados, V001_cat = cut(V001, breaks_qt$brks)) 
dados2[is.na(dados2$V001_cat)==TRUE,]

# Mapa do número de domicílios por setor censitário
g1 <- ggplot(dados2) + 
  geom_sf(aes(fill=V001_cat),fill="white")+
  geom_sf(aes(fill=V001_cat))+
  scale_fill_brewer(palette = "OrRd",labels=c("(4, 145]","(145, 192]","(192, 223]","(223, 255]", "(255, 340]" ,"Sem informações"),na.value =  "white")+
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("(a) Número de domicílios por setor censitário") +
  labs(fill="Legenda")

# Visualizando o mapa
x11()
g1

# Agrupando a população em classes (Percentis: 0-20%, 20-40%, 40-60%, 60-80%, 80-100%)
breaks_qt2 <- classIntervals(c(min(dados$V002,na.rm=TRUE) - .00001, dados$V002), n = 5, style = "quantile")
breaks_qt2

# Criando um novo conjunto de dados contendo a variável V002_cat
# contendo a classe (categoria) a que cada dado de V002 pertence
dados3 <- mutate(dados2, V002_cat = cut(V002, breaks_qt2$brks)) 
dados3[is.na(dados3$V002_cat)==TRUE,]

# Mapa da população por setor censitário
g2 <- ggplot(dados3) + 
  geom_sf(aes(fill=V002_cat))+
  scale_fill_brewer(palette = "OrRd",labels=c("(13, 495]","(495, 627]","(627, 691]","(691 ,791]","(791, 1165]","Sem informações"),na.value =  "white")+
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("(b) População por setor censitário") +
  labs(fill="Legenda")

# Visualizando o mapa
x11()
g2

# Criando um novo conjunto de dados contendo a variável V003_cat
# contendo a classe (categoria) a que cada dado de V003 pertence
breaks_qt3 <- classIntervals(c(min(dados$V003,na.rm=TRUE) - .00001, dados$V003), n = 5, style = "quantile")
breaks_qt3

# Criando um novo conjunto de dados contendo a variável V003_cat
# contendo a classe (categoria) a que cada dado de V003 pertence
dados4 <- mutate(dados3, V003_cat = cut(V003, breaks_qt3$brks)) 
dados4[is.na(dados4$V003_cat)==TRUE,]

# Mapa do número de moradores por domicílio (por setor censitário)
g3 <- ggplot(dados4) + 
  geom_sf(aes(fill=V003_cat))+
  scale_fill_brewer(palette = "OrRd",labels=c("(2.11, 3.00]","(3.00, 3.16]","(3.16, 3.26]","(3.26, 3.47]","(3.47, 3.91]" ,"Sem informações"),na.value =  "white")+
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("(c) Número médio de moradores por domicílio") +
  labs(fill="Legenda")

# Visualizando o mapa
x11()
g3

# Criando um novo conjunto de dados contendo a variável V009_cat
# contendo a classe (categoria) a que cada dado de V009 pertence
breaks_qt4 <- classIntervals(c(min(dados$V009,na.rm=TRUE) - .00001, dados$V009), n = 5, style = "quantile")
breaks_qt4

# Criando um novo conjunto de dados contendo a variável V009_cat
# contendo a classe (categoria) a que cada dado de V009 pertence
dados5 <- mutate(dados4, V009_cat = cut(V009, breaks_qt4$brks)) 
dados5[is.na(dados5$V009_cat)==TRUE,]

# Mapa da renda per capita média por setor censitário
g4 <- ggplot(dados5) + 
  geom_sf(aes(fill=V009_cat))+
  scale_fill_brewer(palette = "OrRd",labels=c("(309, 430]","(430, 505]","(505, 613]","(613, 768]","(768, 3638]","Sem informações"),na.value =  "white")+
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("(d) Renda per capita média") +
  labs(fill="Legenda")

# Visualizando o mapa
x11()
g4

# Carregando o pacote patchwork (combinar gráficos ggplot)
library("patchwork")

# verificando o diretório 
getwd()

# Salvando os quatro mapas em um único pdf
pdf("Gráficos_juntos.pdf",width = 15,height = 12)
(g1+g2)/(g3+g4)
dev.off()


#-------------------------------------------------------------------------------
# I de Moran

# Carregando o pacote rgeoda (I de Moran)
library("rgeoda")

# Criando um novo conjunto de dados contendo a coluna id
dados_sf <- st_as_sf(dados4) %>%
  tibble::rownames_to_column("id")

# Removendo linhas com NA's na variável V003
to_clust <- dados_sf %>%
  filter(!is.na(V003))

# Verificando número de linhas removidas
dim(dados_sf);dim(to_clust)

# I de Moran
queen_wts <- queen_weights(to_clust)
moran <- local_moran(queen_wts, st_drop_geometry(to_clust["V003"]))
moran_lbls <- lisa_labels(moran)
moran_colors <- setNames(lisa_colors(moran), moran_lbls)

# Preparando o conjunto de dados para fazer o mapa
dados_clustered <- to_clust %>%
  st_drop_geometry() %>%
  mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
         cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
  right_join(dados_sf, by = "id") %>%
  st_as_sf()

# Mapa com o agrupamento espacial pelo I de Moran
g5 <- ggplot(dados_clustered, aes(fill = cluster)) +
  geom_sf(color = "black", size = 0) +
  scale_fill_manual(values = moran_colors, na.value = "white")+
  xlab("Longitude")+ylab("Latitude")+
  labs(fill="Clusters")+
  ggtitle("(a) Nº médio de moradores por domicílio")

# Visualizando o mapa com o agrupamento pelo I de Moran
x11()
g5

# Removendo linhas com NA's na variável V009
to_clust <- dados_sf %>%
  filter(!is.na(V009))

# Verificando número de linhas removidas
dim(dados_sf);dim(to_clust)

# I de Moran
queen_wts <- queen_weights(to_clust)
moran <- local_moran(queen_wts, st_drop_geometry(to_clust["V009"]))
moran_lbls <- lisa_labels(moran)
moran_colors <- setNames(lisa_colors(moran), moran_lbls)

# Preparando o conjunto de dados para fazer o mapa
dados_clustered <- to_clust %>%
  st_drop_geometry() %>%
  #select(id) %>%
  mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
         cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
  right_join(dados_sf, by = "id") %>%
  st_as_sf()

# Mapa com o agrupamento espacial pelo I de Moran
g6 <- ggplot(dados_clustered, aes(fill = cluster)) +
  geom_sf(color = "black", size = 0) +
  scale_fill_manual(values = moran_colors, na.value = "white")+
  xlab("Longitude")+ylab("Latitude")+
  labs(fill="Clusters")+
  ggtitle("(b) Renda per capita média")

# Visualizando o mapa com o agrupamento pelo I de Moran
x11()
g6

# Verificando o diretório
getwd()

# Salvando os mapas com os agrupamentos pelo I de Moran 
pdf("Moran.pdf",width = 12,height = 6)
g5+g6
dev.off()


