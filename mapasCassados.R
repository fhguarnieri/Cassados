#
# Faz mapa do QL dos cassados (onde tiveram mais votos)
#

# Arquivos z e cidades podem ser carregados aqui:

load(mapas.RData)

# Base com código TSE e IBGE dos candidatos
#z <- read.csv("/home/iesp2/Documentos/limongi/referendo2005/zucco2.csv")
#z <- z[,c(2,3,133,134)]
#z <- z[z$state == "RS",]

# Faz o merge com os dados dos candidatos e deixa só os cassados

mapDesp <- left_join(D[[1]],z,by = join_by(Mun == mun))
mapDesp <- mapDesp[,c("IBGEcod","Mun","Candidato","Cassado","QL")]
mapDesp <- mapDesp[mapDesp$Cassado == 1,]

# Coloca no formato wide
#library(reshape2)
mapDesp <- dcast(mapDesp, IBGEcod + Mun ~ Candidato, value.var = "QL", fun.aggregate = sum)
mapDesp$IBGEcod <- as.character(mapDesp$IBGEcod)

# Cria os mapas usando os pacotes sf e ggplot2

library(sf)

#cidades <- read_sf("RS_1966_1982", "RS_1966_1982")
#cidades <- left_join(cidades, mapDesp, by = join_by(COD_IBGE == IBGEcod))
sf_use_s2(FALSE)
cidades_points <- st_centroid(cidades)
cidades_points <- cbind(cidades, st_coordinates(st_centroid(cidades$geometry)))


library(ggplot2)

ggplot(data = cidades) +
  geom_sf(aes(fill = `ARMANDO TEMPERANI PEREIRA`)) +
  geom_text(data= cidades_points[cidades_points$ARMANDO.TEMPERANI.PEREIRA > 6,],
            aes(x=X, y=Y, label=MUNICIPIO),
            color = "orange", size = 2) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 5),
        legend.key.size = unit(0.5,"line"))



