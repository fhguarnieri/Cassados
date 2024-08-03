# Abre bases, agrega municípios e Calcula QL

# Carrega bases

v62 <- read.csv("voto_nominal_RS_62.csv", fileEncoding = "Latin3")
v66 <- read.csv("voto_nominal_RS_66.csv", fileEncoding = "Latin3")
v70 <- read.csv("voto_nominal_RS_70.csv", fileEncoding = "Latin3")
v74 <- read.csv("voto_nominal_RS_74.csv", fileEncoding = "Latin3")
v78 <- read.csv("voto_nominal_RS_78.csv", fileEncoding = "Latin3")
v82 <- read.csv("voto_nominal_RS_82.csv", fileEncoding = "Latin3")

# Entre 1962 e 1981 (73 entre 1962 e 1966) foram criados 74 novos municípios
# no RS. Como estamos interessados na transição de votos dos cassados eleitos
# em 1962 e em 1966 vou considerar a malha de municípios de 62, lançando os 
# municípios que surgiram e seus votos naqueles dos quais se originaram.

om <- read.csv("origMun.csv")

om$mun <- remove_acento(om$mun, Toupper = T)
om$novMun <- remove_acento(om$novMun, Toupper = T)
om$mun <- toupper(om$mun)
om$novMun <- toupper(om$novMun)

D <-  lapply(mget(ls(pattern = "^[a-zA-Z]\\d{2}$")), mudaMun)

D <- lapply(D, criaQL)





