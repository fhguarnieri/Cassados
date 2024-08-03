#
# Inferência ecológica usando as funções em VTR.R de Ionnis Andreadis
# disponível em (http://www.polres.gr/en/vtr). Ver também 
# Andreadis, I., & Chadjipadelis, T. (2009). A Method for the Estimation of 
# Voter Transition Rates. Journal of Elections, Public Opinion and Parties,
# 19(2), 203–218. https://doi.org/10.1080/17457280902799089
#


# Cria  tabelas para transição partido X municipio
# Os cassados formam um partido, pois estamos interessados para onde foram
# seus votos. Também incluí asbtenção subtraindo os votantes do eleitorado.


fs <- lapply(D, fazfs)

# faz as tabelas de transição conforme A&C.

myN <- D[[6]] %>% group_by(novMun) %>% summarise(N = sum(NumVotos))
myN <- as.matrix(myN$N)
myX <- as.data.frame(fs[[5]])
myT <- as.data.frame(fs[[6]])

z <- multirate(myN,myX,myT,0.01)
z$Bb
write.csv2(z$Bb,file = "trans7882.csv")
write.csv2(z$bb[,"CAS",], file = "transCAS82.csv")

# Faz os diagramans de Sunkey. Para isso usei o script disponívvel
# em https://www.data-to-viz.com/graph/sankey.html

library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)

data <- as.data.frame(z$Bb)

# I need a long format

data_long <- data %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name",
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)


