
# Testing functions for refactored database.


require(bombLepiSurv)
require(sf)
require(ggplot2)
##
humlesommerfConnect()

## get_map
tt <- get_map()

p <- ggplot(tt) +
  geom_sf(aes(fill = fylke))
class(p)
#OK


## getAmountWeights
tt <- getAmountWeights()
class(tt)
all(dim(tt) == (c(1, 3)))
#OK

## getDiffWeights
tt <- getDiffWeights()
tt
all(dim(tt) == c(4, 3))

##getExpValues
tt <- getExpValues()

ncol(tt) = 2
nrow(tt) > 20

##getAllData
tt <- getAllData(type = "Bumblebees",
                 habitat = "Gressmark",
                 year = 2024)

all("data.frame" %in% class(tt))
all(dim(tt) > c(20, 9))

##getComplData
system.time({
tt <- getComplData(type = "Bumblebees",
                   year = "2024",
                   habitat = "Gressmark")

})
#user  system elapsed
#0.021   0.001   0.044

tt

##collectIndData
data("hInd2010OstGress")
data("hInd2011OstGress")

tt <- collectIndData(type = "Bumblebees",
               habitat = "Gressmark",
               years = 2010:2011)

all("data.frame" %in% class(tt))
all(dim(tt) > c(1, 10))

## indPlot
tt <- collectIndData(type = "Bumblebees",
                     habitat = "Gressmark",
                     years = 2010:2021)

p <- indPlot(tt)
all("ggplot" %in% class(p))


##plotCommIndex
p <- plot(hInd2020OstGress)
all("ggplot" %in% class(p))


##surveBarPlot
allBombusGressmark2023 <- getAllData(type = "Bumblebees",
                                     habitat = "Gressmark",
                                     year = 2023,
                                     language = "Norsk")


p <-  surveyBarplot(allBombusGressmark2023)
all("ggplot" %in% class(p))


##plotArt
humlerSrc <- tbl(con, Id(schema = "views", table = "all_bombus"))
expect <- getExpValues("Bumblebees", region_short = "Trond", habitat = "Gressmark")
colToGet = "species_latin"

toPlot <- getAllData(type = "Bumblebees",
                     habitat = "Gressmark",
                     year = 2024,
                     language = "Latin")


p <- plotArt(toPlot, expect, main = "Gressmark")
all("ggplot" %in% class(p))


##print.comm_index
all(length(print(hInd2020TrondSkog))==1)

##calcInd
hInd2024TrondGress <- calcInd(type = "Bumblebees",
                              region_short = "Trond",
                              habitat = "Gressmark",
                              year = 2024,
                              nIter = 99,
                              save.draws = T)

hInd2024TrondSkog <- calcInd(type = "Bumblebees",
                              region_short = "Trond",
                              habitat = "Skogsmark",
                              year = 2024,
                              nIter = 99,
                              save.draws = T)

all("comm_index" %in% class(hInd2024TrondSkog))
#Not necessarily untrue
all(hInd2024TrondSkog[[1]] != hInd2024TrondGress[[1]])


"sInd2023TrondGress"
"sInd2023TrondSkog"
"sInd2023OstGress"
"sInd2023OstSkog"
"sInd2023SorGress"
"sInd2023SorSkog"
"sInd2023VestGress"
"sInd2023VestSkog"
"hInd2023TrondGress"
"hInd2023TrondSkog"
"hInd2023OstGress"
"hInd2023OstSkog"
"hInd2023SorGress"
"hInd2023SorSkog"
"hInd2023VestGress"
"hInd2023VestSkog"






