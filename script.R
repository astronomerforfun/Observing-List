library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(qdapTools)

my_table <- cbind(rownames(mtcars), mtcars)
colnames(my_table)[1] <- 'car'

#Messier data clean
MessierObjects <- read_excel("MessierObjects.xls")
colnames(MessierObjects) <- MessierObjects[2,]
MessierObjects <- MessierObjects[-c(1:2),c(1,2,3,4,10,11)]

MessierObjects$Magnitude <- as.numeric(MessierObjects$Magnitude)

#Herschel data clean
Herschel400 <- read_excel("Herschel400.xlsx")
unique(Herschel400$Constellation)
dput(unique(Herschel400$Constellation))
oldnames <- c("Cep", "Cas", "Cet", "And", "Scl", "Psc", "Tri", "Per", "Ari", 
              "Eri", "Cam", "Tau", "Aur", "Ori", "Lep", "Gem", "Mon", "CMa", 
              "Lyn", "Pup", "Hya", "Pyx", "UMa", "Cnc", "LMi", "Leo", "Sex", 
              "Dra", "Crt", "Crv", "Vir", "CVn", "Com", "Boo", "Lib", "Ser", 
              "Sco", "Oph", "Her", "UMi", "Sgr", "Sct", "Aql", "Vul", "Cyg", 
              "Del", "Aqr", "Lac", "Peg")

newnames <- c("Cephas", "Cassiopeia", "Cetus", "Andramada",
              "Sculptor", "Pisces", "Triangulum", "Perseus", "Aries",
              "Eridanus", "Camelopardalis", "Taurus", "Auriga", "Orion",
              "Lepus", "Gemini", "Monoceros", "Canis Major", "Lynx",
              "Puppis", "Hydra", "Pyxis", "Ursa Major", "Cancer", 
              "Leo Minor", "Leo", "Sextans", "Draco", "Crater", "Corvus", "Virgo",
              "Canes Venatici", "Como Berenices", "Bootes", "Libra", "Serpens",
              "Scorpius", "Ophiuchus", "Hercules","Ursa Minor", "Sagittarius",
              "Scotum", "Aquila", "Vulpecula", "Cygnus", "Deplhinus", "Aquarius", 
              "Lacerta", "Pegasus")
#create table to reference full names
table <- cbind(oldnames, newnames)

constellationslookup <- lookup(Herschel400$Constellation,table)
constellationslookup
#tie lookup to df
Herschel <- cbind(Herschel400, constellationslookup)

#rename columns to match Messier df and then combine

names(Herschel)[c(1,12)] <- c("ObjectNum", "Constellation")
names(Herschel)
Herschel <- Herschel[,c(1,2,3,10,11,12)]
Herschel$List <- "Herschel 400"
MessierObjects$List <- "Messier Objects"
newdf <- rbind(Herschel, MessierObjects)

#finaldf$Constellation %in% Winter

#Seasons
Allconst <- unique(dput(MessierObjects$Constellation))
Winter <- c("Cassiopeia", "Cephas", "Draco", "Ursa Major", "Ursa Minor", "Canis Major",
            "Cetas", "Eridanus", "Gemini", "Orion", "Perseus", "Taurus")

Spring <- c("Cassiopeia", "Cephas", "Draco", "Ursa Major", "Ursa Minor", "Bootes", "Cancer", "Crater",
            "Hydra", "Leo", "Virgo")

Summer <- c("Cassiopeia", "Cephas", "Draco", "Ursa Major", "Ursa Minor","Aquila", "Cygnus", "Hercules",
            "Lyra", "Ophiuchus", "Sagittarius", "Scorpius")

Fall <- c("Cassiopeia", "Cephas", "Draco", "Ursa Major", "Ursa Minor", "Andramada", "Aquarius", "Capricornus",
          "Pegasus", "Pisces")


##########Reduce diffuse Objects Mag by -1


diffuse_objects <- c("Diffuse Nebula", "Elliptical Galaxy", "Spiral Galaxy",      "Galaxy", "Planetary Nebula",
                     "Nebula", "Cluster Nebulosity")


#subset diffuse Ojbects
newdf2 <-filter(newdf,Type %in% diffuse_objects)

#Change from character to Numeric
newdf2$Magnitude <- as.numeric(newdf2$Magnitude)-1
test <- rbind(newdf, newdf2)
finaldf <- test%>%
  distinct(ObjectNum, .keep_all = TRUE)

finaldf$Magnitude <- as.numeric(finaldf$Magnitude)

#########Data Table for Magnification##########

Zone_Table <- read_excel("zonetable1.xlsx")
Zone_Table <- as.data.frame(Zone_Table)


objects <- MessierObjects[MessierObjects$Magnitude <= test$MaxMag,] 

############DOWNLOAD BUTTON######

csvDownloadUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  
  downloadButton(ns("download"), label)
}
#View(finaldf)

csvDownload <- function(input, output, session, data,
                        filename = paste0("Observing_List", Sys.Date(), ".csv")) {
  
  output$download <- downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
}



