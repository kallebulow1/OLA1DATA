bolig <- read.csv("data/newhomes.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)


subset(bolig, V4 == "elmekrogen")

bolig[grep("elmekrogen", bolig$V4), ]

unique(bolig$V4)


bolig[grepl("elmekrogen", bolig$V4, ignore.case = TRUE), ]

plot(bolig$V9,bolig$V6)

# Fjern "kr." og mellemrum
bolig$V6 <- gsub(" kr\\.", "", bolig$V6)

# Fjern tusindtals-punktummer
bolig$V6 <- gsub("\\.", "", bolig$V6)

# Konverter til numerisk
bolig$V6 <- as.numeric(bolig$V6)

bolig$V6[1] <- "pris i dkk"

# Fjern " m²" fra teksten
bolig$V9 <- gsub(" m²", "", bolig$V9)

# Konverter til numerisk
bolig$V9 <- as.numeric(bolig$V9)

bolig$V9[1] <- "kvm"

# Fjern alt undtagen tal og punktum
bolig$V10 <- gsub("[^0-9\\.]", "", bolig$V10)

# Fjern tusindtals-punktummer
bolig$V10 <- gsub("\\.", "", bolig$V10)

# Konverter til numerisk
bolig$V10 <- as.numeric(bolig$V10)

bolig$V11 <- gsub("[^0-9\\.]","",bolig$V11)
bolig$V11[1] <- "grund m^2"
bolig$V11 <- as.numeric(bolig$V11)


colnames(bolig)[colnames(bolig) == "V6"] <- "Pris dkk"
colnames(bolig)[colnames(bolig) == "V7"] <- "energiklasse"
colnames(bolig)[colnames(bolig) == "V8"] <- "type"
colnames(bolig)[colnames(bolig) == "V9"] <- "kvm"
colnames(bolig)[colnames(bolig) == "V10"] <- "ejerudgift"
colnames(bolig)[colnames(bolig) == "V11"] <- "hej"
colnames(bolig)[colnames(bolig) == "V12"] <- "værelser"
colnames(bolig)[colnames(bolig) == "V13"] <- "liggetid"
colnames(bolig)[colnames(bolig) == "V14"] <- "alder"
colnames(bolig)[colnames(bolig) == "V2"] <- "bolig id"
colnames(bolig)[colnames(bolig) == "V3"] <- "adrese"
colnames(bolig)[colnames(bolig) == "V4"] <- "vej"
colnames(bolig)[colnames(bolig) == "V5"] <- "zip"

bolig <- bolig[-1, ]
bolig <- bolig[-1, ]

colnames(bolig)[colnames(bolig) == "hej"] <- "grund"

# Beregn pris pr. m2
bolig$pris_pr_m2 <- bolig$Pris / bolig$grund
bolig$NyKolonne <- NA

colnames(bolig)[colnames(bolig) == "kvm"] <- "kvm hus"
colnames(bolig)[colnames(bolig) == "grund"] <- "kvm grund"

bolig$NyKolonne <- bolig$Pris / bolig$kvm hus
colnames(bolig)[colnames(bolig) == "kvm hus"] <- "kvm_hus"
bolig$NyKolonne <- bolig$Pris / bolig$kvm_hus

bolig$NyKolonne[5] <- bolig$Pris[5] / bolig$kvm_hus[5]

bolig$Pris <- as.numeric(bolig$Pris)
bolig$kvm_hus <- as.numeric(bolig$kvm_hus)

bolig$NyKolonne[5] <- bolig$Pris[5] / bolig$kvm_hus[5]
bolig$pris_pr_m2 <- bolig$Pris / bolig$kvm_hus


colnames(bolig)[colnames(bolig) == "NyKolonne"] <- "pris_pr_m2"

model1 <- lm(pris_pr_m2 ~ ejerudgift, data = bolig)
summary(model1)

plot(bolig$ejerudgift, bolig$pris_pr_m2,
     main = "Pris pr. m² vs. Ejerudgift",
     xlab = "Ejerudgift (kr./md.)",
     ylab = "Pris pr. m² (kr.)",
     pch = 19, col = "blue")


vars <- bolig[, c("pris_pr_m2", "ejerudgift", "kvm grund", "kvm_hus", "værelser", "alder")]


vars <- bolig[, c("pris_pr_m2", "Ejerudgift", "Grund", "Kvm", "Vaerelser", "Byggeaar")]


colnames(bolig)[colnames(bolig) == "kvm grund"] <- "kvm_grund"
colnames(bolig)[colnames(bolig) == "værelser"] <- "rooms"

vars <- bolig[, c("pris_pr_m2", "ejerudgift", "kvm_grund", "kvm_hus", "rooms", "alder")]


str(bolig)

bolig$kvm hus <- as.numeric(bolig$kvm hus)

colnames(bolig)[colnames(bolig) == "kvm hus"] <- "kvm_hus"
bolig$kvm_hus <- as.numeric(bolig$kvm_hus)



cor_matrix <- cor(vars, use = "complete.obs")

str(bolig)
colnames(bolig)[colnames(bolig) == "Pris dkk"] <- "pris_dkk"
bolig$pris_dkk <- as.numeric(bolig$pris_dkk)

# Fjern alt undtagen tal
bolig$rooms <- gsub("[^0-9]", "", bolig$rooms)

# Konverter til numerisk
bolig$rooms <- as.numeric(bolig$rooms)
bolig$alder <- gsub("[^0-9]", "", bolig$alder)
bolig$alder <- as.numeric(bolig$alder)



lmtest1 <- lm(bolig[1:nrow(bolig),6])~(bolig[1:nrow(bolig),15])
summary(lmtest1)


# Rigtigt (giver en lm-model)
lmtest1 <- lm(pris_pr_m2 ~ pris_dkk, data = bolig)
summary(lmtest1)


lmtest2 <- lm(pris_pr_m2 ~ alder, data = bolig)
summary(lmtest2)


bolig$kvm_grund <- gsub(".000", "", bolig$kvm_grund)

lmtest3 <- lm(pris_pr_m2 ~ rooms, data = bolig)
summary(lmtest3)