options(repos='http://cran.rstudio.com/')
pkgs<-c("tidyverse", "reshape2","cluster","RColorBrewer","plotrix","data.table", "reshape2",
        "viridis", "MASS", "survival", "ggplot2", "ggpubr", "glmnet", "plyr", "dplyr", "sf", "concaveman", "deldir", "ggvoronoi", "spatstat", "readxl", "ggrepel")
lapply(pkgs, require, character.only=TRUE)
detach("package:dplyr", unload=TRUE)
library(dplyr)
library(tidyverse)

# load clinical files ----
# C:/Users/badhu/Documents/DOC
#filedir <- "C:/Users/badhu/Documents/DOC/"
#filedir <- "//crcdb14/imaging/DOC/202102_data/MOTIC/AnalysisOct2022/"

brdoc <- read_excel("DOC_analysis_20231201.xlsx", 1) %>%
#brdoc <- read_excel(paste(filedir, "DOC_analysis_20231124.xlsx", sep = ""), 1) %>%
  mutate(PtId = as.factor(PtId), Gender = as.factor(Gender), SmokingHx = as.factor(SmokingHx), SiteRisk = as.factor(SiteRisk)) %>% 
  mutate(DOB = as.Date(DOB), BrDate = as.Date(VisitDate),PathDate = as.Date(PathDate), outdate = as.Date(outdate))

docvs <- read_excel("DOC_analysis_20231201.xlsx", 2) %>% mutate(PtId = as.factor(PtId)) %>% mutate(VsDate = as.Date(VsDate))

mergid <- brdoc %>% filter(!is.na(mBarcode)) %>% pull(Barcode)
nomergid <- brdoc %>% filter(is.na(mBarcode)) %>% pull(Barcode)
mergept <- brdoc %>% filter(!is.na(mBarcode)) %>% select(PtId, BrushingId, Barcode, mBarcode)
slide1 <- brdoc %>% filter(!is.na(Barcode1)) %>% pull(Barcode)

doc1 <- brdoc %>% select(PtId, BrushingId, DOB, Gender, Smoking, SmokingHx, Barcode, mBarcode, newSet3, newSet4,
                         BrDate, outdate, out, outProg, outProg3, outProg5, 
                         BrushingSampleCode, PathDate, Dx, Dx2, Dx3, LesionCode, SiteRisk) %>%
  arrange(PtId, BrDate) %>% mutate(PtBrLs = paste(PtId, BrDate, LesionCode, sep = "_"))

doc2 <- docvs %>% select(PtId, VsDate, LesionCode, AnatomicDescription, AnatomicSiteName, LsWLGrp2, LesionFV, LesionLength, LesionWidth, LesionThickness) %>% 
  arrange(PtId, VsDate) %>% mutate(PtBrLs = paste(PtId, VsDate, LesionCode, sep = "_")) %>% select(-c(PtId, LesionCode))

docbrvs <- left_join(doc1, doc2) %>% select(-PtBrLs) %>%
  mutate(Pathgrp = as.factor(ifelse(is.na(Dx3), NA,
                                    ifelse(Dx3 %in% c("VC", "SCC"), "3", 
                                           ifelse(Dx3 %in% c("D3", "CIS"), "2", 
                                                  ifelse(Dx3 %in% c("LGL", "LD", "LD1", "D1", "D2", "VHYP"), "1", 
                                                         ifelse(Dx3 %in% c("Acanthosis", "CAN", "FEP", "Granuloma", "HYP(O)K", 
                                                                          "Inflammation", "LM", "LP", "PAP", "Scar", "trauma"), "0.5",
                                                                ifelse(Dx3 %in% c("normal"), "0", NA)))))))) %>%
  mutate(Pathgrp = factor(Pathgrp, levels = c("0", "0.5", "1", "2", "3"))) %>%
  mutate(Path = as.factor(ifelse(is.na(Pathgrp), NA, ifelse(Pathgrp %in% c("3", "2"), "1", "0")))) %>%
  mutate(Age = as.numeric(round((BrDate - DOB)/365.25, 2)))

# read morphology files ----
postDocCells <- readRDS("docells.rds")
# library(pbapply)
# 
# #celldir <- "C:/Users/badhu/Documents/DOC/"
# celldir <- "Z:/DOC/202102_data/MOTIC/Proj_DOC/"
# 
# postPAcellfiles <- list.files(paste(celldir, "PA_cells_normalized/", sep = ""), full.names=FALSE, pattern=".txt", recursive=TRUE)
# postPAcell_list <- pblapply(paste(celldir, "PA_cells_normalized/", postPAcellfiles, sep=""), function(x) {read.table(file=x, header=T, sep="")})
# names(postPAcell_list) <- postPAcellfiles
# postPAcells <-  dplyr::bind_rows(postPAcell_list, .id="FileName") %>%
#   mutate(Barcode = as.factor(substr(UnitId, 1, str_locate(UnitId, "_ScanDate")-1)),
#          GROUP = as.factor(GROUP)) %>% data.frame() %>% select(Barcode, everything()) %>% select(-c(FileName, diagnosis, prob, name_1, name_2, name_3, num, locks))
# 
# postRBcellfiles <- list.files(paste(celldir, "RB_cells_normalized/", sep = ""), full.names=FALSE, pattern=".txt", recursive=TRUE)
# postRBcell_list <- pblapply(paste(celldir, "RB_cells_normalized/", postRBcellfiles, sep=""), function(x) {read.table(file=x, header=T, sep="")})
# names(postRBcell_list) <- postRBcellfiles
# postRBcells <-  dplyr::bind_rows(postRBcell_list, .id="FileName") %>%
#   mutate(Barcode = as.factor(substr(UnitId, 1, str_locate(UnitId, "_ScanDate")-1)),
#          GROUP = as.factor(GROUP)) %>% data.frame() %>% select(Barcode, everything()) %>% select(-c(FileName, diagnosis, prob, name_1, name_2, name_3, num, locks))
# 
# postRAcellfiles <- list.files(paste(celldir, "RA_cells_normalized/", sep = ""), full.names=FALSE, pattern=".txt", recursive=TRUE)
# postRAcell_list <- pblapply(paste(celldir, "RA_cells_normalized/", postRAcellfiles, sep=""), function(x) {read.table(file=x, header=T, sep="")})
# names(postRAcell_list) <- postRAcellfiles
# postRAcells <-  dplyr::bind_rows(postRAcell_list, .id="FileName") %>%
#   mutate(Barcode = as.factor(substr(UnitId, 1, str_locate(UnitId, "_ScanDate")-1)),
#          GROUP = as.factor(GROUP)) %>% data.frame() %>% select(Barcode, everything()) %>% select(-c(FileName, diagnosis, prob, name_1, name_2, name_3, num, locks))
# 
# NUcellfiles <- list.files(paste(celldir, "NU_cells_normalized/", sep = ""), full.names=FALSE, pattern=".txt", recursive=TRUE)
# NUcell_list <- pblapply(paste(celldir, "NU_cells_normalized/", NUcellfiles, sep=""), function(x) {read.table(file=x, header=T, sep="")})
# names(NUcell_list) <- NUcellfiles
# NUcells <- dplyr::bind_rows(NUcell_list, .id="FileName") %>%
#   mutate(Barcode = as.factor(substr(UnitId, 1, str_locate(UnitId, "_ScanDate")-1)),
#          GROUP = as.factor(GROUP)) %>% data.frame() %>% select(Barcode, everything()) %>% 
#   select(-c(FileName, diagnosis, prob, name_1, name_2, name_3, num, locks)) %>% select(Barcode, everything())
# 
# postDocCells <- rbind(postRAcells, postRBcells, postPAcells, NUcells) %>% select(Barcode, everything())
# #postDocCells <- rbind(postRAcells, postRBcells, postPAcells) %>% select(Barcode, everything())

#saveRDS(postDocCells, file = "docells.rds")

# defining cutoffs and groups ----
docgrp <- postDocCells %>% select(Barcode, CellId, GROUP, area, DNA_Index) %>% filter(GROUP %in% c("3", "5", "6", "7")) %>% droplevels() %>%
  mutate(#CoA = as.factor(cut(DNA_Index, breaks = c(0.8, 1.2, 1.8, 2.2, Inf), include.lowest = TRUE, right = FALSE, dig.lab = 3, labels = c("3", "5", "6", "7"))),
         CoB = as.factor(cut(DNA_Index, breaks = c(0.85, 1.1, 1.7, 2.25, Inf), include.lowest = TRUE, right = FALSE, dig.lab = 3, labels = c("3", "5", "6", "7"))),
         #CoC = as.factor(cut(DNA_Index, breaks = c(0.9, 1.1, 1.85, 2.1, Inf), include.lowest = TRUE, right = FALSE, dig.lab = 3, labels = c("3", "5", "6", "7"))),
         CoD = as.factor(cut(DNA_Index, breaks = c(0.85, 1.15, 1.7, 2.3, Inf), include.lowest = TRUE, right = FALSE, dig.lab = 3, labels = c("3", "5", "6", "7"))),
         CoE = as.factor(cut(DNA_Index, breaks = c(0.85, 1.1, 1.7, 2.3, Inf), include.lowest = TRUE, right = FALSE, dig.lab = 3, labels = c("3", "5", "6", "7"))),)

# create bin df ----
binstart <- seq(0.80, 2.6, by = 0.05)
binend <- seq(0.85, 2.65, by = 0.05)
binname <- paste("bin", seq(80, 260, by = 5), sep="")

docbin <- docgrp %>% select(Barcode, DNA_Index) %>%
  mutate(bin = as.factor(cut(DNA_Index, breaks = c(binstart, Inf, by = 0.05), include.lowest = TRUE, right = FALSE, labels = c(binname, "bin999")))) %>% 
  arrange(Barcode, bin) %>% 
  group_by(Barcode, bin) %>% summarize(count = n()) %>% pivot_wider(names_from = bin, values_from = count, values_fill = 0) %>% 
  mutate(total = rowSums(across(bin80:bin999))) %>% select(Barcode, total, binname, bin999) #%>%
  #mutate(across(starts_with("bin"), ~./rowSums(across(bin80:bin999))*100, .names = 'pr{col}'))

# create sup df ----
supstart <- seq(1.15, 3.0, by = 0.05)
supname <- paste("sup", seq(115, 230, by = 5), sep="")

docsup <- docgrp %>% select(Barcode, DNA_Index) %>%
  mutate(sup = ifelse(DNA_Index > 3.00, "sup300",
                      ifelse(DNA_Index > 2.5, "sup250",
                             ifelse(DNA_Index > 2.4, "sup240",
                                    ifelse(DNA_Index > 2.3, "sup230",
                                           ifelse(DNA_Index > 2.25, "sup225",
                                                  ifelse(DNA_Index > 2.20, "sup220",
                                                         ifelse(DNA_Index > 2.15, "sup215",
                                                                ifelse(DNA_Index > 2.10, "sup210",
                                                                       ifelse(DNA_Index > 2.05, "sup205",
                                                                              ifelse(DNA_Index > 2.00, "sup200",
                                                                                     ifelse(DNA_Index > 1.95, "sup195",
                                                                                            ifelse(DNA_Index > 1.90, "sup190",
                                                                                                   ifelse(DNA_Index > 1.85, "sup185",
                                                                                                          ifelse(DNA_Index > 1.80, "sup180",
                                                                                                                 ifelse(DNA_Index > 1.75, "sup175",
                                                                                                                        ifelse(DNA_Index > 1.70, "sup170",
                                                                                                                               ifelse(DNA_Index > 1.65, "sup165",
                                                                                                                                      ifelse(DNA_Index > 1.60, "sup160",
                                                                                                                                             ifelse(DNA_Index > 1.55, "sup155",
                                                                                                                                                    ifelse(DNA_Index > 1.50, "sup150",
                                                                                                                                                           ifelse(DNA_Index > 1.45, "sup145",
                                                                                                                                                                  ifelse(DNA_Index > 1.40, "sup140",
                                                                                                                                                                         ifelse(DNA_Index > 1.35, "sup135",
                                                                                                                                                                                ifelse(DNA_Index > 1.30, "sup130",
                                                                                                                                                                                       ifelse(DNA_Index > 1.25, "sup125",
                                                                                                                                                                                              ifelse(DNA_Index > 1.20, "sup120",
                                                                                                                                                                                                     ifelse(DNA_Index > 1.15, "sup115", NA)))))))))))))))))))))))))))) %>%
  arrange(Barcode, sup) %>% group_by(Barcode, sup) %>% summarize(count = n()) %>% 
  pivot_wider(names_from = sup, values_from = count, values_fill = 0) %>% select(Barcode, supname, sup240, sup250, sup300)

#docbinsup <- left_join(docbin, docsup) %>% mutate(across(starts_with("sup"), ~./rowSums(across(bin80:bin999))*100, .names = 'pr{col}'))

# create prop df ----
docbinsup1 <- left_join(docbin, docsup) %>% filter(!(Barcode %in% mergid))

docbinsup2 <- left_join(docbin, docsup) %>% filter(Barcode %in% mergid)
mdocbinsup <- docbinsup2 %>% left_join(., docbrvs %>% select(PtId, Barcode, mBarcode)) %>% select(PtId, Barcode, mBarcode, everything()) %>% arrange(mBarcode) %>%
  group_by(PtId, mBarcode) %>% summarise_at(vars(total:sup300), sum, na.rm = TRUE) %>% rename(Barcode = "mBarcode")

docbinsup_12 <- rbind(docbinsup1, mdocbinsup)

docgrp_wide2 <- docgrp %>% filter(Barcode %in% mergid) %>% left_join(., docbrvs %>% select(PtId, Barcode, mBarcode)) %>% 
  select(-Barcode) %>% select(PtId, mBarcode, everything()) %>%
  pivot_longer(cols = c(CoB, CoD, CoE), names_to = "CoType", values_to = "CoGrp") %>%
  mutate(CoTypeGrp = as.factor(paste(CoType, CoGrp, sep = "_"))) %>% filter(!is.na(CoGrp)) %>% droplevels() %>%
  group_by(mBarcode, CoType) %>% mutate(CoTotal = n()) %>% ungroup() %>%
  select(-c(CellId, GROUP, area, DNA_Index, CoGrp)) %>% arrange(mBarcode, CoTypeGrp) %>%
  group_by(mBarcode, CoTypeGrp, CoType, CoTotal) %>% summarize(Cnt = n()) %>% ungroup() %>%
  pivot_wider(names_from = c(CoTypeGrp, CoType), names_glue = "{CoTypeGrp}{.value}", values_from = c(Cnt), values_fill = 0) %>%
  mutate(across(CoB_3Cnt:CoE_7Cnt, ~./CoTotal*100, .names = 'pr{col}')) %>% rename(Barcode = "mBarcode")
names(docgrp_wide2) <- gsub("Cnt", "", names(docgrp_wide2)) 

#left_join(mdocbinsup, docgrp_wide2) %>% rename(Barcode = "mBarcode") %>% select(PtId, Barcode, total, CoTotal, everything())

docgrp_wide1 <- docgrp %>% left_join(., docbrvs %>% select(PtId, Barcode, mBarcode)) %>% filter(is.na(mBarcode)) %>% select(Barcode, DNA_Index, CoB, CoD, CoE) %>%  
  pivot_longer(cols = c(CoB, CoD, CoE), names_to = "CoType", values_to = "CoGrp") %>% arrange(Barcode, CoType) %>%
  mutate(CoTypeGrp = as.factor(paste(CoType, CoGrp, sep = "_"))) %>% filter(!is.na(CoGrp)) %>% droplevels() %>%
  group_by(Barcode, CoType) %>% mutate(CoTotal = n()) %>% ungroup() %>% 
  select(-c(DNA_Index, CoGrp)) %>% arrange(Barcode, CoTypeGrp) %>%
  group_by(Barcode, CoTypeGrp, CoType, CoTotal) %>% summarize(Cnt = n()) %>% ungroup() %>%
  pivot_wider(names_from = c(CoTypeGrp, CoType), names_glue = "{CoTypeGrp}{.value}", values_from = c(Cnt), values_fill = 0) %>%
  select(Barcode, CoTotal, CoB_3Cnt:CoB_6Cnt, CoB_7Cnt, CoD_3Cnt:CoD_6Cnt, CoD_7Cnt, CoE_3Cnt:CoE_6Cnt, CoE_7Cnt) %>%
  mutate(across(CoB_3Cnt:CoE_7Cnt, ~./CoTotal*100, .names = 'pr{col}'))
names(docgrp_wide1) <- gsub("Cnt", "", names(docgrp_wide1))

names(docgrp_wide1)
names(docgrp_wide2)

docgrp_wide <- rbind(docgrp_wide1, docgrp_wide2) %>% left_join(., docbinsup_12) %>% 
  mutate(CoB_normal_p = prCoB_3, CoB_abnormal_p = rowSums(across(prCoB_5:prCoB_7)), CoB_67_p = rowSums(across(prCoB_6:prCoB_7)),
         CoD_normal_p = prCoD_3, CoD_abnormal_p = rowSums(across(prCoD_5:prCoD_7)), CoD_67_p = rowSums(across(prCoD_6:prCoD_7))) %>%
  select(-PtId)

# join docgrp_wide with clinical ----
#mdoc_morphvs <- docbrvs %>% select(-Barcode, BrushingId) %>% distinct(.keep_all = TRUE) %>% left_join(merged_wide, .) %>%
docbrvs1 <- docbrvs %>% filter(is.na(mBarcode)) %>% select(-mBarcode)
docbrvs2 <- docbrvs %>% filter(!is.na(mBarcode)) %>% select(-c(Barcode)) %>% distinct(.keep_all= TRUE) %>% rename(Barcode = mBarcode)

docbrvs12 <-rbind(docbrvs1, docbrvs2) %>% select(Barcode, everything())

doc_morphvs <- left_join(docgrp_wide, docbrvs12) %>%
  mutate(newSet3 = factor(newSet3, levels = c("train", "test", "valid")),
         newSetB = factor(newSet4, levels = c("train", "test", "valid")),
         #newSetD = factor(newSetD, levels = c("train", "test", "valid")),
         Pathgrp2 = ifelse(is.na(Pathgrp), "NA", 
                           ifelse(Pathgrp %in% c("2", "3"), "2", 
                                  ifelse(Pathgrp %in% c("0.5", "1") , "1", "0")))) %>%
  mutate(across(starts_with("bin"), ~./CoTotal*100, .names = 'pr{col}')) %>% 
  mutate(across(starts_with("sup"), ~./CoTotal*100, .names = 'pr{col}')) %>%
  select(Barcode:CoD_67_p, prbin80:prsup300, PtId:Pathgrp2)

  #filter(Barcode %in% slide1)
  #distinct(.keep_all = TRUE) %>% droplevels()

# tables of merged slides ----
# mdoc_morphvs %>% filter(CntCoD_7 >= 1) %>% group_by(Pathgrp2, outProg) %>% summarize(count = n())
# mdoc_morphvs %>% filter(CntCoD_7 < 1)
# mdoc_morphvs %>% filter(CntCoD_7 < 1, CoDTotal >= 400, CoD_abnormal_p >= 0.47) %>% group_by(Pathgrp2, outProg) %>% summarize(count = n())

# table of cases ----
names(doc_morphvs)
doc_morphvs %>%
  group_by(Pathgrp, newSetB) %>% summarize(count = n()) %>% pivot_wider(names_from = newSetB, values_from = count)

doc_morphvs %>% filter(Pathgrp %in% c("0", "2", "3"), CntCoB_7 < 1, CoBTotal >= 400)
doc_morphvs %>% filter(Pathgrp %in% c("0.5", "1"), CntCoB_7 < 1, CoBTotal < 400)

doc_morphvs %>% filter(Pathgrp %in% c("0", "2", "3"), CntCoB_7 < 1, CoBTotal >= 400) %>%
  group_by(Pathgrp, newSetB) %>% summarize(count = n()) %>% pivot_wider(names_from = newSetB, values_from = count)

doc_morphvs %>% filter(Pathgrp %in% c("2", "3"), CntCoE_7 < 1, CoETotal >= 400, CoE_abnormal_p >= 2.42) %>%
  group_by(Path, outProg) %>% summarize(count = n()) %>% pivot_wider(names_from = outProg, values_from = count)

# BOXPLOTS - DNA_Index difference in smoking
t.test(doc_morphvs$CoB_abnormal_p~doc_morphvs$SmokingHx, data = doc_morphvs, na.action = na.omit)
ggplot(data = doc_morphvs)+
  geom_boxplot(aes(x = SmokingHx, y = CoB_abnormal_p))

names(doc_morphvs)

#bpvar <- doc_morphvs %>% select(CoTotal:CoD_67_p) %>% names(.)
bpvar <- doc_morphvs %>% select(prCoB_3, prCoB_5, prCoB_6, prCoB_7, CoB_normal_p, CoB_abnormal_p, CoB_67_p) %>% names(.)
bpdf <- doc_morphvs %>% filter(!is.na(Smoking)) %>% droplevels() %>% as.data.frame()
bxplotlist <- list()

for (n in names(bpdf[c(which(colnames(bpdf) %in% bpvar))])){
  #wilp <- wilcox.test(bpdf[,n]~bpdf[,"Smoking"])
  #ttp <- t.test(bpdf[,n]~bpdf[,"Smoking"], data = bpdf, na.action = na.omit)
  avop <- aov(bpdf[,n]~bpdf[,"SmokingHx"], data = bpdf)
  bxplotlist[[n]] <- ggplot(bpdf) +
    geom_boxplot(aes_string(x = "SmokingHx", y = n, fill = "SmokingHx"))+
    labs(y = "Cells", x = paste(n, ' p = ', signif(summary(avop)[[1]][["Pr(>F)"]][1], digits = 2)), sep = "") +
    #labs(y = "Cells", x = paste(n, ' p = ', signif(avop$p.value, digits = 2)), sep = "") +
    scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10))+
    #scale_y_sqrt(breaks = seq(0, 100, 10))+
    #scale_fill_manual(name = "Smoking", values = c("#B2BD7E", "#D4AFB9"))+
    theme(axis.text.x = element_text(angle = 90), panel.background = element_rect(color = "black", fill = NA))
}

ggarrange(plotlist = bxplotlist, common.legend = TRUE, ncol = 4, nrow = 2)


# ROC on training sets ----
library(klaR)
library(pROC)
library(maxstat)

setname <- "newSet3"
cotype <- "CoTotal"
co7type <- "CoB_7"
cnt7 <- 1
plot_roc10 <- TRUE

if(!is.na(setname)){
  if(setname == "newSet2"){
    pagrp <- c("0", "0.5", "2", "3")
  } else if(setname == "newSet3"){
    pagrp <- c("0", "2", "3")
  } else if(setname == "newSet4"){
    pagrp <- c("0.5", "2", "3")
  } else if(setname == "newSetD"){
    pagrp <- c("0", "2", "3")
  }else if(setname == "newSetB"){
    pagrp <- c("0", "2", "3")
  }
  
  if(cotype == "CoTotal"){
    rocnames_10 <- c("CoB_3", "CoB_5", "CoB_6", "CoB_7", "prCoB_3", "prCoB_5", "prCoB_6", "prCoB_7", "CoB_normal_p", "CoB_abnormal_p")
    rocnames_2 <- c("prCoB_5", "CoB_abnormal_p")
  } else if(cotype == "CoTotal"){
    rocnames_10 <- c("CoD_3", "CoD_5", "CoD_6", "CoD_7", "prCoD_3", "prCoD_5", "prCoD_6", "prCoD_7", "CoD_normal_p", "CoD_abnormal_p")
    rocnames_2 <- c("prCoD_5", "CoD_abnormal_p")
  }
  
  if(is.na(cnt7)){
    roc_train_df <- doc_morphvs %>% filter(Pathgrp %in% pagrp, .[[cotype]] >= 400, .[[setname]] %in% c("train")) %>% droplevels() %>% data.frame()  
  } else if(cnt7 == 0){
    roc_train_df <- doc_morphvs %>% filter(Pathgrp %in% pagrp, .[[co7type]] == 0, .[[cotype]] >= 400, .[[setname]] %in% c("train")) %>% droplevels() %>% data.frame()  
  } else if(cnt7 == 1){
    roc_train_df <- doc_morphvs %>% filter(Pathgrp %in% pagrp, .[[co7type]] < 1, .[[cotype]] >= 400, .[[setname]] %in% c("train")) %>% droplevels() %>% data.frame()  
  }
  
  if(plot_roc10 == TRUE){
    rocplot_10 <- list()
    rocauc_10 <- list()
    for (v in names(roc_train_df[c(which(colnames(roc_train_df) %in% rocnames_10))])){
      rocobj <- roc(roc_train_df$Path, roc_train_df[,v], pos_class = "1", neg_class = "0", direction = "auto")
      
      roctbl <- as.data.frame(matrix(c(round(rocobj$thresholds,2), round(rocobj$specificities,2), round(rocobj$sensitivities,2)), ncol = 3)) %>% 
        rename(threshold = V1, SP = V2, SE = V3) 
      rocthresh <- roctbl %>% mutate(sumsesp = rowSums(across(SP:SE))) %>% filter(sumsesp == max(sumsesp))
      rocauc_10[[v]] <- round(as.numeric(auc(rocobj)),2)
      
      rocplot_10[[v]] <- ggroc(rocobj) +
        geom_point(data = rocthresh, aes(x = SP, y = SE), size = 3, pch = 3)+
        geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), linetype = "dashed", color = "gray50") +
        ggtitle(v, subtitle = paste("AUC:", rocauc_10[[v]], " | best:", rocthresh$threshold, sep = ""))+
        theme(panel.background = element_rect(color = "black", fill = NA), plot.title = element_text(face = "bold", size = 10),
              axis.title.x=element_blank(), axis.title.y = element_blank(), title = element_text(size = 8)) +
        geom_text_repel(data = rocthresh, aes(x = SP, y= SE), label = paste0("(", rocthresh$SP, ",",rocthresh$SE, ")"), nudge_x = 0, nudge_y = 0, direction = "both", size = 3)
      if(rocauc_10[[v]] >= 0.9 ){
        rocplot_10[[v]] <- rocplot_10[[v]] + geom_polygon(data = roctbl, aes(x = SP, y = SE), fill = "#D8973C", alpha = 0.3)
      } else{
        rocplot_10[[v]] <- rocplot_10[[v]]
      }
    }
    annotate_figure(ggarrange(plotlist = rocplot_10, nrow = 2, ncol = 5, common.legend =TRUE, legend = "left"), top = text_grob("ROC on Training", face = "bold", size = 10))
  } else if(plot_roc10 == FALSE){
    rocplot_2 <- list()
    rocauc_2 <- list()
    for (v in names(roc_train_df[c(which(colnames(roc_train_df) %in% rocnames_2))])){
      rocobj <- roc(roc_train_df$Path, roc_train_df[,v], pos_class = "1", neg_class = "0", direction = "auto")
      roctbl <- as.data.frame(matrix(c(round(rocobj$thresholds,2), round(rocobj$specificities,2), round(rocobj$sensitivities,2)), ncol = 3)) %>% 
        rename(threshold = V1, SP = V2, SE = V3) 
      rocthresh <- roctbl %>% mutate(sumsesp = rowSums(across(SP:SE))) %>% filter(sumsesp == max(sumsesp))
      rocauc_2[[v]] <- round(as.numeric(auc(rocobj)),2)
      
      rocplot_2[[v]] <- ggroc(rocobj) +
        geom_point(data = rocthresh, aes(x = SP, y = SE), size = 3, pch = 3)+
        geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), linetype = "dashed", color = "gray50") +
        ggtitle(v, subtitle = paste("AUC:", rocauc_2[[v]], " | best:", rocthresh$threshold, sep = ""))+
        theme(panel.background = element_rect(color = "black", fill = NA), plot.title = element_text(face = "bold", size = 10),
              axis.title.x=element_blank(), axis.title.y = element_blank(), title = element_text(size = 8)) +
        geom_text_repel(data = rocthresh, aes(x = SP, y= SE), label = paste0("(", rocthresh$SP, ",",rocthresh$SE, ")"), nudge_x = 0, nudge_y = 0, direction = "both", size = 3)
      if(rocauc_2[[v]] >= 0.9 ){
        rocplot_2[[v]] <- rocplot_2[[v]] + geom_polygon(data = roctbl, aes(x = SP, y = SE), fill = "#D8973C", alpha = 0.3)
      } else{
        rocplot_2[[v]] <- rocplot_2[[v]]
      }
    }
    ggarrange(plotlist = rocplot_2, nrow = 2, ncol = 1, common.legend =TRUE, legend = "left")
  }
}

# single ROC ----
roc_train_df <- doc_morphvs %>% filter(Pathgrp %in% c("0", "0.5", "1", "2", "3"), CntCoD_7 < 1, CoDTotal >= 400) %>% droplevels() %>% data.frame()
roc_train_df <- doc_morphvs %>% filter(Pathgrp %in% c("0", "2", "3"), CoB_7 < 1, CoTotal >= 400, newSet3 %in% c("train")) %>% droplevels() %>% data.frame()

rocvar <- c("prbin115", "bin100", "bin175", "prbin110")
rocvar <- "CoB_abnormal_p"
rocvar <- "CoDratio"

rocobj_1 <- roc(roc_train_df$Path, roc_train_df[,rocvar], pos_class = "1", neg_class = "0", direction = "auto")
rocobj1_tbl <- as.data.frame(matrix(c(round(rocobj_1$thresholds,2), rocobj_1$specificities, round(rocobj_1$sensitivities,2)), ncol = 3)) %>%
  rename(threshold = V1, SP = V2, SE = V3) %>% mutate(sumsesp = rowSums(across(SP:SE)))
rocobj1_thresh <- as.data.frame(matrix(c(round(rocobj_1$thresholds,2), round(rocobj_1$specificities,2), round(rocobj_1$sensitivities,2)), ncol = 3)) %>%
  rename(threshold = V1, SP = V2, SE = V3) %>% mutate(sumsesp = rowSums(across(SP:SE))) %>% filter(sumsesp == max(sumsesp))

ggroc(rocobj_1) +
  geom_point(aes(x = rocobj1_thresh$SP, y = rocobj1_thresh$SE), size = 5, pch = 3)+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), linetype = "dashed", color = "gray50") +
  ggtitle(rocvar, subtitle = paste("AUC:", round(rocobj_1$auc,2), " | best:", rocobj1_thresh$threshold, sep = ""))+
  theme(panel.background = element_rect(color = "black", fill = NA), plot.title = element_text(face = "bold", size = 12),
        axis.title.x=element_blank(), axis.title.y = element_blank(), title = element_text(size = 12))+
  geom_text_repel(data = rocobj1_thresh, aes(x = SP, y= SE), label = paste("(", rocobj1_thresh$SP, ",", rocobj1_thresh$SE, ")", sep = ""), 
                  nudge_x = 0, nudge_y = 0, direction = "both", size = 5)+
  geom_polygon(data = rocobj1_tbl, aes(x = SP, y = SE), fill = "#D8973C", alpha = 0.3)

# LDA and ROC with logistic model ----
library(klaR)
library(pROC)
library(maxstat)

doc_ploidy <- left_join(docgrp_wide, docbrvs %>% select(Barcode, Path, Pathgrp, outProg, newSetB, newSetD)) %>% left_join(., docbinsup) %>% select(Barcode, Path, Pathgrp, everything()) %>% 
  filter(Barcode %in% slide1)

ldadf <- doc_ploidy %>% filter(Pathgrp %in% c("0", "2", "3"), CntCoB_7 < 1, CoBTotal >= 400, newSetB == "train") %>% droplevels() %>%
  select(Barcode, Path, CntCoB_3:CntCoB_7, PropCoB_3:PropCoB_7, CoB_abnormal_p, CoB_67_p, CntCoD_3:CntCoD_7, PropCoD_3:PropCoD_7, CoD_abnormal_p, CoD_67_p, bin80:prsup300)

names(ldadf)
step <- stepclass(ldadf[,c(3:152)], ldadf$Path, method='lda', maxvar=Inf, direction='forward', criterion = "AC", improvement = 0.01)
step <- stepclass(ldadf[,c(3:22)], ldadf$Path, method='lda', maxvar=Inf, direction='forward', criterion = "AC", improvement = 0.01)

# f <- ldadf$Path ~ prbin115+prbin110+bin105+bin175+prbin190+prbin180+prbin165+prbin205
# lda_fit <- lda(f, ldadf)
# glm_fit <- glm(Path~prbin115+bin100+bin175+prbin120, family = "binomial", data = ldadf)

# rocobj_ldafit <- plot.roc(ldadf$Path~predict(lda_fit)$posterior[,2], ci=TRUE,of="thresholds",thresholds="best",print.thres="best")
# rocobj_glmfit <- plot.roc(ldadf$Path~predict(glm_fit), ci=TRUE,of="thresholds",thresholds="best",print.thres="best")

lmodel <- glm(Path ~ prbin110+bin105+bin165, family = "binomial", data = ldadf)
summary(lmodel)

predicted <- predict(lmodel, ldadf, type = "response", pos_class = "1", neg_class = "0", direction = "auto")
glmrocobj <- roc(ldadf$Path, predicted)

roclda_tbl <- as.data.frame(matrix(c(round(glmrocobj$thresholds,2), glmrocobj$specificities, round(glmrocobj$sensitivities,2)), ncol = 3)) %>%
  rename(threshold = V1, SP = V2, SE = V3) %>% mutate(sumsesp = rowSums(across(SP:SE)))

roclda_thresh <- as.data.frame(matrix(c(round(glmrocobj$thresholds,2), round(glmrocobj$specificities,2), round(glmrocobj$sensitivities,2)), ncol = 3)) %>%
  rename(threshold = V1, SP = V2, SE = V3) %>% mutate(sumsesp = rowSums(across(SP:SE))) %>% filter(sumsesp == max(sumsesp))

ggroc(glmrocobj) +
  geom_point(aes(x = roclda_thresh$SP, y = roclda_thresh$SE), size = 5, pch = 3)+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), linetype = "dashed", color = "gray50") +
  ggtitle("GLM model score", subtitle = paste("AUC:", round(glmrocobj$auc,2), " | best:", roclda_thresh$threshold, sep = ""))+
  theme(panel.background = element_rect(color = "black", fill = NA), plot.title = element_text(face = "bold", size = 12),
        axis.title.x=element_blank(), axis.title.y = element_blank(), title = element_text(size = 12))+
  geom_text_repel(data = roclda_thresh, aes(x = SP, y= SE), label = paste("(", roclda_thresh$SP, ",", roclda_thresh$SE, ")", sep = ""), 
                  nudge_x = 0, nudge_y = 0, direction = "both", size = 5)+
  geom_polygon(data = roclda_tbl, aes(x = SP, y = SE), fill = "#D8973C", alpha = 0.3)

threshold_table <- ci.thresholds(ldadf$Path, ldadf$CoB_abnormal_p, boot.n=1000, conf.level=0.95, stratified=FALSE) %>% data.frame() %>% 
  rownames_to_column(var = "thresholds") %>%
  mutate(maxsesp = rowSums(across(specificity.50.:sensitivity.50.)))
sim_best_co <- threshold_table %>% filter(maxsesp == max(maxsesp)) %>% select(thresholds, specificity.50., sensitivity.50.)

# GLM prediction on test set
glm_test <- doc_ploidy %>% filter(Pathgrp %in% c("0", "2", "3"), CntCoB_7 < 1, CoBTotal >= 400, newSetB == "test") %>% droplevels() %>%
  select(Barcode, Path, CntCoB_3:CntCoB_7, PropCoB_3:PropCoB_7, CoB_abnormal_p, CoB_67_p,
         CntCoD_3:CntCoD_7, PropCoD_3:PropCoD_7, CoD_abnormal_p, CoD_67_p, bin80:prsup300)

glm_valid <- doc_ploidy %>% filter(Pathgrp %in% c("0", "2", "3"), CntCoB_7 < 1, CoBTotal >= 400, newSetB == "valid") %>% droplevels() %>%
  select(Barcode, Path, CntCoB_3:CntCoB_7, PropCoB_3:PropCoB_7, CoB_abnormal_p, CoB_67_p,
         CntCoD_3:CntCoD_7, PropCoD_3:PropCoD_7, CoD_abnormal_p, CoD_67_p, bin80:prsup300)

glm_lgl <- doc_ploidy %>% filter(Pathgrp %in% c("0.5", "1"), CntCoB_7 < 1, CoBTotal >= 400) %>% droplevels() %>%
  select(Barcode, Path, outProg, CntCoB_3:CntCoB_7, PropCoB_3:PropCoB_7, CoB_abnormal_p, CoB_67_p,
         CntCoD_3:CntCoD_7, PropCoD_3:PropCoD_7, CoD_abnormal_p, CoD_67_p, bin80:prsup300)

glm_pred_test <- table(as.matrix((glm031 = predict(lmodel, glm_test, type = "response", pos_class = "1", neg_class = "0", direction = "auto")))) %>% 
  as.data.frame() %>% mutate(glm031 = as.numeric(glm031)) %>%
  cbind(glm_test, .) %>% select(-Freq) %>% select(Barcode, Path, glm031) %>% mutate(glm031co = as.factor(ifelse(glm031 < 0.31, "0", "1")))

glm_pred_valid <- as.data.frame(as.matrix((glm031 = predict(lmodel, glm_valid, type = "response", pos_class = "1", neg_class = "0", direction = "auto")))) %>%
  mutate(glm031 = as.numeric(glm031)) %>%
  cbind(glm_valid, .) %>% select(-V1) %>% select(Barcode, Path, glm031) %>% mutate(glm031co = as.factor(ifelse(glm031 < 0.31, "0", "1")))

glm_pred_lgl <- as.data.frame(as.matrix((glm031 = predict(lmodel, glm_lgl, type = "response", pos_class = "1", neg_class = "0", direction = "auto")))) %>%
  mutate(glm031 = as.numeric(glm031)) %>%
  cbind(glm_lgl, .) %>% select(-V1) %>% select(Barcode, Path, outProg, glm031) %>% mutate(glm031co = as.factor(ifelse(glm031 < 0.31, "0", "1")), outProg = as.factor(outProg))

confusionMatrix(reference = glm_pred_lgl$outProg, data = glm_pred_lgl$glm031co, positive = "1")

# confusion matrix on training, test, validation sets ----
library(caret)
library(gridExtra)
library(grid)
library(gtable)

tt1 <- ttheme_default(rowhead = list(fg_params = list(fontfamily = "arial", fontface = "bold", fontsize = 10)),
                      core = list(fg_params = list(fontfamily = "arial", fontface = "plain", fontsize = 10)),
                      colhead = list(fg_params = list(fontfamily = "arial", fontface = "bold", fontsize = 10)))

tt2 <- ttheme_default(rowhead = list(fg_params = list(fontfamily = "arial", fontface = "bold", fontsize = 10)),
                      core = list(fg_params = list(fontfamily = "arial", fontface = "plain", fontsize = 10),
                                  bg_params = list(fill = c("white", "gray", "gray", "white"), col = NA)),
                      colhead = list(fg_params = list(fontfamily = "arial", fontface = "bold", fontsize = 10)))

setname <- "newSet3"
cotype <- "CoTotal"
co7type <- "CoB_7"
prop5_name <- "prCoB_5"
propAb_name <- "CoB_abnormal_p"

cnt7 <- 1
prop5_co <- 2.32
propAb_co <- 2.42

if(!is.na(setname)){
  if(setname == "newSet2"){
    pagrp <- c("0", "0.5", "2", "3")
  } else if(setname == "newSet3"){
    pagrp <- c("0", "2", "3")
  } else if(setname == "newSet4"){
    pagrp <- c("0.5", "2", "3")
  } else if(setname == "newSetD"){
    pagrp <- c("0", "2", "3")
  }else if(setname == "newSetB"){
    pagrp <- c("0", "2", "3")}
  
  if(is.na(cnt7)){
    roc_confm <- doc_morphvs %>% filter(Pathgrp %in% pagrp, .[[cotype]] >= 400) %>% droplevels() %>% data.frame() %>%
      mutate(p_gp5_co = as.factor(ifelse(.[[prop5_name]] > prop5_co, "1", "0")), p_gp5_co = factor(p_gp5_co, levels = c("0", "1")),
             p_abnormal_co = as.factor(ifelse(.[[propAb_name]] > propAb_co, "1", "0")), p_abnormal_co = factor(p_abnormal_co, levels = c("0", "1")))
    co_train <- roc_confm %>% filter(.[[setname]] %in% c("train")) %>% droplevels()
    co_test <- roc_confm %>% filter(.[[setname]] =="test") %>% droplevels()
    co_valid <- roc_confm %>% filter(.[[setname]] %in% c("valid")) %>% droplevels()
  } else if(cnt7 == 0){
    roc_confm <- doc_morphvs %>% filter(Pathgrp %in% pagrp, .[[co7type]] == 0, .[[cotype]] >= 400) %>% droplevels() %>% data.frame() %>%
      mutate(p_gp5_co = as.factor(ifelse(.[[prop5_name]] > prop5_co, "1", "0")), p_gp5_co = factor(p_gp5_co, levels = c("0", "1")),
             p_abnormal_co = as.factor(ifelse(.[[propAb_name]] > propAb_co, "1", "0")), p_abnormal_co = factor(p_abnormal_co, levels = c("0", "1")))
    co_train <- roc_confm %>% filter(.[[setname]] == "train") %>% droplevels()
    co_test <- roc_confm %>% filter(.[[setname]] =="test") %>% droplevels()
    co_valid <- roc_confm %>% filter(.[[setname]] == "valid") %>% droplevels()
  } else if(cnt7 == 1){
    roc_confm <- doc_morphvs %>% filter(Pathgrp %in% pagrp, .[[co7type]] < 1, .[[cotype]] >= 400) %>% droplevels() %>% data.frame()%>%
      mutate(p_gp5_co = as.factor(ifelse(.[[prop5_name]] > prop5_co, "1", "0")), p_gp5_co = factor(p_gp5_co, levels = c("0", "1")),
             p_abnormal_co = as.factor(ifelse(.[[propAb_name]] > propAb_co, "1", "0")), p_abnormal_co = factor(p_abnormal_co, levels = c("0", "1")))
    co_train <- roc_confm %>% filter(.[[setname]] == "train") %>% droplevels()
    co_test <- roc_confm %>% filter(.[[setname]] =="test") %>% droplevels()
    co_valid <- roc_confm %>% filter(.[[setname]] == "valid") %>% droplevels()
  } else if(cnt7 == 2){
    roc_confm <- doc_morphvs %>% filter(Pathgrp %in% pagrp, .[[co7type]] < 2, .[[cotype]] >= 400) %>% droplevels() %>% data.frame()%>%
      mutate(p_gp5_co = as.factor(ifelse(.[[prop5_name]] > prop5_co, "1", "0")), p_gp5_co = factor(p_gp5_co, levels = c("0", "1")),
             p_abnormal_co = as.factor(ifelse(.[[propAb_name]] > propAb_co, "1", "0")), p_abnormal_co = factor(p_abnormal_co, levels = c("0", "1")))
    co_train <- roc_confm %>% filter(.[[setname]] == "train") %>% droplevels()
    co_test <- roc_confm %>% filter(.[[setname]] =="test") %>% droplevels()
    co_valid <- roc_confm %>% filter(.[[setname]] == "valid") %>% droplevels()
  }
}

cmdflist <- list(co_train, co_test) # change dataframes
names(cmdflist) <- c("train", "test") # change names of dataframes

cmdfcotype <- c("p_gp5_co") # change the cutoff variables for confusion matrix
valigned_list <- list()
valigned_combine <- list()
for (i in 1:length(cmdflist)){
  cmdf <- cmdflist[[i]]
  for (co in names(cmdf[c(which(colnames(cmdf) %in% cmdfcotype))])){
    cmres <- confusionMatrix(reference = cmdf$Path, data = cmdf[, co], positive = "1")
    
    subtitle <- textGrob(names(cmdflist[i]), x = 0, hjust = 0, gp=gpar(fontface = "bold", fontsize = 10))
    cmtable <- gtable_add_rows(tableGrob(cmres$table, theme = tt2), heights = grobHeight(subtitle)+unit(3, "mm"), pos = 0)
    cmtable <- gtable_add_grob(cmtable, subtitle, 1, 1, 1, ncol(cmtable))
    
    cmclass <- tableGrob(as.table(as.matrix(c(round(cmres$byClass["Sensitivity"],3), round(cmres$byClass["Specificity"],3),
                                              round(cmres$byClass["Pos Pred Value"],3), round(cmres$byClass["Neg Pred Value"],3), round(cmres$overall["Accuracy"],3)))),
                         cols = NULL, rows = c("SE", "SP", "PPV", "NPV", "ACC"), theme = tt1)
    valigned_combine[[co]] <- gtable_combine(cmtable, cmclass, along = 2)
  }
  #valigned_list[[i]] <- grid.arrange(valigned_combine[[1]], valigned_combine[[2]], valigned_combine[[3]], valigned_combine[[4]], nrow = 1, left = textGrob(names(cmdflist[i])))
  #valigned_list[[i]] <- grid.arrange(valigned_combine[[1]], nrow = 1, left = textGrob(names(cmdflist[i])))
  valigned_list[[i]] <- grid.arrange(valigned_combine[[1]], nrow = 1)
}
gp5_comf <- grid.arrange(valigned_list[[1]], valigned_list[[2]], nrow = 1, top = textGrob(paste(cmdfcotype, ">", prop5_co), gp=gpar(fontsize = 9, font = 2)))

cmdfcotype <- c("p_abnormal_co") # change the cutoff variables for confusion matrix
valigned_list <- list()
valigned_combine <- list()
for (i in 1:length(cmdflist)){
  cmdf <- cmdflist[[i]]
  for (co in names(cmdf[c(which(colnames(cmdf) %in% cmdfcotype))])){
    cmres <- confusionMatrix(reference = cmdf$Path, data = cmdf[, co], positive = "1")
    
    subtitle <- textGrob(names(cmdflist[i]), x = 0, hjust = 0, gp=gpar(fontface = "bold", fontsize = 10))
    cmtable <- gtable_add_rows(tableGrob(cmres$table, theme = tt2), heights = grobHeight(subtitle)+unit(3, "mm"), pos = 0)
    cmtable <- gtable_add_grob(cmtable, subtitle, 1, 1, 1, ncol(cmtable))
    
    cmclass <- tableGrob(as.table(as.matrix(c(round(cmres$byClass["Sensitivity"],3), round(cmres$byClass["Specificity"],3),
                                              round(cmres$byClass["Pos Pred Value"],3), round(cmres$byClass["Neg Pred Value"],3), round(cmres$overall["Accuracy"],3)))),
                         cols = NULL, rows = c("SE", "SP", "PPV", "NPV", "ACC"), theme = tt1)
    valigned_combine[[co]] <- gtable_combine(cmtable, cmclass, along = 2)
  }
  #valigned_list[[i]] <- grid.arrange(valigned_combine[[1]], valigned_combine[[2]], valigned_combine[[3]], valigned_combine[[4]], nrow = 1, left = textGrob(names(cmdflist[i])))
  #valigned_list[[i]] <- grid.arrange(valigned_combine[[1]], nrow = 1, left = textGrob(names(cmdflist[i])))
  valigned_list[[i]] <- grid.arrange(valigned_combine[[1]], nrow = 1)
}
abnormal_comf <- grid.arrange(valigned_list[[1]], valigned_list[[2]], nrow = 1, top = textGrob(paste(cmdfcotype, ">", propAb_co), gp=gpar(fontsize = 9, font = 2)))

ggarrange(gp5_comf, abnormal_comf, nrow = 2)

# confusion matrix of validation set
co_valid %>% group_by(Path, p_gp5_co) %>% summarize(count = n()) %>%
  pivot_wider(names_from = "Path", values_from = count, names_prefix = "Path")
co_valid %>% group_by(Path, p_abnormal_co) %>% summarize(count = n()) %>% 
  pivot_wider(names_from = "Path", values_from = count, names_prefix = "Path")

# survival curves ----
library(survminer)
library(survival)

# Grp5_p <- 0.45
# GrpAb_p <- 0.47
Grp5_7p <- 2.32
GrpAb_7p <- 2.42

# cnt7 <- 1
# survyear <- 5
# survcolist <- list()

if(is.na(cnt7)){
  survdf <- doc_morphvs %>% filter(survD == "1", CntCoD < 1, CoDTotal >= 400) %>% droplevels() %>%
    mutate(gp5_co = as.factor(ifelse(PropCoD_5 <= Grp5_p, "0", "1")),
           gp567_co = as.factor(ifelse(CoD_abnormal_p <= GrpAb_p, "0", 1)), time = as.numeric(outdate - BrDate)/365.25)
  # survdf <- doc_morphvs %>% filter(Pathgrp %in% c("0.5", "1"), CoBTotal >= 400) %>% droplevels() %>% 
  #   mutate(gp5_co = as.factor(ifelse(PropCoB_5 <= Grp5_p, "0", "1")),
  #          gp567_co = as.factor(ifelse(CoB_abnormal_p <= GrpAb_p, "0", 1)), time = as.numeric(outdate - BrDate)/365.25)
  cob5p = Grp5_p
  cob567p = GrpAb_p
  } else if(cnt7 == 1){
  survdf <- doc_morphvs %>% filter(survD == "1", CntCoD_7 < 1, CoDTotal >= 400) %>% droplevels() %>% 
    mutate(gp5_co = as.factor(ifelse(PropCoD_5 <= Grp5_7p, "0", "1")),
           gp567_co = as.factor(ifelse(CoD_abnormal_p <= GrpAb_7p, "0", 1)), time = as.numeric(outdate - BrDate)/365.25)
  # survdf <- doc_morphvs %>% filter(Pathgrp %in% c("0.5", "1"), CntCoB_7 < 1, CoBTotal >= 400) %>% droplevels() %>% 
  #   mutate(gp5_co = as.factor(ifelse(PropCoB_5 <= Grp5_p, "0", "1")),
  #          gp567_co = as.factor(ifelse(CoB_abnormal_p <= GrpAb_p, "0", 1)), time = as.numeric(outdate - BrDate)/365.25)
  cob5p = Grp5_7p
  cob567p = GrpAb_7p
  }

for (v in names(survdf[c(which(colnames(survdf) %in% c("gp5_co", "gp567_co")))])){
  if(v == "gp5_co" & is.na(cnt7)){
    cobp = Grp5_p
    plottitle = "GP5%"
  } else if(v == "gp567_co" & is.na(cnt7)){
    cobp = GrpAb_p
    plottitle = "GP567%"
  } else if(v == "gp5_co" & cnt7 == 1){
    cobp = Grp5_7p
    plottitle = "GP5%"
  } else if(v == "gp567_co" & cnt7 == 1){
    cobp = GrpAb_7p
    plottitle = "GP567%"
  }
  if(survyear == 3){
    fit <- survfit(Surv(time, outProg3)~survdf[[v]], data = survdf)
    survcolist[[v]] <- ggsurvplot(fit, data = survdf, pval = TRUE, pval.method = TRUE, conf.int = TRUE, palette = c("black", "#95190C"),
                                  xlab = "years", break.x.by=1, xlim = c(0,3),
                                  legend.title = plottitle, legend.labs = c(paste("Low", "\u2264", cobp, "%", sep = ""), paste("High>", cobp, "%", sep = "")))
  } else if(survyear == 5){
    fit <- survfit(Surv(time, outProg5)~survdf[[v]], data = survdf)
    survcolist[[v]] <- ggsurvplot(fit, data = survdf, pval = TRUE, pval.method = TRUE, conf.int = TRUE, palette = c("black", "#95190C"),
                                  xlab = "years", break.x.by=1, xlim = c(0,5),
                                  legend.title = plottitle, legend.labs = c(paste("Low", "\u2264", cobp, "%", sep = ""), paste("High>", cobp, "%", sep = "")))
  }
}
arrange_ggsurvplots(survcolist, nrow = 2, ncol = 1)

survdf_single <- doc_morphvs %>% filter(Pathgrp %in% c("0.5", "1"), CoB_7 < 1, CoTotal >= 400) %>% droplevels() %>%
  rbind(., doc_morphvs %>% filter(Pathgrp %in% c("0", "2", "3"), CoB_7 < 1, CoTotal >= 400, newSet3 == "valid") %>% droplevels()) %>%
  mutate(gp5_co = as.factor(ifelse(prCoB_5 < Grp5_7p, "0", "1")), gp567_co = as.factor(ifelse(CoB_abnormal_p < GrpAb_7p, "0", "1")),
         time = as.numeric(outdate - BrDate)/365.25)

fit <- survfit(Surv(time, outProg3)~gp567_co, data = survdf_single)
lgpval <- round(survdiff(Surv(time, outProg3) ~ gp567_co, data = survdf_single)$pvalue,2)
attr(surv_summary(fit), "table")
ggsurvplot(fit, data = survdf_single, pval = lgpval, pval.method = TRUE, conf.int = TRUE, palette = c("black", "#95190C"),
           xlab = "years", legend = c("top"),
           xlim = c(0, 3), break.x.by = 1,
           #risk.table = TRUE,
           legend.title = "Abnormal", legend.labs = c(paste("Low", "\u2264", GrpAb_7p, "%", sep = ""), paste("High>", GrpAb_7p, "%", sep = "")))

survdf_single %>% filter(gp567_co == "0", outProg5 == "1") %>% select(Barcode, Dx, outProg, outdate, time, PropCoB_5, CoB_abnormal_p, gp5_co, gp567_co, CntCoB_6, CntCoB_7)

doc_morphvs %>% filter(Pathgrp %in% c("2", "3"), CoDTotal >= 400, CntCoD_7 < 1) %>% filter(CoD_abnormal_p > 0.47) %>% group_by(newSetD, outProg5) %>% summarize(count = n())
doc_morphvs %>% filter(Pathgrp %in% c("0.5", "1"), outProg == "0", CoB_abnormal_p >= 2.42)

# cut off survival ----
surv_cut <- doc_morphvs %>% filter(survanalysis == "1", CoBTotal >= 400) %>% droplevels() %>% mutate(time = as.numeric(outdate - BrDate)/365.25)

res.cut <- surv_cutpoint(surv_cut, time = "time", event = "outProg5", variables = c("CoB_abnormal_p"))
res.cat <- surv_categorize(res.cut)

fit <- survfit(Surv(time, outProg5)~CoB_abnormal_p, data = res.cat)
attr(surv_summary(fit), "table")
ggsurvplot(fit, data = res.cat, pval = TRUE, conf.int = TRUE, xlab = "years", legend = c("right"), xlim = c(0, 5), break.x.by = 1, legend.title = "Abnormal")

# plot DNA_Index histogram by group ----
DOChrid <- doc_morphvs %>% filter(Pathgrp %in% c("2", "3")) %>% droplevels() %>% slice(252:270) %>% pull(Barcode)

ggplot(docgrp %>% filter(Barcode %in% DOChrid))+
#ggplot(docgrp %>% filter(Barcode == "0116A120504"))+
  #geom_histogram(aes(x = DNA_Index), fill = "#3F85EC", color = "gray20", binwidth = 0.05) +
  geom_histogram(aes(x = DNA_Index, fill = CoB), color = "gray20", binwidth = 0.05) +
  #scale_fill_manual(name = "DI", values = c("#D3D3F7", "#5D5EE4", "#3334A2", "black"), labels = c(">0.85-1.10", ">1.10-1.70", ">1.70-2.25", ">2.25"))+
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 0.85, 1.1, 1.15, 1.7, 2.25, 2.3, 3, 4, 5))+ 
  scale_y_sqrt(breaks = seq(0, 1000, 200))+
  geom_vline(xintercept = c(0.85, 1.1, 1.15, 1.7, 2.30), linetype = "dashed", color = "gray30")+
  facet_wrap(.~Barcode)+
  ggtitle("DNA ploidy histogram:\nisolated epithelial cell nuclei")+
  ylab("Number of nuclei")+  xlab("DNA Index")+
  theme(legend.position = "none", plot.title = element_text(size = 10),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "gray", fill = NA), axis.text.x = element_text(angle = 90))


# Summary of clinical variables ----
library(gtsummary)

names(docbrvs12)
# by lesions
pt <- docbrvs12 %>% mutate(Gender = as.factor(ifelse(Gender == "1", "Female", "Male")), 
                           SmokingHx = as.factor(ifelse(SmokingHx == "0", "Never", "Ever")), 
                           SmokingGrp = as.factor(ifelse(Smoking %in% c("Non-Smoker", "SecondHand"), "Non-smoker", 
                                                                        ifelse(Smoking == "S_Current", "Smoker (Current)",
                                                                               ifelse(Smoking == "S_Former", "Smoker (Former)", NA))))) %>%
  select(PtId, Age, Gender, SmokingGrp) %>% group_by(PtId) %>% mutate(minAge = min(Age)) %>% ungroup() %>% select(-Age) %>% 
  distinct(.keep_all = TRUE)

pt %>% select(minAge, Gender, SmokingGrp) %>%
  tbl_summary(statistic = list(all_categorical()~"{n} ({p}%)", minAge ~ "{mean} ({sd})"),
              label = list(SmokingGrp ~ "Smoking", minAge ~ "Age (mean, SD)")) %>%
  bold_labels() %>% modify_caption("**Table 1. Patient Demograhpics**")

lesion <- docbrvs12 %>% 
  mutate(SiteGrp = ifelse(SiteRisk == "1", "BM/GING/HP", ifelse(SiteRisk == "2", "SP", "TONG/FOM"))) %>%
  mutate(PathGrp3 = ifelse(Pathgrp == "0", "Normal control",
                           ifelse(Pathgrp %in% c("0.5", "1"), "Reactive/LGL", "HGL/SCC"))) %>%
  mutate(PathGrp3 = factor(PathGrp3, levels = c("Normal control", "Reactive/LGL", "HGL/SCC"))) %>%
  select(PathGrp3, SiteGrp, LsWLGrp2, LesionLength, LesionWidth)


lesion %>% tbl_summary(by = "LsWLGrp2", 
                       statistic = list(all_categorical()~"{n} ({p}%)",
                                        LesionLength ~ "{mean} ({sd})", 
                                        LesionWidth ~ "{mean} ({sd})"),
                       label = list(PathGrp3 ~ "Pathology",
                                     SiteGrp ~ "Lesion site")) %>%
  add_p(test = list(all_continuous()~"anova",
                    all_categorical()~"chisq.test")) %>%
  add_overall()

doc_morphvs %>% filter(Barcode == "512952133") %>% select(Barcode, CoTotal, CoB_7, CoB_abnormal_p)

# table of cellularity ----
doc_morphvs <- doc_morphvs %>% mutate(Pathgrp2 = as.factor(Pathgrp2))
cnpbp <- ggboxplot(doc_morphvs, x = "LsWLGrp2", y = "CoTotal", outlier.shape = NA)+
  stat_compare_means(comparison = list(c("Negative/Scar", "Positive")),
                     label.y = c(10000, 12000))+ 
  scale_y_continuous(limits = c(0, 18000))+
  xlab("Clinical group") +
  ylab("Isolated epithelial cells")+
  theme(panel.background = element_rect(fill = NA, color = "gray"))

pnpbp <- ggboxplot(doc_morphvs, x = "Pathgrp2", y = "CoTotal", outlier.shape = NA)+
  stat_compare_means(comparison = list(c("0", "1"), c("0", "2"), c("1", "2")),
                     label.y = c(10000, 12000, 15000))+ 
  scale_y_continuous(limits = c(0, 18000))+
  scale_x_discrete(limits = c("0", "1", "2", "NA"), labels = c("Normal", "Reactive/LGL", "HGL/SCC", "non-current path"))+
  xlab("Pathology") +
  ylab("Isolated epithelial cells")+
  theme(panel.background = element_rect(fill = NA, color = "gray"))


ggarrange(cnpbp, pnpbp)
#dxbp + geom_pwc(aes(group = Pathgrp2), tip.leghth = 0, method = "wilcox_test")

test <- doc_morphvs %>% filter(!is.na(Dx3)) %>% droplevels()
wilcox.test(doc_morphvs$CoTotal~doc_morphvs$LsWLGrp2)
kruskal.test(test$CoTotal~test$Pathgrp2)
pairwise.wilcox.test(test$CoTotal, test$Pathgrp2, p.adjust.method = "BH")

# lesion size and cellularity ----
names(doc_morphvs)
ls_size <- doc_morphvs %>% filter(LsWLGrp2 == "Positive") %>% droplevels() %>%
  select(LsWLGrp2, CoTotal, LesionLength, LesionWidth) %>% 
  mutate(maxdim = ifelse(LesionLength >= LesionWidth, LesionLength, LesionWidth))

ggplot(data = ls_size) +
  geom_point(aes(x = maxdim, y = CoTotal))+
  xlab("greatest dimension of clinically positive lesions (mm)") +
  ylab("Isolated epithelial cells")+
  theme(panel.background = element_rect(fill = NA, color = "gray"))

# plots of DI ----
celldi <- doc_morphvs %>% select(Barcode, Pathgrp2, Pathgrp, Path, LsWLGrp2) %>% left_join(., docgrp) %>% filter(!is.na(DNA_Index)) %>% droplevels()
celldi %>% group_by(Pathgrp) %>% summarize(mean=mean(DNA_Index))

celldi_path <- celldi %>% filter(!is.na(Pathgrp)) %>% droplevels()
t.test(celldi_path$DNA_Index~celldi_path$Path)
t.test(celldi$DNA_Index~celldi$LsWLGrp2)

names(celldi_path)
celldi %>% filter(is.na(DNA_Index))

dicnpbp <- ggboxplot(celldi, x = "LsWLGrp2", y = "DNA_Index")+
  stat_compare_means(comparison = list(c("Negative/Scar", "Positive")),
                     label = "p.signif")+
  #scale_y_continuous(limits = c(0, 18000))+
  xlab("Clinical group") +
  ylab("Isolated epithelial cells")+
  theme(panel.background = element_rect(fill = NA, color = "gray"))

dipnbp <- ggboxplot(celldi_path, x = "Path", y = "DNA_Index")+
  stat_compare_means(comparison = list(c("0", "1")),
                     label = "p.signif")+
  #scale_y_continuous(limits = c(0, 18000))+
  scale_x_discrete(limits = c("0", "1"), labels = c("Normal/Reactive/LGL", "HGL/SCC"))+
  xlab("Pathology") +
  ylab("Isolated epithelial cells")+
  theme(panel.background = element_rect(fill = NA, color = "gray"))

ggarrange(dicnpbp, dipnbp, nrow = 1, ncol = 2)

# cdihisto <- ggplot(data = celldi)+
#   geom_histogram(aes(x = DNA_Index, fill = LsWLGrp2), binwidth = 0.05, color = "gray60", )+
#   scale_fill_manual(values = c("#F7C1BB", "#2274A5"), labels = c("Negative\nScar", "Positive"), name = "Clinical")+
#   scale_y_sqrt()+ ylab("sqrt(Isolated epithelial cells)")+xlab("DNA Index")+
#   scale_x_continuous(limits = c(0,9), breaks = seq(0, 9, 0.5))+theme(panel.background = element_rect(fill = NA, color = "gray"))
# 
# pdihisto <- ggplot(data = celldi_path)+
#   geom_histogram(aes(x = DNA_Index, fill = Path), binwidth = 0.05, color = "gray50", )+
#   scale_fill_manual(values = c("#73A580", "#B80C09"), labels = c("normal\nreactive\nLGL", "HGL/SCC"), name = "Pathology")+
#   scale_y_sqrt()+ ylab("sqrt(Isolated epithelial cells)")+xlab("DNA Index")+
#   scale_x_continuous(limits = c(0,9), breaks = seq(0, 9, 0.5))+theme(panel.background = element_rect(fill = NA, color = "gray"))


# bin plots ----
names(doc_morphvs)
binname_order <- factor(c(binname, "bin999"), levels = c(binname, "bin999"))
bin_long <- doc_morphvs %>% select(Barcode, LsWLGrp2, Path, CoTotal, bin85:bin999) %>%
  pivot_longer(cols = c(bin85:bin999), names_to = "bins", values_to = "count") %>%
  mutate(bins = ifelse(is.na(bins), "bin999", as.vector(bins))) %>%
  mutate(bins = factor(bins, levels = binname_order))

cbinbar <- ggplot(bin_long)+
  geom_bar(aes(x = bins, y = count, fill = LsWLGrp2), stat = "identity")+
  scale_y_sqrt()+ ylab("sqrt(Isolated epithelial cells)")+xlab("Bins of DNA Index")+
  scale_fill_manual(values = c("#F7C1BB", "#2274A5"), labels = c("Negative\nScar", "Positive"), name = "Clinical")+
  theme(panel.background = element_rect(fill = NA, color = "gray"),
        axis.text.x = element_text(angle = 90, size = 8))

pbinbar <- ggplot(bin_long %>% filter(!is.na(Path)) %>% droplevels())+
  geom_bar(aes(x = bins, y = count, fill = Path), stat = "identity")+
  scale_y_sqrt()+ ylab("sqrt(Isolated epithelial cells)")+xlab("Bins of DNA Index")+
  scale_fill_manual(values = c("#73A580", "#B80C09"), labels = c("normal\nreactive\nLGL", "HGL/SCC"), name = "Pathology")+
  theme(panel.background = element_rect(fill = NA, color = "gray"),
        axis.text.x = element_text(angle = 90, size = 8))

ggarrange(cbinbar+rremove("xlab"), pbinbar, nrow = 2, ncol = 1)

prbinname <- c(paste("prbin", seq(80, 260, by = 5), sep=""), "prbin999")
prbinname_order <- factor(c(prbinname), levels = c(prbinname))
prop_bin_long <- doc_morphvs %>% select(Barcode, LsWLGrp2, Path, CoTotal, prbin85:prbin999) %>%
  pivot_longer(cols = c(prbin85:prbin999), names_to = "prbins", values_to = "count") %>%
  mutate(prbins = factor(prbins, levels = prbinname_order))

cprbinbar <- ggplot(prop_bin_long)+
  geom_bar(aes(x = prbins, y = count, fill = LsWLGrp2), stat = "identity")+
  scale_y_sqrt()+ylab("sqrt(Isolated epithelial cells)")+xlab("Bins of DNA Index")+
  scale_fill_manual(values = c("#F7C1BB", "#2274A5"), labels = c("Negative\nScar", "Positive"), name = "Clinical")+
  theme(panel.background = element_rect(fill = NA, color = "gray"),
        axis.text.x = element_text(angle = 90, size = 8))

pprbinbar <- ggplot(prop_bin_long %>% filter(!is.na(Path)) %>% droplevels())+
  geom_bar(aes(x = prbins, y = count, fill = Path), stat = "identity")+
  scale_y_sqrt()+ylab("sqrt(Isolated epithelial cells)")+xlab("Bins of DNA Index")+
  scale_fill_manual(values = c("#73A580", "#B80C09"), labels = c("normal\nreactive\nLGL", "HGL/SCC"), name = "Pathology")+
  theme(panel.background = element_rect(fill = NA, color = "gray"),
        axis.text.x = element_text(angle = 90, size = 8))

ggarrange(cprbinbar+rremove("xlab"), pprbinbar, nrow = 2, ncol = 1)

names(doc_morphvs)

co_long <- doc_morphvs %>% select(Barcode, LsWLGrp2, Path, CoTotal, CoB_3:CoB_7, prCoB_3:prCoE_7, CoB_normal_p, CoB_abnormal_p) %>%
  pivot_longer(cols = c(CoB_3:CoB_abnormal_p), names_to = "cos", values_to = "count")

ccohisto <- ggplot(co_long %>% filter(cos %in% c("CoB_3", "prCoB_3", "CoB_5", "prCoB_5", "CoB_6", "prCoB_6", "CoB_7", "prCoB_7")) %>%
       mutate(cos = as.factor(cos)))+
  #geom_bar(aes(x = factor(cos, levels = c("CoB_3", "prCoB_3", "CoB_5", "prCoB_5", "CoB_6", "prCoB_6", "CoB_7", "prCoB_7")),
  geom_bar(aes(x = cos, y = count, fill = LsWLGrp2), stat = "identity")+
  scale_fill_manual(values = c("#F7C1BB", "#2274A5"), labels = c("Negative\nScar", "Positive"), name = "Clinical")+
  scale_y_sqrt()+
  #scale_x_discrete(labels = c("diploid", "diploid%", "cycling", "cycling%", "tetraploid", "tetraploid%", "aneuploid", "aneuploid%"))+
  scale_x_discrete(labels = c("diploid", "cycling", "tetraploid", "aneuploid", "diploid%", "cycling%", "tetraploid%", "aneuploid%"))+
  ylab("sqrt(Isolated epithelial cells)")+xlab("Ploidy Groups - by counts & percentage")+
  theme(panel.background = element_rect(fill = NA, color = "gray"),
        axis.text.x = element_text(angle = 0, size = 8))

pcohisto <- ggplot(co_long %>% filter(cos %in% c("CoB_3", "prCoB_3", "CoB_5", "prCoB_5", "CoB_6", "prCoB_6", "CoB_7", "prCoB_7"), !is.na(Path)) %>%
         mutate(cos = as.factor(cos)))+
  #geom_bar(aes(x = factor(cos, levels = c("CoB_3", "prCoB_3", "CoB_5", "prCoB_5", "CoB_6", "prCoB_6", "CoB_7", "prCoB_7")),
  geom_bar(aes(x = cos, y = count, fill = Path), stat = "identity")+
  scale_fill_manual(values = c("#73A580", "#B80C09"), labels = c("normal\nreactive\nLGL", "HGL/SCC"), name = "Pathology")+
  scale_y_sqrt()+
  #scale_x_discrete(labels = c("diploid", "diploid%", "cycling", "cycling%", "tetraploid", "tetraploid%", "aneuploid", "aneuploid%"))+
  scale_x_discrete(labels = c("diploid", "cycling", "tetraploid", "aneuploid", "diploid%", "cycling%", "tetraploid%", "aneuploid%"))+
  ylab("sqrt(Isolated epithelial cells)")+xlab("Ploidy Groups - by counts & percentage")+
  theme(panel.background = element_rect(fill = NA, color = "gray"),
        axis.text.x = element_text(angle = 0, size = 8))

ggarrange(ccohisto, pcohisto, nrow = 2, ncol = 1)
