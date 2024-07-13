human <- read_excel("www/ice_references/OECD Defined Approach to Skin Sensitization Human (R).xlsx")
human$`Original InChIKey` <- gsub("\\s", "", human$`Original InChIKey`)
human$`Human Binary hazard reference classification`[human$`Human Binary hazard reference classification` == "NC"] <- "0"
write.table(human, "www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization Human (R).txt", quote = F, col.names = T, row.names = F, sep = "\t")

llna <- read_excel("www/ice_references/OECD Defined Approach to Skin Sensitization LLNA (R).xlsx")
llna$`Original InChIKey` <- gsub("\\s", "", llna$`Original InChIKey`)
llna$`LLNA Binary hazard reference classification`[llna$`LLNA Binary hazard reference classification` == "NC"] <- "0"
write.table(llna, "www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization LLNA (R).txt", quote = F, col.names = T, row.names = F, sep = "\t")
