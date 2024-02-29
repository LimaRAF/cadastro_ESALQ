#### SORTEANDO 2 INDIVÍDUOS POR EXPÉCIES PARA COLETA ####
path <- "cadastro_arvores_ESALQ.xlsx"
cadastro <- readxl::read_excel(path, .name_repair = "unique")
dim(cadastro)
length(unique(cadastro$suggestedName))

#Sorteando 5 árvores por espécie
# do.call(rbind, 
#         lapply(split(cadastro, cadastro$suggestedName), 
#                function(x) x[sample(nrow(x), 5, replace = TRUE), ]))
tmp <- do.call(rbind, 
        lapply(split(cadastro, cadastro$suggestedName), 
               function(x) sample(x$NumeroPlaca, 2, replace = FALSE)))
tmp1 <- rbind.data.frame(cbind(row.names(tmp), tmp[,1]), 
              cbind(row.names(tmp), tmp[,2]))
names(tmp1) <- c("suggestedName", "PlacaSorteada")
cadastro$PlacaSorteada <- cadastro$NumeroPlaca %in% tmp1$PlacaSorteada
tmp2 <- cadastro[ , c("NumeroPlaca", "PlacaSorteada")]
write.csv(tmp2, "sorteio_arvores_cadastro.csv")
