fetch_datasus2 = function (year_start, month_start, year_end, month_end, uf = "all", 
          information_system, vars = NULL, stop_on_error = FALSE) 
{
  sisSIH <- c("SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER")
  sisSIM <- c("SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", 
              "SIM-DOMAT")
  sisSINASC <- c("SINASC")
  sisCNES <- c("CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", 
               "CNES-SR", "CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", 
               "CNES-IN", "CNES-EE", "CNES-EF", "CNES-GM")
  sisSIA <- c("SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", 
              "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", "SIA-PS", 
              "SIA-SAD")
  sisSINAN <- c("SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA", 
                "SINAN-MALARIA")
  POBR <- c("POBR")
  SINPRENATAL <- ("SISPRENATAL")
  available_information_system <- c(sisSIH, sisSIM, sisSINASC, 
                                    sisCNES, sisSIA, sisSINAN, 
                                    POBR, SINPRENATAL)
  if (!(information_system %in% available_information_system)) 
    stop("Health informaton system unknown.")
  if (substr(information_system, 1, 3) == "SIH" 
    | substr(information_system, 1, 4) == "CNES" 
    | substr(information_system, 1, 3) == "SIA"
    | information_system == "SISPRENATAL") {
    date_start <- as.Date(paste0(year_start, "-", 
                                 formatC(month_start,
                                         width = 2, 
                                         format = "d", 
                                         flag = "0"), "-", "01"))
    date_end <- as.Date(paste0(year_end, "-", 
                               formatC(month_end,
                                       width = 2, 
                                       format = "d", 
                                       flag = "0"), "-", "01"))
  }
  else if (substr(information_system, 1, 3) == "SIM" 
        | information_system == "SINASC" 
        | information_system == "SINAN-DENGUE" 
        | information_system == "SINAN-CHIKUNGUNYA" 
        | information_system == "SINAN-ZIKA" 
        | information_system == "SINAN-MALARIA"
        | information_system == "POBR") {
    date_start <- as.Date(paste0(year_start, "-01-01"))
    date_end <- as.Date(paste0(year_end, "-01-01"))
  }
  if (date_start > date_end) 
    stop("Start date must be greather than end date.")
  if (substr(information_system, 1, 3) == "SIH" 
    | substr(information_system, 1, 4) == "CNES" 
    | substr(information_system, 1, 3) == "SIA"
    | information_system == "SISPRENATAL") {
    dates <- seq(date_start, date_end, by = "month")
    dates <- paste0(substr(lubridate::year(dates), 3, 4), 
                    formatC(lubridate::month(dates), width = 2, format = "d", 
                            flag = "0"))
  }
  else if (substr(information_system, 1, 3) == "SIM" 
           | information_system == "SINASC" 
           | information_system == "SINAN-DENGUE" 
           | information_system == "SINAN-CHIKUNGUNYA" 
           | information_system == "SINAN-ZIKA" 
           | information_system == "SINAN-MALARIA"
           | information_system == "POBR") {
    dates <- seq(date_start, date_end, by = "year")
    dates <- lubridate::year(dates)
  }
  ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", 
           "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", 
           "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", 
           "TO")
  if (!all((uf %in% c("all", ufs)))) 
    stop("UF unknown.")
  lista_uf <- vector()
  if (uf[1] == "all") {
    lista_uf <- ufs
  }
  else {
    lista_uf = uf
  }
  if (information_system %in% sisSINAN & uf[1] != "all") {
    message("SINAN files are not available per UF. Ignoring argument 'uf' and downloading data.")
  }
  if (information_system %in% POBR & uf[1] != "all") {
    message("POBR files are not available per UF. Ignoring argument 'uf' and downloading data.")
  }
  if (information_system == "SIM-DO") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DORES/"
    avail_geral <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                                                       ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                                 start = 5, stop = 8))
    avail_prelim <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                                                        ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                                  start = 5, stop = 8))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "DO", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_geral], 
                                               ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "DO", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_prelim], 
                                                ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIM-DOFET") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOFET", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOFET", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                         0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                            tmp))))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (uf != "Any") {
      message(paste0("DOFET data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "DOFET", substr(valid_dates[valid_dates %in% 
                                                      avail_geral], 3, 4), ".dbc")
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "DOFET", substr(valid_dates[valid_dates %in% 
                                                       avail_prelim], 3, 4), ".dbc")
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIM-DOEXT") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOEXT", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOEXT", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                         0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                            tmp))))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (uf != "Any") {
      message(paste0("DOEXT data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "DOEXT", substr(valid_dates[valid_dates %in% 
                                                      avail_geral], 3, 4), ".dbc")
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "DOEXT", substr(valid_dates[valid_dates %in% 
                                                       avail_prelim], 3, 4), ".dbc")
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIM-DOINF") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOINF", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOINF", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                         0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                            tmp))))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (uf != "Any") {
      message(paste0("DOINF data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "DOINF", substr(valid_dates[valid_dates %in% 
                                                      avail_geral], 3, 4), ".dbc")
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "DOINF", substr(valid_dates[valid_dates %in% 
                                                       avail_prelim], 3, 4), ".dbc")
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIM-DOMAT") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOMAT", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOMAT", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                         0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                            tmp))))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (uf != "Any") {
      message(paste0("DOMAT data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "DOMAT", substr(valid_dates[valid_dates %in% 
                                                      avail_geral], 3, 4), ".dbc")
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "DOMAT", substr(valid_dates[valid_dates %in% 
                                                       avail_prelim], 3, 4), ".dbc")
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIH-RD") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("RD", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("RD", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "RD", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "RD", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIH-RJ") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("RJ", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("RJ", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "RJ", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "RJ", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIH-SP") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("SP", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("SP", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "SP", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "SP", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIH-ER") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ER", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ER", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "ER", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "ER", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SINASC") {
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1994_1995/Dados/DNRES/"
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/DNRES/"
    avail_antigo <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                                                        ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                                  start = 6, stop = 9))
    avail_atual <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                                                       ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                                 start = 5, stop = 8))
    avail_prelim <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                                                        ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                                  start = 5, stop = 8))
    if (!all(dates %in% c(avail_antigo, avail_atual, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_antigo, avail_atual, 
                                                avail_prelim)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_antigo, avail_atual, 
                                      avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates are from old folders and and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "DNR", as.vector(sapply(lista_uf, 
                                                 paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                 ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "DN", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list_3 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "DN", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_prelim], 
                                                ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2, files_list_3)
  }
  else if (information_system == "CNES-LT") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "LT", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-ST") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "ST", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-DC") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/DC/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "DC", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-EQ") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "EQ", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-SR") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/SR/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "SR", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-HB") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "HB", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-PF") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "PF", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-EP") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "EP", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-RC") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/RC/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "RC", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-IN") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/IN/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "IN", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-EE") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EE/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "EE", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-EF") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EF/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "EF", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "CNES-GM") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/GM/"
    avail <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = url, 
                                                                 ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), 
                           start = 5, stop = 8))
    if (!all(dates %in% avail)) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% avail], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail]
    files_list <- paste0(url, "GM", as.vector(sapply(lista_uf, 
                                                     paste0, valid_dates[valid_dates %in% avail], ".dbc")))
  }
  else if (information_system == "SIA-AB") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AB", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AB", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "AB", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "AB", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-ABO") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ABO", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ABO", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "ABO", as.vector(sapply(lista_uf, 
                                                 paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                 ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "ABO", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_atual], 
                                                ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-ACF") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ACF", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ACF", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "ACF", as.vector(sapply(lista_uf, 
                                                 paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                 ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "ACF", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_atual], 
                                                ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-AD") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AD", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AD", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "AD", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "AD", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-AN") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AN", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AN", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "AN", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "AN", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-AM") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AM", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AM", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "AM", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "AM", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-AQ") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AQ", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AQ", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "AQ", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "AQ", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-AR") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AR", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("AR", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "AR", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "AR", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-ATD") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ATD", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ATD", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "ATD", as.vector(sapply(lista_uf, 
                                                 paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                 ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "ATD", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_atual], 
                                                ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-PA") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("^PA", tmp)]
    tmp <- tmp[substr(x = tmp, start = 3, stop = 4) %in% 
                 lista_uf]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 9))
    avail_atual <- gsub(pattern = "\\.", replacement = "", 
                        x = avail_atual)
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("^PA", tmp)]
    tmp <- tmp[substr(x = tmp, start = 3, stop = 4) %in% 
                 lista_uf]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 9))
    avail_antigo <- gsub(pattern = "\\.", replacement = "", 
                         x = avail_antigo)
    if (!all(dates %in% c(substr(x = avail_atual, start = 0, 
                                 stop = 4), substr(x = avail_antigo, start = 0, stop = 4)))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(substr(x = avail_atual, 
                                                       start = 0, stop = 4), substr(x = avail_antigo, 
                                                                                    start = 0, stop = 4))], collapse = ", "), 
                     ". Only the available dates will be downloaded."))
    }
    valid_dates <- c(avail_atual, avail_antigo)[substr(x = c(avail_atual, 
                                                             avail_antigo), start = 0, stop = 4) %in% dates]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "PA", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "PA", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-PS") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("^PS", tmp)]
    tmp <- tmp[substr(x = tmp, start = 3, stop = 4) %in% 
                 lista_uf]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 9))
    avail_atual <- gsub(pattern = "\\.", replacement = "", 
                        x = avail_atual)
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("^PS", tmp)]
    tmp <- tmp[substr(x = tmp, start = 3, stop = 4) %in% 
                 lista_uf]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 9))
    avail_antigo <- gsub(pattern = "\\.", replacement = "", 
                         x = avail_antigo)
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "PS", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "PS", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SIA-SAD") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("^SAD", tmp)]
    tmp <- tmp[substr(x = tmp, start = 3, stop = 4) %in% 
                 lista_uf]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 9))
    avail_atual <- gsub(pattern = "\\.", replacement = "", 
                        x = avail_atual)
    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("^SAD", tmp)]
    tmp <- tmp[substr(x = tmp, start = 3, stop = 4) %in% 
                 lista_uf]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 9))
    avail_antigo <- gsub(pattern = "\\.", replacement = "", 
                         x = avail_antigo)
    if (!all(dates %in% c(avail_atual, avail_antigo))) {
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", 
                     paste0(dates[!dates %in% c(avail_atual, avail_antigo)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]
    if (any(valid_dates %in% avail_antigo)) {
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", 
                     paste0(valid_dates[valid_dates %in% avail_antigo], 
                            collapse = ", "), "."))
    }
    files_list_1 <- if (any(valid_dates %in% avail_antigo)) {
      paste0(antigo_url, "SAD", as.vector(sapply(lista_uf, 
                                                 paste0, valid_dates[valid_dates %in% avail_antigo], 
                                                 ".dbc")))
    }
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "SAD", as.vector(sapply(lista_uf, 
                                                paste0, valid_dates[valid_dates %in% avail_atual], 
                                                ".dbc")))
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SINAN-DENGUE") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DENGBR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DENGBR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                         0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                            tmp))))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (uf != "Any") {
      message(paste0("DENGUE data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "DENGBR", substr(valid_dates[valid_dates %in% 
                                                       avail_geral], 3, 4), ".dbc")
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "DENGBR", substr(valid_dates[valid_dates %in% 
                                                        avail_prelim], 3, 4), ".dbc")
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SINAN-CHIKUNGUNYA") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("CHIKBR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("CHIKBR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                         0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                            tmp))))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (uf != "Any") {
      message(paste0("CHIKUNGUNYA data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "CHIKBR", substr(valid_dates[valid_dates %in% 
                                                       avail_geral], 3, 4), ".dbc")
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "CHIKBR", substr(valid_dates[valid_dates %in% 
                                                        avail_prelim], 3, 4), ".dbc")
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SINAN-ZIKA") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ZIKABR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ZIKABR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                         0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                            tmp))))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (uf != "Any") {
      message(paste0("ZIKA data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "ZIKABR", substr(valid_dates[valid_dates %in% 
                                                       avail_geral], 3, 4), ".dbc")
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "ZIKABR", substr(valid_dates[valid_dates %in% 
                                                        avail_prelim], 3, 4), ".dbc")
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "SINAN-MALARIA") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("MALABR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("MALABR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                         0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                            tmp))))
    if (!all(dates %in% c(avail_geral, avail_prelim))) {
      message(paste0("The following dates are not availabe at DataSUS: ", 
                     paste0(dates[!dates %in% c(avail_geral, avail_prelim)], 
                            collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]
    if (any(valid_dates %in% avail_prelim)) {
      message(paste0("The following dates are preliminar: ", 
                     paste0(valid_dates[valid_dates %in% avail_prelim], 
                            collapse = ", "), "."))
    }
    if (uf != "Any") {
      message(paste0("MALARIA data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "MALABR", substr(valid_dates[valid_dates %in% 
                                                       avail_geral], 3, 4), ".dbc")
    }
    files_list_2 <- if (any(valid_dates %in% avail_prelim)) {
      paste0(prelim_url, "MALABR", substr(valid_dates[valid_dates %in% 
                                                        avail_prelim], 3, 4), ".dbc")
    }
    files_list <- c(files_list_1, files_list_2)
  }
  else if (information_system == "POBR") {
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/painel_oncologia/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("POBR", tmp)]
    tmp <- unique(substr(x = tmp, start = 7, stop = 8))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 
                                                        0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", 
                                                                                                           tmp))))
    if (!all(dates %in% c(avail_geral))) {
    message(paste0("The following dates are not availabe at DataSUS: ", 
                 paste0(dates[!dates %in% c(avail_geral)], 
                        collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral)]
    if (uf != "Any") {
      message(paste0("POBR data is not available by UF. Downloading all data available instead. "))
    }
    files_list_1 <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, "POBR", valid_dates[valid_dates %in% avail_geral], ".dbc")
    }
    files_list <- c(files_list_1)
  }
  else if (information_system == "SISPRENATAL") {
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISPRENATAL/201201_/Dados/"
    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, 
                                             ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))
    valid_dates <- dates[dates %in% avail_atual]
    files_list_2 <- if (any(valid_dates %in% avail_atual)) {
      paste0(atual_url, "PN", as.vector(sapply(lista_uf, 
                                               paste0, valid_dates[valid_dates %in% avail_atual], 
                                               ".dbc")))
    }
    files_list <- files_list_2
  }
  local_internet <- curl::has_internet()
  if (local_internet == TRUE) {
    message("Your local Internet connection seems to be ok.")
  }
  else {
    stop("It appears that your local Internet connection is not working. Can you check?")
  }
  remote_file_is_availabe <- RCurl::url.exists("ftp.datasus.gov.br")
  if (remote_file_is_availabe == TRUE) {
    message("DataSUS FTP server seems to be up. Starting download...")
  }
  else {
    message("It appears that DataSUS FTP is down. I will try to download the files anyway...")
  }
  data <- NULL
  for (file in files_list) {
    temp <- tempfile()
    partial <- data.frame()
    tryCatch({
      utils::download.file(file, temp, mode = "wb", method = "libcurl")
      partial <- read.dbc::read.dbc(temp, as.is = TRUE)
      file.remove(temp)
    }, error = function(cond) {
      message(paste("Something went wrong with this URL:", 
                    file))
      message("This can be a problem with the Internet or the file does not exist yet.")
      if (stop_on_error == TRUE) {
        stop("Stopping download.")
      }
    })
    if (nrow(partial) > 0) {
      if (!all(vars %in% names(partial))) 
        stop("One or more variables names are unknown. Typo?")
      if (is.null(vars)) {
        data <- dplyr::bind_rows(data, partial)
      }
      else {
        data <- dplyr::bind_rows(data, subset(partial, 
                                              select = vars))
      }
    }
  }
  return(data)
}
