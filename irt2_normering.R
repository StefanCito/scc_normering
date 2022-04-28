# Dit script wordt gebruikt om de IRT2-normering uit te voeren voor een of meerdere toetsaanbieders

# De output van dit script bestaat per toetsaanbieder uit:
# - Behaalde referentieniveaus
#   Overzichtstabel met percentages behaalde referentieniveaus, streefpercentages en toegepaste cesuren
#   bestandsnaam TOETS_referentieniveaus_irt2.csv
# - Behaalde toetsadviezen
#   Overzichtstabel met percentages behaalde toetsadviezen, streefpercentages en toegepaste cesuren
#   bestandsnaam TOETS_toetsadviezen_irt2.csv
# - Verschillen IRT1/IRT2-toetsadviezen
#   Verschillen tussen de IRT1- en IRT2-toetsadviezen, voor de beslisprocedure
#   bestandsnaam TOETS_toetsadvies_verschillen.csv

# Daarnaast worden er drie bestanden weggeschreven met overzichtstabellen met alle aanbieders met behaalde referentieniveaus,
# behaalde toetsadviezen en de verschillen tussen de IRT1- en IRT2-normering van de toetsadviezne. De bestandsnamen zijn
# overzicht_referentieniveaus_irt2.csv, overzicht_toetsadviezen_irt2.csv en overzicht_toetsadvies_verschillen.csv

# De packages plyr en dplyr zijn noodzakelijk om dit script te kunnen draaien

# Invoer ==================================================

# Folder met alle databestanden
# Per toetsaanbieder wordt een bestand met leerlingdata (bestandsnaam TOETS_leerlingen.csv) verwacht
data_folder = 'dummy_data'

# Bestand met PCET-resultaten, wordt gemaakt met behulp van controle.R indien de PCET wordt ingelezen
pcet_file = 'pcet_irt2.csv'

# Folder met resultaten uit IRT1-normering
# De volgende resultaatbestanden worden verwacht per toetsaanbieder:
# - Leerlingvaardigheden IRT1 (TOETS_vaardigheden.csv)
# - Toetsadviezen IRT1 (TOETS_toetsadviezen_irt1.csv)
# Deze folder wordt ook gebruikt voor de output van de IRT2-normering weg te schrijven
resultaten_folder = 'dummy_resultaten'

# Sampling factor voor IRT2-normering
sampling = 10000

# Code ====================================================

options(warn = 1)

# Mapping voor toetsadviezen
toetsadvies_mapping = list('1' = 'pro/vmbo bb', '2' = 'vmbo bb/kb', '3' = 'vmbo kb/gl-tl', '4' = 'vmbo gl-tl/havo', '5' = 'havo/vwo', '6' = 'vwo')

# Referentieniveaus
refniveaus = c('LEZEN1F', 'LEZEN2F', 'REKENEN1F', 'REKENEN1S', 'TAAL1F', 'TAAL2F')

# Inventariseer alle bestanden
leerling_files = list.files(path = data_folder, pattern = '_leerlingen.csv')
ability_files = list.files(path = resultaten_folder, pattern = '_vaardigheden.csv')
toetsadvies_files = list.files(path = resultaten_folder, pattern = '_toetsadviezen_irt1.csv')

# Lees bestand met pcet-data in
pcet_data = read.csv2(pcet_file)

# Gemiddelde en SD standaardscore PCET voor z-scores
mean_standaardscore_pcet = mean(pcet_data$standaardscore)
sd_standaardscore_pcet = sd(pcet_data$standaardscore)

summary_refs = NULL
summary_toetsadviezen = NULL
summary_verschillen = NULL
for (leerling_file in leerling_files) {

  aanbieder = gsub('_leerlingen.csv', '', leerling_file, fixed = TRUE)

  message(paste0('Bezig met aanbieder ', aanbieder))

  # Lees leerlingdata in (voor schooladviezen)
  leerlingen = read.csv2(file.path(data_folder, leerling_file))
  leerlingen_irt2 = leerlingen[leerlingen[, 'schooladvies'] %in% 1:10 & leerlingen[, 'schooltype'] == 1, ] # Alleen met schooladviezen en BO

  referentieniveaus = NULL
  toetsadviezen = NULL
  standaardscores_schooladvies = NULL
  # Loop over alle schooladviezen en voer trekking uit in PCET-data
  for (schooladvies in unique(leerlingen_irt2$schooladvies)) {
    n_leerlingen = nrow(leerlingen_irt2[leerlingen_irt2[, 'schooladvies'] == schooladvies, ])

    # Referentieniveaus
    for (refniveau in refniveaus) {
      behaald = sum(sample(pcet_data[pcet_data[, 'schooladvies'] == schooladvies, refniveau], size = n_leerlingen * sampling, replace = TRUE))
      referentieniveaus = rbind(referentieniveaus, data.frame('refniveau' = refniveau, 'n' = n_leerlingen * sampling, 'behaald' = behaald))
    }

    # Toetsadviezen
    toetsadviezen_trekking = sample(pcet_data[pcet_data[, 'schooladvies'] == schooladvies, 'toetsadvies'], size = n_leerlingen * sampling, replace = TRUE)
    toetsadviezen_freq = as.data.frame(table(toetsadviezen_trekking), stringsAsFactors = FALSE)
    colnames(toetsadviezen_freq) = c('toetsadvies', 'n')
    toetsadviezen = rbind(toetsadviezen, toetsadviezen_freq)

    # Standaardscores
    standaardscores_schooladvies = c(standaardscores_schooladvies, sample(pcet_data[pcet_data[, 'schooladvies'] == schooladvies, 'standaardscore'], size = n_leerlingen * sampling, replace = TRUE))
  }

  # Voeg trekkingen referentieniveaus samen en bepaal streefpercentages
  referentieniveaus = plyr::ddply(referentieniveaus, 'refniveau', function(x) {
    return(data.frame('streefpercentage' = sum(x$behaald) / sum(x$n) * 100))
  })

  # Voeg trekkingen toetsadviezen samen en bepaal streefpercentages
  toetsadviezen = plyr::ddply(toetsadviezen, 'toetsadvies', function(x) {
    return(data.frame('streefpercentage' = sum(x$n) / sum(toetsadviezen$n) * 100.0))
  })
  toetsadviezen$toetsadvies = unlist(toetsadvies_mapping[toetsadviezen$toetsadvies], use.names = FALSE)
  toetsadviezen$cum_perc = cumsum(toetsadviezen$streefpercentage) # cumulatieve percentages voor cesuurpuntenbepaling

  # Lees vaardigheden IRT1-normering in
  abilities = read.csv2(file.path(resultaten_folder, ability_files[grepl(aanbieder, ability_files)]))

  # Lees toetsadviezen IRT1-normering in (voor cesuur pro/bb-correctie)
  toetsadviezen_irt1 = read.csv2(file.path(resultaten_folder, toetsadvies_files[grepl(aanbieder, toetsadvies_files)]))

  ref_cesuren = NULL
  toetsadvies_cesuren = NULL
  # Loop over de modellen om cesuurbepaling uit te voeren
  for (model in c('1pl', '2pl')) {
    # Cesuurbepaling voor ieder referentieniveau
    for (refniveau in refniveaus) {
      onderdeel = gsub('1F|2F|1S', '', refniveau)
      streefpercentage = referentieniveaus[referentieniveaus[, 'refniveau'] == refniveau, 'streefpercentage']
      ref_abilities = sort(abilities[abilities[, 'schooltype'] == 1 & !is.na(abilities[, paste0('theta_', model, '_', onderdeel)]), paste0('theta_', model, '_', onderdeel)]) # Alleen BO en neem alleen leerlingen mee met een bekende theta-waarde
      cesuur = ref_abilities[ceiling(length(ref_abilities) * 0.01 * (100 - streefpercentage))]
      gerealiseerd_percentage = length(ref_abilities[ref_abilities >= cesuur]) / length(ref_abilities) * 100.0

      ref_cesuren = rbind(ref_cesuren, data.frame('onderdeel' = onderdeel, 'niveau' = gsub(onderdeel, '', refniveau), 'refniveau' = refniveau, 'model' = model, 'cesuur' = cesuur, 'perc_behaald' = gerealiseerd_percentage))
    }

    # Cesuurbepaling voor toetsadviezen
    model_glv = sort(abilities[abilities[, 'schooltype'] == 1 & !is.na(abilities[, paste0('glv_', model)]), paste0('glv_', model)]) # Alleen BO en leerlingen met een GLV die bepaald kon worden
    for (i in 1:(nrow(toetsadviezen) - 1)) {
      target_row = ceiling(toetsadviezen[i, 'cum_perc'] * 0.01 * length(model_glv))
      cesuur = model_glv[target_row]

      # Pas pro/bb-correctie toe indien nodig (niet-CET)
      if (!grepl('CET', aanbieder) && toetsadviezen[i, 'toetsadvies'] == 'pro/vmbo bb') {
        cesuur_pro = toetsadviezen_irt1[toetsadviezen_irt1[, 'toetsadvies'] == 'pro/vmbo bb', paste0('cesuur_', model)]
        message(paste0('IRT2-cesuur ', cesuur, ' voor pro/vmbo bb vervangen door cesuur pro/bb-correctie (', round(cesuur_pro, 5), ')'))
        cesuur = cesuur_pro
      }

      gerealiseerd_percentage = length(model_glv[model_glv < cesuur]) / length(model_glv) * 100

      toetsadvies_cesuren = rbind(toetsadvies_cesuren, data.frame('toetsadvies' = toetsadviezen[i, 'toetsadvies'], 'model' = model, 'cesuur' = cesuur, 'cum_perc_behaald' = gerealiseerd_percentage))
    }
    toetsadvies_cesuren = rbind(toetsadvies_cesuren, data.frame('toetsadvies' = tail(toetsadviezen$toetsadvies, 1), 'model' = model, 'cesuur' = NA, 'cum_perc_behaald' = 100.0)) # Ook nog even hoogste adviescategorie toevoegen (geen cesuur)
  }

  # Tabel met referentieniveaus mooi maken en streefpercentages toevoegen
  ref_cesuren = reshape(ref_cesuren, idvar = c('onderdeel', 'niveau', 'refniveau'), timevar = 'model', direction = 'wide', sep = '_')
  ref_cesuren = dplyr::left_join(ref_cesuren, referentieniveaus, by = 'refniveau')
  ref_cesuren = ref_cesuren[, c('onderdeel', 'niveau', 'streefpercentage', colnames(ref_cesuren)[!colnames(ref_cesuren) %in% c('onderdeel', 'niveau', 'streefpercentage', 'refniveau')])]

  # Tabel met toetsadviezen mooi maken en streefpercentages toevoegen
  toetsadvies_cesuren = reshape(toetsadvies_cesuren, idvar = c('toetsadvies'), timevar = 'model', direction = 'wide', sep = '_')
  for (column in colnames(toetsadvies_cesuren)[grepl('cum_perc_behaald', colnames(toetsadvies_cesuren))]) {
    toetsadvies_cesuren[, gsub('cum_', '', column)] = c(toetsadvies_cesuren[1, column], diff(toetsadvies_cesuren[, column])) # Ga nog even van cumulatieve naar normale percentages
    toetsadvies_cesuren = toetsadvies_cesuren[, colnames(toetsadvies_cesuren) != column]
  }
  toetsadvies_cesuren = dplyr::left_join(toetsadviezen[, colnames(toetsadviezen) != 'cum_perc'], toetsadvies_cesuren, by = 'toetsadvies')

  # Rond percentages af
  for (column in colnames(ref_cesuren)[grepl('streefpercentage|perc_behaald', colnames(ref_cesuren))]) {
    ref_cesuren[, column] = round(ref_cesuren[, column], 2)
  }
  for (column in colnames(toetsadvies_cesuren)[grepl('streefpercentage|perc_behaald', colnames(toetsadvies_cesuren))]) {
    toetsadvies_cesuren[, column] = round(toetsadvies_cesuren[, column], 2)
  }

  # Bepaal z-scores schooladviezen
  z_schooladvies = (standaardscores_schooladvies - mean_standaardscore_pcet) / sd_standaardscore_pcet
  rm(standaardscores_schooladvies)
  mean_z_schooladvies = mean(z_schooladvies)
  sd_z_schooladvies = sd(z_schooladvies)
  rm(z_schooladvies)

  # Loop over alle toetsadviezen en voer trekking uit in PCET-data voor bepaling z-scores
  toetsadviezen_irt2 = dplyr::semi_join(abilities[, c('person_id', 'toetsadvies_1pl', 'toetsadvies_2pl')], leerlingen_irt2, by = 'person_id')
  gemiddelde_verschillen = NULL
  for (model in c('1pl', '2pl')) {
    standaardscores_toetsadvies = NULL
    for (toetsadvies in unique(toetsadviezen_irt2[, paste0('toetsadvies_', model)])) {
      if (toetsadvies == '') {
        next
      }
      n_leerlingen = nrow(toetsadviezen_irt2[toetsadviezen_irt2[, paste0('toetsadvies_', model)] == toetsadvies, ])
      standaardscores_toetsadvies = c(standaardscores_toetsadvies, sample(pcet_data[pcet_data[, 'toetsadvies'] == names(toetsadvies_mapping)[toetsadvies_mapping == toetsadvies], 'standaardscore'], size = n_leerlingen * sampling, replace = TRUE))
    }

    # Bepaal z-scores toetsadviezen
    z_toetsadvies = (standaardscores_toetsadvies - mean_standaardscore_pcet) / sd_standaardscore_pcet
    rm(standaardscores_toetsadvies)
    mean_z_toetsadvies = mean(z_toetsadvies)
    sd_z_toetsadvies = sd(z_toetsadvies)
    rm(z_toetsadvies)

    # Verschil in gemiddelde en standaarddeviatie
    verschil_mean = (mean_z_toetsadvies - mean_z_schooladvies) / sd_z_schooladvies
    verschil_sd = sd_z_schooladvies / sd_z_toetsadvies
    gemiddelde_verschillen = rbind(gemiddelde_verschillen, data.frame('aanbieder' = aanbieder, 'model' = model, 'verschil_gemiddelde' = round(verschil_mean, 3), 'verschil_sd' = round(verschil_sd, 3)))
  }

  gemiddelde_verschillen = reshape(gemiddelde_verschillen, idvar = 'aanbieder', timevar = 'model', direction = 'wide', sep = '_')

  # Schrijf tabellen weg
  write.csv2(ref_cesuren, paste0(resultaten_folder, '/', aanbieder, '_referentieniveaus_irt2.csv'), row.names = FALSE, quote = FALSE)
  write.csv2(toetsadvies_cesuren, paste0(resultaten_folder, '/', aanbieder, '_toetsadviezen_irt2.csv'), row.names = FALSE, quote = FALSE, na = '')
  write.csv2(gemiddelde_verschillen[, colnames(gemiddelde_verschillen) != 'aanbieder'], paste0(resultaten_folder, '/', aanbieder, '_toetsadvies_verschillen.csv'), row.names = FALSE, quote = FALSE, na = '')

  # Schrijf gegevens naar samenvattingstabellen met resultaten van alle aanbieders
  ref_cesuren$aanbieder = aanbieder
  toetsadvies_cesuren$aanbieder = aanbieder
  summary_refs = rbind(summary_refs, ref_cesuren[, c('aanbieder', 'onderdeel', 'niveau', 'streefpercentage')])
  summary_toetsadviezen = rbind(summary_toetsadviezen, toetsadvies_cesuren[, c('aanbieder', 'toetsadvies', 'streefpercentage')])
  summary_verschillen = rbind(summary_verschillen, gemiddelde_verschillen)
}

# Schrijf samenvattingstabellen weg
summary_refs = reshape(summary_refs, idvar = c('onderdeel', 'niveau'), timevar = 'aanbieder', direction = 'wide', sep = '_')
summary_toetsadviezen = reshape(summary_toetsadviezen, idvar = c('toetsadvies'), timevar = 'aanbieder', direction = 'wide', sep = '_')
write.csv2(summary_refs, file.path(resultaten_folder, 'overzicht_referentieniveaus_irt2.csv'), row.names = FALSE, quote = FALSE)
write.csv2(summary_toetsadviezen, file.path(resultaten_folder, 'overzicht_toetsadviezen_irt2.csv'), row.names = FALSE, quote = FALSE)
write.csv2(summary_verschillen, file.path(resultaten_folder, 'overzicht_toetsadvies_verschillen.csv'), row.names = FALSE, quote = FALSE)
