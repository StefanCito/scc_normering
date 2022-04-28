# Dit script wordt gebruikt om de leerachterstanden en eventuele coronacorrectie voor de IRT2-normering uit te rekenen

# De output van dit script bestaat per toetsaanbieder uit:
# - Behaalde toetsadviezen
#   Overzichtstabel met percentages behaalde toetsadviezen aldus de coronacorrectie
#   bestandsnaam TOETS_toetsadviezen_irt2_coronacorrectie.csv

# Daarnaast worden twee bestanden weggeschreven:
# Overzicht landelijk behaalde en (indien nodig) gecorrigeerde toetsadviezen: coronacorrectie_irt2.csv
# Gecorrigeerde cesuren toetsadviezen CET: cesuren_coronacorrectie_irt2.csv (alleen als correctie nodig is)

# De packages openxlsx, plyr en dplyr zijn noodzakelijk om dit script te kunnen draaien

# Invoer ==================================================

# Folder met alle databestanden
# Per toetsaanbieder wordt een bestand met leerlingdata (bestandsnaam TOETS_leerlingen.csv) verwacht
data_folder = 'dummy_data'

# Bestand met PCET-resultaten, wordt gemaakt met behulp van controle.R indien de PCET wordt ingelezen
pcet_file = 'pcet_irt2.csv'

# Bestand met normeringsgegevens, bevat tabbladen toetsadvies_cesuren, coronanormering en modellen
normeringsgegevens_file = 'normeringsgegevens_dummy.xlsx'

# Folder met resultaten uit IRT1-normering
# De volgende resultaatbestanden worden verwacht per toetsaanbieder:
# - Leerlingvaardigheden IRT1 (TOETS_vaardigheden.csv)
# - Toetsadviezen IRT1 (TOETS_toetsadviezen_irt1.csv)
# - Toetsadviezen IRT2 (TOETS_toetsadviezen_irt2.csv)
# Deze folder wordt ook gebruikt voor de output van de coronacorrectie weg te schrijven
resultaten_folder = 'dummy_resultaten'

# Sampling factor voor IRT2-normering
sampling = 10000

# Code ====================================================

options(warn = 1)

# Lees normeringsgegevens in
toetsadvies_cesuren = openxlsx::read.xlsx(normeringsgegevens_file, 'toetsadvies_cesuren')
streefnormering = openxlsx::read.xlsx(normeringsgegevens_file, 'coronanormering')
voorkeursmodellen = openxlsx::read.xlsx(normeringsgegevens_file, 'modellen')

streefnormering$toetsadvies = sapply(streefnormering$grenspunt, function(x) strsplit(x, split = ' -> ')[[1]][1])

# Deduceer lijst van toetsadviezen op volgorde uit toetsadvies-cesuren
toetsadviezen = unique(unlist(strsplit(toetsadvies_cesuren[order(toetsadvies_cesuren$cesuur_1pl), 'grenspunt'], split = ' -> ')))

# Inventariseer alle bestanden
ability_files = list.files(path = resultaten_folder, pattern = '_vaardigheden.csv')
irt2_files = list.files(path = resultaten_folder, pattern = '_toetsadviezen_irt2.csv')
leerling_files = list.files(path = data_folder, pattern = '_leerlingen.csv')
toetsadvies_files = list.files(path = resultaten_folder, pattern = '_toetsadviezen_irt1.csv')

# PCET data
pcet_data = read.csv2(pcet_file)

# Lees alle vaardigheden uit de IRT1-normering in
all_abilities = list()
aanbieders = NULL
for (ability_file in ability_files) {

  aanbieder = gsub('_vaardigheden.csv', '', ability_file, fixed = TRUE)
  aanbieders = c(aanbieders, aanbieder)

  aanbieder_abilities = read.csv2(file.path(resultaten_folder, ability_file))
  aanbieder_abilities = aanbieder_abilities[aanbieder_abilities[, 'schooltype'] == 1, c('glv_1pl', 'glv_2pl')] # Alleen regulier BO
  aanbieder_abilities$aanbieder = aanbieder
  all_abilities[[aanbieder]] = aanbieder_abilities
}
all_abilities = dplyr::bind_rows(all_abilities)

# Check of we alle aanbieders hebben om landelijk beeld te krijgen (verifieer nhv voorkeursmodellen)
if (!setequal(aanbieders, voorkeursmodellen$aanbieder)) {
  stop(paste0('Er ontbreken aanbieders in de beschikbare data, hierdoor kan geen compleet landelijk beeld geconstrueerd worden. Het gaat om:\n', paste(aanbieders[!aanbieders %in% voorkeursmodellen$aanbieder], collapse = '\n')))
}

# Functie om streefpercentages voor irt2-normering voor toetsadviezen toe te passen, input:
# schooladviezen: dataframe met kolommen aanbieder en schooladvies
# pcet_data: dataframe met kolommen schooladvies en toetsadvies
# sampling: samplingfactor voor irt2-normering
irt2_normering = function(schooladviezen, pcet_data, sampling) {

  irt2_toetsadviezen = NULL
  # Loop over alle aanbieders
  for (aanbieder in unique(schooladviezen$aanbieder)) {
    aanbieder_schooladviezen = schooladviezen[schooladviezen[, 'aanbieder'] == aanbieder, ]

    aanbieder_toetsadviezen = NULL
    # Loop over alle schooladviezen om trekking in pcet uit te voeren
    for (schooladvies in unique(aanbieder_schooladviezen$schooladvies)) {
      if (!schooladvies %in% 1:10) {
        next
      }
      n_leerlingen = nrow(aanbieder_schooladviezen[aanbieder_schooladviezen[, 'schooladvies'] == schooladvies, ])

      toetsadviezen_trekking = sample(pcet_data[pcet_data[, 'schooladvies'] == schooladvies, 'toetsadvies'], size = n_leerlingen * sampling, replace = TRUE)
      toetsadviezen_freq = as.data.frame(table(toetsadviezen_trekking), stringsAsFactors = FALSE)
      colnames(toetsadviezen_freq) = c('toetsadvies', 'n')
      aanbieder_toetsadviezen = rbind(aanbieder_toetsadviezen, toetsadviezen_freq)
    }

    aanbieder_toetsadviezen = plyr::ddply(aanbieder_toetsadviezen, 'toetsadvies', function(x) {
      return(data.frame('streefpercentage' = sum(x$n) / sum(aanbieder_toetsadviezen$n) * 100.0))
    })

    aanbieder_toetsadviezen$streefaantal = aanbieder_toetsadviezen$streefpercentage * 0.01 * nrow(aanbieder_schooladviezen)
    aanbieder_toetsadviezen$aanbieder = aanbieder
    irt2_toetsadviezen = rbind(irt2_toetsadviezen, aanbieder_toetsadviezen)
  }

  irt2_toetsadviezen = reshape(irt2_toetsadviezen, idvar = 'toetsadvies', timevar = 'aanbieder', direction = 'wide', sep = '_')
  
  return(irt2_toetsadviezen)
}

# Loop over alle aanbieders en bereken toetsadviezen
# Voor de CET bepalen we de percentages behaalde percentages via de standaardscores, voor de private aanbieders uit de streefpercentages van de IRT2-normering
all_schooladviezen = list() # Object voor schooladviezen private aanbieders
all_toetsadviezen_irt2 = NULL
cet_standaardscores = NULL # Vector om standaardscores CET bij te houden
for (aanbieder in aanbieders) {

  aanbieder_leerlingen = read.csv2(file.path(data_folder, leerling_files[grepl(aanbieder, leerling_files)]))

  if (grepl('CET', aanbieder)) {
    standaardscores = aanbieder_leerlingen[aanbieder_leerlingen[, 'schooltype'] == 1, 'standaardscore']
    cet_standaardscores = c(cet_standaardscores, standaardscores)
    cet_toetsadviezen = as.character(cut(standaardscores, c(-Inf, sort(toetsadvies_cesuren$cesuur_standaardscore), Inf), right = FALSE, labels = toetsadviezen))
    cet_toetsadviezen = as.data.frame(table(cet_toetsadviezen), stringsAsFactors = FALSE)
    colnames(cet_toetsadviezen) = c('toetsadvies', 'n')
    cet_toetsadviezen$streefpercentage = prop.table(cet_toetsadviezen$n) * 100.0 # Is eigenlijk geen streefpercentage
    all_toetsadviezen_irt2 = rbind(all_toetsadviezen_irt2, cet_toetsadviezen)
  } else {

    streefpercentages_irt2 = read.csv2(file.path(resultaten_folder, irt2_files[grepl(aanbieder, irt2_files)]))

    aanbieder_leerlingen = aanbieder_leerlingen[aanbieder_leerlingen[, 'schooltype'] == 1, 'schooladvies', drop = FALSE]
    aanbieder_leerlingen$aanbieder = aanbieder
    all_schooladviezen[[aanbieder]] = aanbieder_leerlingen

    toetsadviezen_irt2 = streefpercentages_irt2[, c('toetsadvies', 'streefpercentage')]
    toetsadviezen_irt2$n = toetsadviezen_irt2$streefpercentage * 0.01 * nrow(aanbieder_leerlingen)
    all_toetsadviezen_irt2 = rbind(all_toetsadviezen_irt2, toetsadviezen_irt2)
  }
}

all_schooladviezen = dplyr::bind_rows(all_schooladviezen)

# Bereken totale (cumulatieve) percentages
all_toetsadviezen_irt2 = plyr::ddply(all_toetsadviezen_irt2, 'toetsadvies', function(x) data.frame('n' = sum(x$n)))
all_toetsadviezen_irt2$perc = prop.table(all_toetsadviezen_irt2$n) * 100
all_toetsadviezen_irt2 = all_toetsadviezen_irt2[order(match(all_toetsadviezen_irt2$toetsadvies, toetsadviezen)), ]
all_toetsadviezen_irt2$cum_perc = cumsum(all_toetsadviezen_irt2$perc)
all_toetsadviezen_irt2 = dplyr::left_join(all_toetsadviezen_irt2, streefnormering[, c('toetsadvies', 'cum_2019', 'bovengrens')], by = 'toetsadvies')
all_toetsadviezen_irt2 = all_toetsadviezen_irt2[, c('toetsadvies', 'perc', 'cum_perc', 'cum_2019', 'bovengrens')]

# Bekijk of er overschrijdingen van de bovengrenzen zijn, pro/bb doen niet mee vanwege correctie (eigen verantwoordelijkheid CET) en vwo per definitie niet
overschrijdingen_irt2 = all_toetsadviezen_irt2[!all_toetsadviezen_irt2$toetsadvies %in% c('pro/vmbo bb', 'vwo') & all_toetsadviezen_irt2[, 'cum_perc'] > all_toetsadviezen_irt2[, 'bovengrens'], c('toetsadvies', 'cum_2019', 'bovengrens', 'cum_perc')]

# Bereken indien nodig nieuwe cesuurpunten voor de coronanormering
coronacesuren_cet = toetsadvies_cesuren[, c('grenspunt', 'cesuur_standaardscore')]
if (nrow(overschrijdingen_irt2) > 0) {

  message(paste0('Er waren ', nrow(overschrijdingen_irt2), ' van de bovengrenzen voor de IRT2-normering, er zal een coronacorrectie toegepast moeten worden:\n', paste(capture.output(print(overschrijdingen_irt2, row.names = FALSE)), collapse = '\n')))

  # Loop over alle overschrijdingen om nieuwe cesuren te bepalen
  for (i in 1:nrow(overschrijdingen_irt2)) {

    # Cesuurpunt voor dit toetsadvies volgens reguliere normering CET
    standaardscore_cesuur = toetsadvies_cesuren[grepl(paste0('^', overschrijdingen_irt2[i, 'toetsadvies']), toetsadvies_cesuren[, 'grenspunt']), 'cesuur_standaardscore']

    ss_cum_perc = data.frame('standaardscore_cesuur' = standaardscore_cesuur, 'cum_perc' = overschrijdingen_irt2[i, 'cum_perc']) # Dataframe om cumulatieve percentages per standaardscorecesuur bij te houden

    # Loop over mogelijke cesuurpunten, tussen huidige en grens van een niveau lager (zou voldoende moeten zijn)
    for (ss in seq(standaardscore_cesuur - 1, toetsadvies_cesuren[which(toetsadvies_cesuren[, 'cesuur_standaardscore'] == standaardscore_cesuur) - 1, 'cesuur_standaardscore'] + 1, by = -1)) {

      # Bepaal nieuwe toetsadviezen PCET met behulp van nieuw cesuurpunt
      coronacesuren_cet[grepl(paste0('^', overschrijdingen_irt2[i, 'toetsadvies']), coronacesuren_cet[, 'grenspunt']), 'cesuur_standaardscore'] = ss
      pcet_data$toetsadvies = as.character(cut(pcet_data$standaardscore, c(-Inf, sort(coronacesuren_cet$cesuur_standaardscore), Inf), right = FALSE, labels = toetsadviezen))

      ss_toetsadviezen = irt2_normering(all_schooladviezen[!grepl('CET', all_schooladviezen[, 'aanbieder']), ], pcet_data, sampling)

      # Bepaal ook toetsadviezen voor de totale populatie van alle toetsen van de CET met nieuwe standaardscoregrenzen
      cet_toetsadviezen = as.character(cut(cet_standaardscores, c(-Inf, sort(coronacesuren_cet$cesuur_standaardscore), Inf), right = FALSE, labels = toetsadviezen))
      cet_toetsadviezen = as.data.frame(table(cet_toetsadviezen), stringsAsFactors = FALSE)
      colnames(cet_toetsadviezen) = c('toetsadvies', 'streefaantal_CET')

      ss_toetsadviezen = dplyr::left_join(ss_toetsadviezen, cet_toetsadviezen, by = 'toetsadvies')
      ss_toetsadviezen$n = rowSums(ss_toetsadviezen[, grepl('streefaantal', colnames(ss_toetsadviezen))])
      ss_toetsadviezen$perc = prop.table(ss_toetsadviezen$n) * 100.0
      ss_toetsadviezen = ss_toetsadviezen[order(match(ss_toetsadviezen$toetsadvies, toetsadviezen)), ]
      ss_toetsadviezen$cum_perc = cumsum(ss_toetsadviezen$perc)

      ss_cum_perc = rbind(ss_cum_perc, data.frame('standaardscore_cesuur' = ss, 'cum_perc' = ss_toetsadviezen[ss_toetsadviezen[, 'toetsadvies'] == overschrijdingen_irt2[i, 'toetsadvies'], 'cum_perc']))
    }

    # Het kleinste verschil met het cumulatieve percentage van 2019 geeft de nieuwe cesuur
    nieuwe_cesuur = ss_cum_perc[which.min(abs(ss_cum_perc$cum_perc - overschrijdingen_irt2[i, 'cum_2019'])), 'standaardscore_cesuur']

    if (nieuwe_cesuur == standaardscore_cesuur) {
      message('Het was niet mogelijk om met een nieuwe standaardscorecesuur dichter in de buurt van het cumulatieve percentage van 2019 te komen')
    } else {
      message(paste0('De gecorrigeerde standaardscorecesuur voor ', overschrijdingen_irt2[i, 'toetsadvies'], ' is ', nieuwe_cesuur))
      coronacesuren_cet[grepl(paste0('^', overschrijdingen_irt2[i, 'toetsadvies']), coronacesuren_cet[, 'grenspunt']), 'cesuur_standaardscore'] = nieuwe_cesuur
    }
  }

  # Overzicht met nieuwe cesuren standaardscores
  colnames(coronacesuren_cet)[colnames(coronacesuren_cet) == 'cesuur_standaardscore'] = 'cesuur_coronacorrectie'
  coronacesuren_cet = dplyr::left_join(toetsadvies_cesuren[, c('grenspunt', 'cesuur_standaardscore')], coronacesuren_cet, by = 'grenspunt')

  write.csv2(coronacesuren_cet, file.path(resultaten_folder, 'cesuren_coronacorrectie_irt2.csv'), row.names = FALSE, quote = FALSE, na = '')

  # PCET-data met nieuwe cesuren
  pcet_data$toetsadvies = as.character(cut(pcet_data$standaardscore, c(-Inf, sort(coronacesuren_cet$cesuur_coronacorrectie), Inf), right = FALSE, labels = toetsadviezen))

  irt2_toetsadviezen_corona = irt2_normering(all_schooladviezen[!grepl('CET', all_schooladviezen[, 'aanbieder']), ], pcet_data, sampling)

  # Loop over private aanbieders om nieuwe streefpercentages te berekenen via IRT2-normering
  summary_toetsadviezen = NULL
  for (aanbieder in aanbieders[!grepl('CET', aanbieders)]) {

    aanbieder_toetsadviezen_irt2 = irt2_toetsadviezen_corona[, c('toetsadvies', colnames(irt2_toetsadviezen_corona)[grepl(aanbieder, colnames(irt2_toetsadviezen_corona))])]
    colnames(aanbieder_toetsadviezen_irt2) = gsub(paste0('_', aanbieder), '', colnames(aanbieder_toetsadviezen_irt2))
    aanbieder_toetsadviezen_irt2 = aanbieder_toetsadviezen_irt2[order(match(aanbieder_toetsadviezen_irt2$toetsadvies, toetsadviezen)), ]
    aanbieder_toetsadviezen_irt2$cum_perc = cumsum(aanbieder_toetsadviezen_irt2$streefpercentage)

    # Lees toetsadviezen IRT1-normering in (voor cesuur pro/bb-correctie)
    toetsadviezen_irt1 = read.csv2(file.path(resultaten_folder, toetsadvies_files[grepl(aanbieder, toetsadvies_files)]))

    aanbieder_abilities = all_abilities[all_abilities[, 'aanbieder'] == aanbieder, ]
    toetsadvies_cesuren = NULL
    for (model in c('1pl', '2pl')) {

      # Cesuurbepaling voor toetsadviezen
      model_glv = sort(aanbieder_abilities[!is.na(aanbieder_abilities[, paste0('glv_', model)]), paste0('glv_', model)])
      for (i in 1:(nrow(aanbieder_toetsadviezen_irt2) - 1)) {
        target_row = ceiling(aanbieder_toetsadviezen_irt2[i, 'cum_perc'] * 0.01 * length(model_glv))
        cesuur = model_glv[target_row]

        # Pas pro/bb-correctie toe indien nodig
        if (aanbieder_toetsadviezen_irt2[i, 'toetsadvies'] == 'pro/vmbo bb') {
          cesuur_pro = toetsadviezen_irt1[toetsadviezen_irt1[, 'toetsadvies'] == 'pro/vmbo bb', paste0('cesuur_', model)]
          cesuur = cesuur_pro
        }
  
        gerealiseerd_percentage = length(model_glv[model_glv < cesuur]) / length(model_glv) * 100
        toetsadvies_cesuren = rbind(toetsadvies_cesuren, data.frame('toetsadvies' = aanbieder_toetsadviezen_irt2[i, 'toetsadvies'], 'model' = model, 'cesuur' = cesuur, 'cum_perc_behaald' = gerealiseerd_percentage))
      }
      toetsadvies_cesuren = rbind(toetsadvies_cesuren, data.frame('toetsadvies' = tail(aanbieder_toetsadviezen_irt2$toetsadvies, 1), 'model' = model, 'cesuur' = NA, 'cum_perc_behaald' = 100.0)) # Ook nog even hoogste adviescategorie toevoegen (geen cesuur)
    }

    # Tabel met toetsadviezen mooi maken en streefpercentages toevoegen
    toetsadvies_cesuren = reshape(toetsadvies_cesuren, idvar = c('toetsadvies'), timevar = 'model', direction = 'wide', sep = '_')
    for (column in colnames(toetsadvies_cesuren)[grepl('cum_perc_behaald', colnames(toetsadvies_cesuren))]) {
      toetsadvies_cesuren[, gsub('cum_', '', column)] = c(toetsadvies_cesuren[1, column], diff(toetsadvies_cesuren[, column])) # Ga nog even van cumulatieve naar normale percentages
      toetsadvies_cesuren = toetsadvies_cesuren[, colnames(toetsadvies_cesuren) != column]
    }
    toetsadvies_cesuren = dplyr::left_join(aanbieder_toetsadviezen_irt2[, colnames(aanbieder_toetsadviezen_irt2) != 'cum_perc'], toetsadvies_cesuren, by = 'toetsadvies')

    # Afrondingen
    for (column in colnames(toetsadvies_cesuren)[grepl('streefpercentage|perc_behaald', colnames(toetsadvies_cesuren))]) {
      toetsadvies_cesuren[, column] = round(toetsadvies_cesuren[, column], 2)
    }

    # Schrijf resultaten weg
    write.csv2(toetsadvies_cesuren[, colnames(toetsadvies_cesuren) != 'streefaantal'], paste0(resultaten_folder, '/', aanbieder, '_toetsadviezen_irt2_coronacorrectie.csv'), row.names = FALSE, quote = FALSE, na = '')

    toetsadvies_cesuren$aanbieder = aanbieder
    summary_toetsadviezen = rbind(summary_toetsadviezen, toetsadvies_cesuren[, c('aanbieder', 'toetsadvies', 'streefaantal', 'streefpercentage')])
  }

  # Bereken gecorrigeerde toetsadviezen CET op basis van nieuwe standaardscoregrenzen
  for (aanbieder in aanbieders[grepl('CET', aanbieders)]) {
    cet_toetsadviezen = as.character(cut(cet_standaardscores, c(-Inf, sort(coronacesuren_cet$cesuur_coronacorrectie), Inf), right = FALSE, labels = toetsadviezen))
    cet_toetsadviezen = as.data.frame(table(cet_toetsadviezen), stringsAsFactors = FALSE)
    colnames(cet_toetsadviezen) = c('toetsadvies', 'streefaantal')
    cet_toetsadviezen$streefpercentage = round(prop.table(cet_toetsadviezen$streefaantal) * 100.0, 2)
    cet_toetsadviezen = cet_toetsadviezen[order(match(cet_toetsadviezen$toetsadvies, toetsadviezen)), ]

    write.csv2(cet_toetsadviezen[, colnames(cet_toetsadviezen) != 'streefaantal'], paste0(resultaten_folder, '/', aanbieder, '_toetsadviezen_irt2_coronacorrectie.csv'), row.names = FALSE, quote = FALSE, na = '')

    cet_toetsadviezen$aanbieder = aanbieder
    summary_toetsadviezen = rbind(summary_toetsadviezen, cet_toetsadviezen[, c('aanbieder', 'toetsadvies', 'streefaantal', 'streefpercentage')])
  }

  # Samenvattingstabel vormgeven
  summary_toetsadviezen = reshape(summary_toetsadviezen, idvar = 'toetsadvies', timevar = 'aanbieder', direction = 'wide', sep = '_')
  summary_toetsadviezen$n_totaal = rowSums(summary_toetsadviezen[, grepl('streefaantal', colnames(summary_toetsadviezen))])
  summary_toetsadviezen$perc_covid = prop.table(summary_toetsadviezen$n_totaal) * 100.0
  summary_toetsadviezen$cum_perc_covid = cumsum(summary_toetsadviezen$perc_covid)
  summary_toetsadviezen = summary_toetsadviezen[, !grepl('streefaantal|n_totaal', colnames(summary_toetsadviezen))]
  colnames(summary_toetsadviezen) = gsub('streefpercentage', 'perc_covid', colnames(summary_toetsadviezen))

  all_toetsadviezen_irt2 = dplyr::left_join(all_toetsadviezen_irt2, summary_toetsadviezen, by = 'toetsadvies')

} else {
  message('Het was niet nodig om een coronacorrectie uit te voeren op de IRT2-normering')
}

# Samenvattingstabel wegschrijven
all_toetsadviezen_irt2 = dplyr::mutate_if(all_toetsadviezen_irt2, is.numeric, round, digits = 2)
write.csv2(all_toetsadviezen_irt2, file.path(resultaten_folder, 'coronacorrectie_irt2.csv'), row.names = FALSE, quote = FALSE, na = '')
