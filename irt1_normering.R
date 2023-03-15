# Dit script wordt gebruikt om de IRT1-normering uit te voeren voor een of meerdere toetsaanbieders
# Tevens wordt de pro/bb-correctie uitgevoerd op basis van schooladviezen

# De output van dit script bestaat per toetsaanbieder uit:
# - Uitgeschakelde items
#   Lijst van items die uitgeschakeld zijn omdat ze een discriminatieparameter lager dan 0.1 hebben
#   bestandsnaam TOETS_uitgeschakeld.csv
# - itemparameters
#   Parameterschattingen voor het 1PL- en 2PL-model
#   bestandsnaam TOETS_itemparameters.csv
# - (MML-)populatieparameters
#   Populatieparameters voor het 1PL- en 2PL-mode
#   bestandsnaam TOETS_populatieparameters.csv
# - Vaardigheidsschattingen
#   Voor alle leerlingen vaardigheidsschattingen, GLV's en toetsadviezen in het 1PL- en 2PL-model
#   bestandsnaam TOETS_vaardigheden.csv
# - Behaalde referentieniveaus
#   Overzichtstabel met percentages behaalde referentieniveaus
#   bestandsnaam TOETS_referentieniveaus_irt1.csv
# - Behaalde toetsadviezen
#   Overzichtstabel met percentages behaalde toetsadviezen en cesuren
#   bestandsnaam TOETS_toetsadviezen_irt1.csv
# Daarnaast worden er twee bestanden weggeschreven met overzichtstabellen met alle aanbieders met behaalde referentieniveaus en
# behaalde toetsadviezen, met bestandsnamen overzicht_referentieniveaus_irt1.csv en overzicht_toetsadviezen_irt1.csv
# Tot slot is er een Excelbestand overzicht_correlatiematrices_irt1.xlsx met correlatiematrices van alle onderdelen in beide modellen voor alle toetsaanbieders

# De volgende R-packages worden gebruikt in dit script:
# dexterMML (https://github.com/dexter-psychometrics/dexterMML), openxlsx, reshape2, plyr en dplyr

# Invoer ==================================================

# Folder met alle databestanden
# De volgende databestanden worden verwacht per toetsaanbieder:
# - Leerlingdata (TOETS_leerlingen.csv)
# - Scoredata per onderdeel (TOETS_ONDERDEEL.csv)
# - Optioneel noscore-items (TOETS_noscore.csv)
data_folder = 'dummy_data'

# Aanbieders waar we rekening houden met verschillende MML-populaties
populatie_aanbieders = c('PCET', 'ROUTE8')

# Bestand met bestaande gezamenlijk ankerparameters, bevat een tabblad '1pl' en een tabblad '2pl'
anker_file = 'dummy_resultaten/ankerparameters_2023.xlsx'

# Bestand met uitgeschakelde en losgekoppelde ankeritems, bevat kolommen aanbieder, item_id, onderdeel en actie (los of uit)
items_off_file = 'anker_off.csv'

# Bestand met cesuren en onderdeelgewichten, bevat tabbladen ref_cesuren, toetsadvies_cesuren en onderdeelgewichten
normeringsgegevens_file = 'normeringsgegevens_dummy.xlsx'

# Output folder
output_folder = 'dummy_resultaten'

# Populatiemap voor route8, welke populatie is welke toets?
route8_map = list('ROUTE8' = 1, 'ROUTE8PLUS' = 2)

# Code ====================================================

options(warn = 1)
options(width = 200)

# Inventariseer alle bestanden
leerling_files = list.files(path = data_folder, pattern = '_leerlingen.csv')
onderdeel_files = list.files(path = data_folder, pattern = '.csv')
onderdeel_files = onderdeel_files[!grepl('leerlingen|controle|representiviteit|representativiteit', onderdeel_files)]
noscore_files = list.files(path = data_folder, pattern = '_noscore.dat')

# Lees parameters gezamenlijk anker in
anker_parameters = list()
for (model in c('1pl', '2pl')) {
  anker_parameters[[model]] = openxlsx::read.xlsx(anker_file, model)
}

# Check op gelijkheid items voor beide modellen
if (!setequal(anker_parameters[['1pl']]$item_id, anker_parameters[['2pl']]$item_id)) {
  stop('Er is een mismatch tussen de items waarvoor parameters beschikbaar zijn in beide modellen')
}

items_off = read.csv2(items_off_file)

# Check op items_off
if (!all(items_off$actie %in% c('los', 'uit'))) {
  warning(paste0('Niet alle acties in het bestand met uitgeschakelde items zijn gelijk aan "los" of "uit"'))
}

# Verplichte onderdelen
verplichte_onderdelen = unique(anker_parameters[['1pl']]$onderdeel)

# Lees normeringsgegevens in
ref_cesuren = openxlsx::read.xlsx(normeringsgegevens_file, 'ref_cesuren')
toetsadvies_cesuren = openxlsx::read.xlsx(normeringsgegevens_file, 'toetsadvies_cesuren')
toetsadvies_cesuren = toetsadvies_cesuren[, colnames(toetsadvies_cesuren) != 'cesuur_standaardscore']
onderdeelgewichten = openxlsx::read.xlsx(normeringsgegevens_file, 'onderdeelgewichten')

# Deduceer lijst van toetsadviezen op volgorde uit toetsadvies-cesuren
toetsadviezen = unique(unlist(strsplit(toetsadvies_cesuren[order(toetsadvies_cesuren$cesuur_1pl), 'grenspunt'], split = ' -> ')))

summary_refs = NULL
summary_toetsadviezen = NULL
wb = openxlsx::createWorkbook() # workbook voor correlaties
for (leerling_file in leerling_files) {

  aanbieder = gsub('_leerlingen.csv', '', leerling_file, fixed = TRUE)

  message(paste0('Bezig met aanbieder ', aanbieder, '...'))

  # Lees leerlingenbestand in
  leerlingen = read.csv2(file.path(data_folder, leerling_file))
  leerlingen$person_id = as.character(leerlingen$person_id) # Anders gaan joins met abilities uit dexterMML verkeerd

  # Als er geen MML-populaties zijn schrijven we hier de aanbiedernaam als placeholder
  if (!aanbieder %in% populatie_aanbieders) {
    leerlingen$populatie = aanbieder
  } 
  leerlingen = leerlingen[, c('person_id', 'schooltype', 'populatie', 'schooladvies')]

  # Lees noscore-items in indien nodig
  noscore_items = NULL
  if (length(noscore_files[grepl(aanbieder, noscore_files)]) > 0) {
    noscore_items = readLines(file.path(data_folder, noscore_files[grepl(aanbieder, noscore_files)]))
  }

  # Sheet voor correlaties
  openxlsx::addWorksheet(wb, aanbieder)
  wb_rows = 1

  all_abilities = NULL
  all_removed_items = NULL
  aanbieder_parameters = list('1pl' = NULL, '2pl' = NULL)
  aanbieder_populations = NULL
  # Loop over ieder onderdeel om de kalibratie uit te voeren
  for (onderdeel_file in onderdeel_files[grepl(aanbieder, onderdeel_files)]) {

    onderdeel = gsub(paste0(aanbieder, '_|\\.csv'), '', onderdeel_file)

    message(paste0('  Bezig met onderdeel ', onderdeel, '...'))

    # Lees onderdeelscores in
    score_data = read.csv2(file.path(data_folder, onderdeel_file), check.names = FALSE)
    score_data$person_id = as.character(score_data$person_id) # Anders gaan joins met abilities uit dexterMML verkeerd

    # Maak long-format van wide-format data indien nodig
    if (!'item_id' %in% colnames(score_data)) {
      score_data = reshape2::melt(score_data, id.vars = 'person_id', value.name = 'item_score')
      colnames(score_data)[colnames(score_data) == 'variable'] = 'item_id'
      score_data$item_id = as.character(score_data$item_id)
    }

    # Verwijder missings
    score_data = score_data[!is.na(score_data$item_score) & score_data$item_score != 9, ]

    # Verwijder uitgeschakelde ankeritems
    score_data = score_data[!score_data[, 'item_id'] %in% items_off[items_off[, 'onderdeel'] == onderdeel & items_off[, 'aanbieder'] == aanbieder & items_off[, 'actie'] == 'uit', 'item_id'], ]

    # Koppel eventueel ook ankeritems los
    for (item_id in items_off[items_off[, 'onderdeel'] == onderdeel & items_off[, 'aanbieder'] == aanbieder & items_off[, 'actie'] == 'los', 'item_id']) {
      score_data[score_data[, 'item_id'] == item_id, 'item_id'] = paste0('x', item_id, '_', aanbieder)
      if (item_id %in% noscore_items) {
        noscore_items[noscore_items == item_id] = paste0('x', item_id, '_', aanbieder) # Zorg ook dat dit met de noscore-items goed gaat
      }
    }

    # Voeg persoonsparameters toe en maak kalibratiedata
    score_data = dplyr::inner_join(score_data, leerlingen, by = 'person_id')
    cal_data = score_data[score_data[, 'schooltype'] == 1, ]

    # Bepaal aantal score-categorieen en observaties per item
    score_categories = plyr::ddply(cal_data, 'item_id', function(x) {
      return(data.frame('ncat' = length(unique(x$item_score)),
                        'n' = nrow(x)))
    })

    # Haal items eruit met maar 1 score-categorie, dit gaat niet goed in de kalibratie, hou ook minimum van 150 observaties aan
    cal_data = cal_data[cal_data[, 'item_id'] %in% score_categories[score_categories[, 'ncat'] > 1 & score_categories[, 'n'] > 150, 'item_id'], ]

    message('    Bezig met initiele 2pl-kalibratie')

    # Initiele 2pl-kalibratie om slechte items eruit te gooien
    fixed_parameters = anker_parameters[['2pl']][anker_parameters[['2pl']][, 'onderdeel'] == onderdeel &
                                                 anker_parameters[['2pl']][, 'item_id'] %in% score_categories[score_categories[, 'ncat'] > 1 &score_categories[, 'n'] > 150, 'item_id'], ]
    if (nrow(fixed_parameters) == 0) {
      fixed_parameters = NULL
    }
    cal = dexterMML::fit_2pl(cal_data, fixed_param = fixed_parameters, group = 'populatie')

    parameters = as.data.frame(coef(cal))

    # Verwijder items met discriminatieparameter lager dan 0.1
    remove_items = parameters[parameters[, 'alpha'] < 0.1, ]
    if (nrow(remove_items) > 0) {
      remove_items$onderdeel = onderdeel
      remove_items = remove_items[, c('item_id', 'onderdeel', colnames(remove_items)[!colnames(remove_items) %in% c('item_id', 'onderdeel')])] # Sorteer kolommen
      all_removed_items = rbind(all_removed_items, remove_items) # Hou verwijderde items bij
      cal_data = cal_data[!cal_data[, 'item_id'] %in% remove_items$item_id, ]
    }

    # Loop over de verschillende modellen
    for (model in c('1pl', '2pl')) {

      message(paste0('    Bezig met ', model, '-kalibratie'))
  
      # Gefixeerde parameters voor dit onderdeel
      fixed_parameters = anker_parameters[[model]][anker_parameters[[model]][, 'onderdeel'] == onderdeel &
                                                   anker_parameters[[model]][, 'item_id'] %in% score_categories[score_categories[, 'ncat'] > 1 & score_categories[, 'n'] > 150, 'item_id'], ]
      if (nrow(fixed_parameters) == 0) {
        fixed_parameters = NULL
      }
      
      # Kalibraties
      if (model == '1pl') {
        cal = dexterMML::fit_1pl(cal_data, fixed_param = fixed_parameters, group = 'populatie')
      } else {
        cal = dexterMML::fit_2pl(cal_data, fixed_param = fixed_parameters, group = 'populatie')
      }
  
      # Populatieparameters voor diagnostische doeleinden
      populations = as.data.frame(coef(cal, 'populations'))
      populations$onderdeel = onderdeel
      populations$model = model
      aanbieder_populations = rbind(aanbieder_populations, populations)

      # Itemparameters
      parameters = as.data.frame(coef(cal))
      parameters$onderdeel = onderdeel
      aanbieder_parameters[[model]] = rbind(aanbieder_parameters[[model]], parameters[, colnames(parameters) != 'item_score'])
  
      # Maak data voor vaardigheidsschatting, alleen met items die we hebben kunnen kalibreren maar nu inclusief alle onderwijstypen
      ability_data = score_data[score_data[, 'item_id'] %in% parameters$item_id, ]

      # Verwijder noscore-items indien nodig
      if (!is.null(noscore_items)) {
        ability_data = ability_data[!ability_data[, 'item_id'] %in% noscore_items, ]
      }

      # Vaardigheidsschatter
      abilities = as.data.frame(dexterMML::ability.mml(ability_data, parms = parameters, method = 'WLE'))

      # Sla op voor latere analyse
      colnames(abilities)[colnames(abilities) != 'person_id'] = paste0(colnames(abilities)[colnames(abilities) != 'person_id'], '_', model, '_', onderdeel)
      if (is.null(all_abilities)) {
        all_abilities = abilities
      } else {
        all_abilities = dplyr::full_join(all_abilities, abilities, by = 'person_id')
      }
    }
  }

  # Voeg persoonsparameters (populatie, schooltype) in
  all_abilities = dplyr::left_join(all_abilities, leerlingen, by = 'person_id')

  # Bepaal behaalde referentieniveaus voor ieder model
  aanbieder_ref_behaald = NULL
  for (onderdeel in unique(ref_cesuren$onderdeel)) {
    for (refniveau in unique(ref_cesuren[ref_cesuren[, 'onderdeel'] == onderdeel, 'niveau'])) {
      for (model in c('1pl', '2pl')) {
        cesuur = ref_cesuren[ref_cesuren[, 'onderdeel'] == onderdeel & ref_cesuren[, 'niveau'] == refniveau, paste0('cesuur_', model)]
        perc_behaald = round(nrow(all_abilities[all_abilities[, 'schooltype'] == 1 & !is.na(all_abilities[, paste0('theta_', model, '_', onderdeel)]) & all_abilities[, paste0('theta_', model, '_', onderdeel)] >= cesuur, ]) / 
                             nrow(all_abilities[all_abilities[, 'schooltype'] == 1 & !is.na(all_abilities[, paste0('theta_', model, '_', onderdeel)]), ]) * 100.0, 2)
        aanbieder_ref_behaald = rbind(aanbieder_ref_behaald, data.frame('model' = model, 'aanbieder' = aanbieder, 'onderdeel' = onderdeel, 'niveau' = refniveau, 'perc_behaald' = perc_behaald))
      }
    }
  }  

  aanbieder_ref_behaald = reshape(aanbieder_ref_behaald, idvar = c('aanbieder', 'onderdeel', 'niveau'), timevar = 'model', direction = 'wide', sep = '_')

  # Route8 opsplitsen in 2 toetsen voor GLV-berekening
  if (aanbieder == 'ROUTE8') {
    toetsen = names(route8_map)
  } else {
    toetsen = aanbieder
  }

  # Bereken GLV
  all_glv = list('1pl' = list(), '2pl' = list())
  for (toets in toetsen) {

    # Bepaal onderdeelgewichten voor deze aanbieder (named vector)
    aanbieder_gewichten = unlist(onderdeelgewichten[onderdeelgewichten[, 'toets'] == toets, !colnames(onderdeelgewichten) %in% c('toets', 'totaal')])
    aanbieder_gewichten = aanbieder_gewichten[!is.na(aanbieder_gewichten)]

    for (model in c('1pl', '2pl')) {

      glv_data = all_abilities[, c('person_id', 'schooltype', 'populatie', paste0('theta_', model, '_', names(aanbieder_gewichten)))]
      # Hou rekening met route8
      if (aanbieder == 'ROUTE8') {
        glv_data = glv_data[glv_data[, 'populatie'] == route8_map[[toets]], ]
      }

      # Bepaal correlatiematrix tussen de verschillende onderdelen
      cor_matrix = cor(all_abilities[, paste0('theta_', model, '_', names(aanbieder_gewichten))], use = 'complete.obs')
      colnames(cor_matrix) = gsub('theta_|1pl_|2pl_', '', colnames(cor_matrix))
      rownames(cor_matrix) = gsub('theta_|1pl_|2pl_', '', rownames(cor_matrix))
      if (any(cor_matrix < 0.4)) {
        warning(paste0('Er waren correlaties tussen onderdelen lager dan 0.4 bij ', toets, ' in het ', model, ' model.'))
      } 
      openxlsx::writeData(wb, aanbieder, data.frame('x' = paste0(toets, ' ', model)), startRow = wb_rows, borders = 'none', colNames = FALSE)
      openxlsx::addStyle(wb, aanbieder, rows = wb_rows, cols = 1, style = openxlsx::createStyle(textDecoration = 'bold'))
      openxlsx::writeData(wb, aanbieder, as.data.frame(cor_matrix), startRow = wb_rows + 1, borders = 'none', rowNames = TRUE)
      wb_rows = wb_rows + nrow(cor_matrix) + 3

      # correlaties[[model]][[toets]] = cor_matrix

      # Kijk of er leerlingen zijn met ontbrekende onderdelen
      na_leerlingen = glv_data[!complete.cases(glv_data[, paste0('theta_', model, '_', names(aanbieder_gewichten))]), !colnames(glv_data) %in% c('schooltype', 'populatie')]
      if (nrow(na_leerlingen) > 0) {
        warning(paste0('Er waren ', nrow(na_leerlingen), ' leerlingen bij ', toets, ' met een of meer onderdelen zonder vaardigheid, namelijk:\n', paste(capture.output(print(na_leerlingen, row.names = FALSE)), collapse = '\n')))
        glv_data = glv_data[!glv_data[, 'person_id'] %in% na_leerlingen$person_id, ]
      }

      # Bereken gemiddelden
      theta_means = colMeans(glv_data[glv_data[, 'schooltype'] == 1, paste0('theta_', model, '_', names(aanbieder_gewichten))]) # Schaling bepalen we alleen op regulier BO
      mean_verplicht = mean(theta_means[paste0('theta_', model, '_', verplichte_onderdelen)]) # Gemiddelde van verplichte onderdelen samen

      # Voer schaling uit voor optionele onderdelen (stel gemiddelde gelijk aan gemiddelde theta verplichte onderdelen)
      for (optioneel_onderdeel in names(aanbieder_gewichten)[!names(aanbieder_gewichten) %in% verplichte_onderdelen]) {
        glv_data[, paste0('theta_', model, '_', optioneel_onderdeel)] = glv_data[, paste0('theta_', model, '_', optioneel_onderdeel)] + (mean_verplicht - theta_means[paste0('theta_', model, '_', optioneel_onderdeel)])
      }

      # Bereken GLV
      glv_data[, paste0('glv_', model)] = apply(glv_data[, paste0('theta_', model, '_', names(aanbieder_gewichten))], 1, weighted.mean, aanbieder_gewichten)

      all_glv[[model]][[length(all_glv[[model]]) + 1]] = glv_data[, c('person_id', paste0('glv_', model))]
    }
  }

  # Voeg GLVs bij abilities, bepaal pro/bb-correctie, bereken toetsadviezen en bereken percentages behaalde toetsadviezen
  aanbieder_toetsadviezen = NULL
  aanbieder_cesuren = toetsadvies_cesuren
  for (model in c('1pl', '2pl')) {
    model_glv = dplyr::bind_rows(all_glv[[model]])
    all_abilities = dplyr::left_join(all_abilities, model_glv, by = 'person_id')

    # Pro/bb-correctie
    if (!grepl('CET', aanbieder)) {

      # Maak data om pro/bb-correctieve cesuur op te bepalen
      data_probb = all_abilities[all_abilities[, 'schooladvies'] %in% 1:3 & all_abilities[, 'schooltype'] == 1, c('person_id', 'schooladvies', paste0('glv_', model))]
      data_probb = data_probb[complete.cases(data_probb), ]
      data_probb = data_probb[order(data_probb[, paste0('glv_', model)]), ]
   
      # Loop over verschillende clusteringen
      glv_cesuren = NULL
      for (m in 7:26) {
   
        # Maak clustering en bereken proporties schooladviezen
        clusters = plyr::ldply(split(data_probb, rep(1:m, length.out = nrow(data_probb), each = ceiling(nrow(data_probb)/m))), function(x) {
          cluster_leerlingen = nrow(x)
          return(data.frame('mean_glv' = mean(x[, paste0('glv_', model)]),
                            'p_pro' = nrow(x[x$schooladvies == 1, ]) / cluster_leerlingen,
                            'p_bb' = nrow(x[x$schooladvies == 2, ]) / cluster_leerlingen,
                            'p_bbkb' = nrow(x[x$schooladvies == 3, ]) / cluster_leerlingen))
        })
   
        # Toepassen beslisregels toetsadviezen
        clusters$toetsadvies = NA
        clusters[clusters[, 'p_pro'] > clusters[, 'p_bb'] & clusters[, 'p_pro'] > clusters[, 'p_bbkb'], 'toetsadvies'] = 1
        clusters[clusters[, 'p_bbkb'] > clusters[, 'p_bb'] & clusters[, 'p_bbkb'] > clusters[, 'p_pro'], 'toetsadvies'] = 2
        clusters[clusters[, 'p_bb'] > clusters[, 'p_bbkb'] & clusters[, 'p_bb'] > clusters[, 'p_pro'] & clusters[, 'p_pro'] > clusters[, 'p_bbkb'], 'toetsadvies'] = 1
        clusters[clusters[, 'p_bb'] > clusters[, 'p_bbkb'] & clusters[, 'p_bb'] > clusters[, 'p_pro'] & clusters[, 'p_bbkb'] > clusters[, 'p_pro'], 'toetsadvies'] = 2
   
        # Eis dat er geen NA's meer zijn, beide toetsadviezen moeten voorkomen en dat ze monotoon stijgend zijn
        if (any(is.na(clusters$toetsadvies)) || !all(1:2 %in% clusters$toetsadvies) || !all(clusters$toetsadvies == cummax(clusters$toetsadvies))) {
          next
        }
   
        # Bereken GLV van omslagpunt
        glv_cesuur = (tail(clusters[clusters[, 'toetsadvies'] == 1, 'mean_glv'], 1) + head(clusters[clusters[, 'toetsadvies'] == 2, 'mean_glv'], 1)) / 2
        glv_cesuren = c(glv_cesuren, glv_cesuur)
      }
   
      if (is.null(glv_cesuren)) {
        warning('Het is niet gelukt om een cesuur voor de pro/bb-correctie te berekenen')
      } else {
        probb_cesuur = mean(glv_cesuren)
        message(paste0('De cesuur voor de pro/bb-correctie is bepaald op ', round(probb_cesuur, 3), ' in het ', model, '-model'))
        aanbieder_cesuren[aanbieder_cesuren[, 'grenspunt'] == 'pro/vmbo bb -> vmbo bb/kb', paste0('cesuur_', model)] = probb_cesuur
      }
    }

    all_abilities[, paste0('toetsadvies_', model)] = as.character(cut(all_abilities[, paste0('glv_', model)], c(-Inf, sort(aanbieder_cesuren[, paste0('cesuur_', model)]), Inf), right = FALSE, labels = toetsadviezen))

    # Bereken behaalde toetsadviezen
    toetsadviezen_behaald = as.data.frame(table(all_abilities[all_abilities[, 'schooltype'] == 1, paste0('toetsadvies_', model)]), stringsAsFactors = FALSE)
    colnames(toetsadviezen_behaald) = c('toetsadvies', 'n')
    toetsadviezen_behaald$perc_behaald = round(prop.table(toetsadviezen_behaald$n) * 100.0, 2)
    toetsadviezen_behaald$aanbieder = aanbieder
    toetsadviezen_behaald$model = model

    aanbieder_toetsadviezen = rbind(aanbieder_toetsadviezen, toetsadviezen_behaald[, colnames(toetsadviezen_behaald) != 'n'])
  }

  aanbieder_toetsadviezen = reshape(aanbieder_toetsadviezen, idvar = c('aanbieder', 'toetsadvies'), timevar = 'model', direction = 'wide', sep = '_')
  aanbieder_toetsadviezen = aanbieder_toetsadviezen[order(match(aanbieder_toetsadviezen$toetsadvies, toetsadviezen)), ]

  # Join ook cesuren erin
  aanbieder_cesuren$toetsadvies = sapply(aanbieder_cesuren$grenspunt, function(x) strsplit(x, split = ' -> ')[[1]][1])
  aanbieder_toetsadviezen = dplyr::left_join(aanbieder_toetsadviezen, aanbieder_cesuren[, colnames(aanbieder_cesuren) != 'grenspunt'], by = 'toetsadvies')
  aanbieder_toetsadviezen = aanbieder_toetsadviezen[, c('aanbieder', 'toetsadvies', colnames(aanbieder_toetsadviezen)[grepl('cesuur', colnames(aanbieder_toetsadviezen))], colnames(aanbieder_toetsadviezen)[grepl('perc_behaald', colnames(aanbieder_toetsadviezen))])]

  # Vanaf hier genereren we output

  # Schrijf verwijderde items weg
  if (!is.null(all_removed_items)) {
    for (column in colnames(all_removed_items)[grepl('beta|alpha', colnames(all_removed_items))]) {
      all_removed_items[, column] = round(all_removed_items[, column], 5)
    }
    write.csv2(all_removed_items[, colnames(all_removed_items) != 'item_score'], paste0(output_folder, '/', aanbieder, '_uitgeschakeld.csv'), row.names = FALSE, quote = FALSE)
  } else {
    file.create(paste0(output_folder, '/', aanbieder, '_uitgeschakeld.csv')) # Schrijf leeg bestand
  }

  # Schrijf itemparameters weg
  for (model in c('1pl', '2pl')) {
    colnames(aanbieder_parameters[[model]])[!colnames(aanbieder_parameters[[model]]) %in% c('item_id', 'onderdeel')] = paste0(colnames(aanbieder_parameters[[model]])[!colnames(aanbieder_parameters[[model]]) %in% c('item_id', 'onderdeel')], '_', model)
  }
  aanbieder_parameters = dplyr::left_join(aanbieder_parameters[['1pl']], aanbieder_parameters[['2pl']], by = c('item_id', 'onderdeel'))
  for (column in colnames(aanbieder_parameters)[grepl('_1pl|_2pl', colnames(aanbieder_parameters))]) {
    aanbieder_parameters[, column] = round(aanbieder_parameters[, column], 5)
  }
  write.csv2(aanbieder_parameters[, c('item_id', 'onderdeel', colnames(aanbieder_parameters)[grepl('_1pl|_2pl', colnames(aanbieder_parameters))])], paste0(output_folder, '/', aanbieder, '_itemparameters.csv'), row.names = FALSE, quote = FALSE, na = '')

  # Schrijf populatieparameters weg
  aanbieder_populations = reshape(aanbieder_populations, idvar = c('populatie', 'onderdeel', 'group_n'), timevar = 'model', direction = 'wide', sep = '_')
  for (column in colnames(aanbieder_populations)[grepl('_1pl|_2pl', colnames(aanbieder_populations))]) {
    aanbieder_populations[, column] = round(aanbieder_populations[, column], 5)
  }
  write.csv2(aanbieder_populations, paste0(output_folder, '/', aanbieder, '_populatieparameters.csv'), row.names = FALSE, quote = FALSE, na = '')

  # Bereid vaardigheid-data voor om weg te schrijven en schrijf weg
  for (column in colnames(all_abilities)[grepl('theta_|se_|glv_', colnames(all_abilities))]) {
    all_abilities[, column] = round(all_abilities[, column], 5)
  }
  all_abilities = all_abilities[, c('person_id', 'schooltype', 'populatie', colnames(all_abilities)[grepl('theta_|se_|glv_', colnames(all_abilities))], colnames(all_abilities)[grepl('toetsadvies_', colnames(all_abilities))])]
  if (!aanbieder %in% populatie_aanbieders) {
    all_abilities = all_abilities[, colnames(all_abilities) != 'populatie']
  }
  write.csv2(all_abilities, paste0(output_folder, '/', aanbieder, '_vaardigheden.csv'), row.names = FALSE, quote = FALSE, na = '')

  # Schrijf ook behaalde referentieniveaus en toetsasviezen weg
  write.csv2(aanbieder_ref_behaald[, colnames(aanbieder_ref_behaald) != 'aanbieder'], paste0(output_folder, '/', aanbieder, '_referentieniveaus_irt1.csv'), row.names = FALSE, quote = FALSE)
  write.csv2(aanbieder_toetsadviezen[, colnames(aanbieder_toetsadviezen) != 'aanbieder'], paste0(output_folder, '/', aanbieder, '_toetsadviezen_irt1.csv'), row.names = FALSE, quote = FALSE, na = '')

  summary_refs = rbind(summary_refs, aanbieder_ref_behaald)
  summary_toetsadviezen = rbind(summary_toetsadviezen, aanbieder_toetsadviezen)
}

# Schrijf samenvattingsbestanden weg
summary_refs = reshape(summary_refs, idvar = c('onderdeel', 'niveau'), timevar = 'aanbieder', direction = 'wide', sep = '_')
summary_toetsadviezen = reshape(summary_toetsadviezen, idvar = c('toetsadvies'), timevar = 'aanbieder', direction = 'wide', sep = '_')
write.csv2(summary_refs, file.path(output_folder, 'overzicht_referentieniveaus_irt1.csv'), row.names = FALSE, quote = FALSE)
write.csv2(summary_toetsadviezen, file.path(output_folder, 'overzicht_toetsadviezen_irt1.csv'), row.names = FALSE, quote = FALSE)

# Excelbestand met correlatiematrices
openxlsx::saveWorkbook(wb, file.path(output_folder, 'overzicht_correlatiematrices_irt1.xlsx'), overwrite = TRUE)
