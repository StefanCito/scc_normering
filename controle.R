# Dit script voert controles uit op de aangeleverde data voor de normering

# Als de PCET wordt ingelezen schrijft het script een doorslag van de PCET-data weg waarmee de IRT2-normering uitgevoerd kan worden

# De packages openxlsx, reshape2, plyr en dplyr zijn noodzakelijk om dit script te kunnen draaien

# Invoer ==================================================

# Folder met alle databestanden
# De volgende databestanden worden verwacht per toetsaanbieder:
# - Leerlingdata (TOETS_leerlingen.csv)
# - Scoredata per onderdeel (TOETS_ONDERDEEL.csv)
# - Controlebestanden (TOETS_controle.csv)
data_folder = 'dummy_data'

# Bestand met bestaande gezamenlijk ankerparameters
anker_file = 'ankerparameters_2021.xlsx'

# Bestand met onderdeelgewichten, bevat tabbladen onderdeelgewichten
normeringsgegevens_file = 'normeringsgegevens_dummy.xlsx'

# Code ====================================================

options(warn = 1)

# Inventariseer alle bestanden
leerling_files = list.files(path = data_folder, pattern = '_leerlingen.csv')
controle_files = list.files(path = data_folder, pattern = '_controle.csv')
onderdeel_files = list.files(path = data_folder, pattern = '.csv')
onderdeel_files = onderdeel_files[!onderdeel_files %in% c(leerling_files, controle_files)]

# Lees bestand met ankeritems
anker_items = openxlsx::read.xlsx(anker_file, '1pl')[, c('item_id', 'onderdeel')]
ref_onderdelen = unique(anker_items$onderdeel)

# Lees onderdeelgewichten
onderdeelgewichten = openxlsx::read.xlsx(normeringsgegevens_file, 'onderdeelgewichten')

# Leerlingbestand leidend in vaststellen welke aanbieders we hebben
for (leerling_file in leerling_files) {

  aanbieder = gsub('_leerlingen.csv', '', leerling_file, fixed = TRUE)

  message(paste0('Bezig met aanbieder ', aanbieder))

  leerlingen = read.csv2(file.path(data_folder, leerling_file))

  # Zijn de juiste kolommen aanwezig in leerlingenbestand?
  if (!all(c('person_id', 'schooltype', 'schooladvies') %in% colnames(leerlingen))) {
    warning(paste0('Niet alle verplichte kolomnamen zijn aanwezig in leerlingbestand ', aanbieder, '. Aanwezig zijn: ', paste(colnames(leerlingen), collapse = ', ')))
  }

  # Zijn alle person_ids uniek?
  duplicate_ids = leerlingen[duplicated(leerlingen$person_id), 'person_id']
  if (length(duplicate_ids) > 0) {
    warning(paste0('In leerlingbestand ', aanbieder, ' zaten dubbele person_ids, deze wordt overgeslagen.'))
    next
  }

  # Komen de juiste schooltypes voor?
  if (!all(leerlingen$schooltype %in% 1:4)) {
    warning(paste0('Niet alle schooltypes in het leerlingbestand zijn toegestane waarden. Aanwezig zijn: ', paste(unique(leerlingen$schooltype), collapse = ', ')))
  }

  # Welke onderdelen verwachten we op basis van de onderdeelgewichten?
  exp_onderdelen = unlist(onderdeelgewichten[grepl(paste0('^', aanbieder), onderdeelgewichten$toets), !colnames(onderdeelgewichten) %in% c('toets', 'totaal')])
  exp_onderdelen = unique(gsub('[[:digit:]]+', '', names(exp_onderdelen[!is.na(exp_onderdelen)])))

  # Kijk of er scorebestanden zijn van alle onderdelen die verwacht worden
  aanbieder_onderdelen = gsub(paste0(aanbieder, '_|\\.csv'), '', onderdeel_files[grepl(aanbieder, onderdeel_files)])
  if (!all(exp_onderdelen %in% aanbieder_onderdelen)) {
    warning(paste0('De volgende onderdelen worden verwacht bij ', aanbieder, ', maar is geen scorebestand van gevonden: ', paste(exp_onderdelen[!exp_onderdelen %in% aanbieder_onderdelen])))
  }

  p_values_data = list()
  # Loop over de onderdelen
  for (onderdeel_file in onderdeel_files[grepl(aanbieder, onderdeel_files)]) {

    onderdeel = gsub(paste0(aanbieder, '_|\\.csv'), '', onderdeel_file)

    message(paste0('  Bezig met onderdeel ', onderdeel))

    score_data = read.csv2(file.path(data_folder, onderdeel_file), check.names = FALSE)

    # Zijn alle leerlingen in de score opgenomen in de leerlingen?
    missing_leerlingen = score_data[!score_data$person_id %in% leerlingen$person_id, 'person_id']
    if (length(missing_leerlingen) > 0) {
      warning(paste0('In onderdeelbestand ', onderdeel_file, ' zitten leerlingen die niet in het leerlingenbestand zitten, namelijk:\n', paste(missing_leerlingen, collapse = '\n')))
    }

    # Zijn alle leerlingen uit leerlingen opgenomen in score?
    missing_leerlingen = leerlingen[!leerlingen$person_id %in% score_data$person_id, 'person_id']
    if (length(missing_leerlingen) > 0) {
      warning(paste0('In het leerlingenbestand van ', aanbieder, ' zitten leerlingen die niet in onderdeelbestand ', onderdeel_file, ' zitten, namelijk:\n', paste(missing_leerlingen, collapse = '\n')))
    }

    # Maak long-format van wide-format data
    if (!'item_id' %in% colnames(score_data)) {
      score_data = reshape2::melt(score_data, id.vars = 'person_id', value.name = 'item_score')
      colnames(score_data)[colnames(score_data) == 'variable'] = 'item_id'
    }

    # Verwijder missings
    score_data = score_data[!is.na(score_data$item_score) & score_data$item_score != 9, ]

    # Kijk of item-persoon combinaties uniek zijn
    if (nrow(unique(score_data[, c('person_id', 'item_id')])) != nrow(score_data[, c('person_id', 'item_id')])) {
      warning(paste0('In onderdeelbestand ', onderdeel_file, ' zijn combinaties van person_id en item_id die niet uniek zijn.'))
    }

    # Bereken observaties p-waarden per item
    p_values = plyr::ddply(score_data, 'item_id', function(x) {
      data.frame('n_data' = nrow(x), 'p_data' = mean(x$item_score))
    })

    # Controleer op aanwezigheid ankeritems
    if (onderdeel %in% ref_onderdelen) {
      onderdeelankers = anker_items[anker_items[, 'onderdeel'] == onderdeel, 'item_id']
      if (length(onderdeelankers[onderdeelankers %in% p_values$item_id]) == 0) {
        warning(paste0('Onderdeel ', onderdeel, ' bij ', aanbieder, ' bevat geen gezamenlijk ankeritems.'))
      }
    }

    p_values$onderdeel = onderdeel
    p_values_data[[onderdeel]] = p_values
  }

  p_values_data = dplyr::bind_rows(p_values_data)

  # Lees controlebestand in
  p_values = read.csv2(file.path(data_folder, controle_files[grepl(aanbieder, controle_files)]))

  # Zijn de juiste kolommen aanwezig in leerlingenbestand?
  if (!all(c('item_id', 'onderdeel', 'n', 'p_waarde') %in% colnames(p_values))) {
    warning(paste0('Niet alle verplichte kolomnamen zijn aanwezig in controlebestand ', aanbieder, '. Aanwezig zijn: ', paste(colnames(p_values), collapse = ', ')))
  }

  p_values = dplyr::left_join(p_values_data, p_values, by = c('item_id', 'onderdeel'))

  # Kijk of er p-waarden ontbreken in het controlebestand
  p_missing = p_values[is.na(p_values[, 'n']), 'item_id']
  if (length(p_missing) > 0) {
    warning(paste0('De volgende items zijn wel aanwezig in de data maar ontbreken in het controlebestand:\n', paste(p_missing, collapse = ', ')))
  }

  # Kijk of aantal observaties in controlebestand overeen komt
  n_mismatch = p_values[!p_values[, 'item_id'] %in% p_missing & p_values[, 'n'] != p_values[, 'n_data'], c('item_id', 'n', 'n_data')]
  if (nrow(n_mismatch) > 0) {
    warning('Er waren items met een mismatch in het aantal observaties in het controlebestand en in de data, namelijk:\n', paste(capture.output(print(n_mismatch, row.names = FALSE)), collapse = '\n'))
  }

  # Kijk of p-waarden in controlebestand overeen komen
  p_diff = p_values[!p_values[, 'item_id'] %in% p_missing & abs(p_values[, 'p_waarde'] - p_values[, 'p_data']) > 0.002, c('item_id', 'p_waarde', 'p_data')]
  if (nrow(p_diff) > 0) {
    warning('Er waren items met een verschil in p-waarde in het controlebestand en in de data, namelijk:\n', paste(capture.output(print(p_diff, row.names = FALSE)), collapse = '\n'))
  }

  # Schrijf PCET-waarden weg voor IRT2-normering
  if (grepl('PCET', aanbieder)) {

    kolommen_pcet = c('toetsadvies', 'standaardscore', 'LEZEN1F', 'LEZEN2F', 'TAAL1F', 'TAAL2F', 'REKENEN1F', 'REKENEN1S')

    # Controleer aanwezigheid kolommen speciaal voor PCET
    if (!all(c(kolommen_pcet) %in% colnames(leerlingen))) {
      warning(paste0('Niet alle speciale kolomnamen zijn aanwezig in leerlingbestand PCET. Aanwezig zijn: ', paste(colnames(leerlingen), collapse = ', ')))
    }

    # Alleen leerlingen met schooladvies
    irt2_ref = leerlingen[leerlingen[, 'schooladvies'] %in% 1:10 & leerlingen[, 'schooltype'] == 1, c('schooladvies', kolommen_pcet)]

    # Controleer of waarden kloppen?
    if (!all(1:10 %in% irt2_ref$schooladvies)) {
      warning('Niet alle schooladviezen kwamen voor in het leerlingbestand van de PCET.')
    }
    if (!all(1:6 %in% irt2_ref$toetsadvies)) {
      warning('Niet alle toetsadviezen kwamen voor onder leerlingen die een schooladvies hebben in de PCET.')
    }
    for (ref_kolom in kolommen_pcet[!kolommen_pcet %in% c('standaardscore', 'toetsadvies')]) {
      if (!all(c(0,1) %in% irt2_ref[, ref_kolom])) {
        warning(paste0('Niet alle waarden in de kolom ', ref_kolom, ' in het leerlingbestand van de PCET hebben waarde 0 of 1.'))
      }
    }

    write.csv2(irt2_ref, 'pcet_irt2.csv', quote = FALSE, row.names = FALSE)
  }
}
