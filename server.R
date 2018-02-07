#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 30 * 1024 ^ 2, readr.show_progress = FALSE,
        shiny.trace=FALSE)
        # shiny.error = browser,
        # shiny.reactlog=TRUE)

source('date_formats.R')

stringsAsFactors <- function (dt) {
  for (c in colnames(dt)) {
    dt[is.na(get(c)), (c) := 'NULL']
  }

  dt <- dt[, lapply(.SD, function(x) {
    if (!is.character(x)) {
      x
    } else if (all(grepl(x, pattern='^\\d+$'))) {
      as.numeric(x)
    } else if (all(grepl(x, pattern='^\\d+.*%.*$'))) {
      as.numeric(sub(x, pattern='%', replacement = '', fixed=T)) / 100
    } else {
      factor(x)
    }
  })]

  return(dt)
}

# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(plotlist, ncol, nrow, title=NULL,
                                       position = c("bottom", "right")) {

  if (missing(ncol)) { ncol <- length(plotlist) }
  if (missing(nrow)) { nrow = ceiling(length(plotlist)/ncol) }

  # plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plotlist[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) {x$name}) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plotlist, function(x) {
                x + theme(legend.position = "none",
                          plot.title = element_blank(),
                          plot.subtitle = element_blank(),
                          plot.caption = element_blank())
                })
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(
    position,
    "bottom" = arrangeGrob(top=textGrob(title, just='center'),
      do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    ),
    "right" = arrangeGrob(top = title, bottom = caption,
      do.call(arrangeGrob, gl),
      legend,
      ncol = 2,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth)
    )
  )

  grid.newpage()
  grid.draw(combined)

  # return gtable invisibly
  invisible(combined)

}

# https://stackoverflow.com/a/8197703
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

function(input, output, session) {
  my <- reactiveValues(dataset_original=NULL, dataset=NULL, dataset_grouped=NULL, theme=NULL)

  observe({

    req(input$presetDataset)

    # print('Entered origin block')

    dataset <- NULL
    if (input$presetDataset == 'Game Ratings') {
      dataset <- readRDS('ign.RDS')
    } else if (input$presetDataset == 'Border Patrol Apprehensions') {
      dataset <- readRDS('border.RDS')
    } else if (input$presetDataset == 'College Scorecards') {
      dataset <- readRDS('college.RDS')
    } else if (input$presetDataset == 'Starbucks Nutrition') {
      dataset <- readRDS('starbucks.RDS')
    }

    my$dataset_original <- dataset
    my$dataset <- dataset

    updateTextInput(session, 'title', value=input$presetDataset)
  })

  observeEvent(input$userFile, {

    file_ext <- toupper(regmatches(input$userFile$name, regexpr(input$userFile$name, pattern='\\.(.+)$')))

    ext_i <- match(file_ext, c('.CSV', '.XLS', '.XLSX', '.RDS'))

    if (!is.na(ext_i)) {
      if (ext_i > 3) {
        my$dataset_original <- readRDS(input$userFile$datapath) %>% as.data.table() # %>% stringsAsFactors()
      } else {
        read_FUN <- if (ext_i == 1) { read_csv } else { read_excel }

        my$dataset_original <- read_FUN(input$userFile$datapath, col_names = TRUE,
                                        trim_ws = FALSE, guess_max = 1000, na=c('', '-', 'NA', 'NULL')) %>%
                                as.data.table() %>% stringsAsFactors()

      }

      my$dataset <- my$dataset_original

      updateSelectizeInput(session, 'presetDataset', selected=NA)

      updateTextInput(session, 'title',
                      value=tools::toTitleCase(sub(tolower(input$userFile$name), pattern = tolower(file_ext), replacement='', fixed=T)))
    }
  })

  observeEvent(input$startOver, {
    my$dataset <- my$dataset_original
  })

  observe({
    updateSelectizeInput(session, 'userFileIdCols', choices = colnames(my$dataset), selected=isolate(input$userFileIdCols))
    updateSelectizeInput(session, 'columnsToCombine', choices = colnames(my$dataset), selected=isolate(input$columnsToCombine))
    updateSelectizeInput(session, 'columnToSplit', choices = colnames(my$dataset), selected=isolate(input$columnToSplit))
  })

  observeEvent(input$meltData, {

    # print('Entered melt block')

    if (length(input$userFileIdCols) > 0) {

      var_name <- 'feature'
      if (nchar(input$meltedKeyName) > 0) { var_name <- input$meltedKeyName }

      value_name <- 'value'
      if (nchar(input$meltedValueName) > 0) { value_name <- input$meltedValueName }

      dataset <- my$dataset

      id_vars <- input$userFileIdCols

      num_cols_TF <- sapply(dataset, is.numeric)
      num_cols <- colnames(dataset)[num_cols_TF] %>% .[!(. %in% id_vars)]
      char_cols <- colnames(dataset) %>% .[!(. %in% id_vars) & !(. %in% num_cols)]

      count_char_cols <- FALSE

      if (length(char_cols) > 0) { count_char_cols <- TRUE }

      if (count_char_cols && length(num_cols) > 0 &&
          nrow(dataset[, id_vars, with=F] %>% unique()) < nrow(dataset)) {
        output$sourceDatasetMessage <- renderText({
          'Non-distinct ID columns specified; summed numeric values.'
        })
      } else {
        output$sourceDatasetMessage <- renderText({''})
      }

      if (count_char_cols) {
        dataset <- dataset[, c(list('# group obs.' = .N), lapply(.SD, function(x) {
                      if (is.numeric(x)) { sum(x) } else { uniqueN(x) }
                    })), by=id_vars]

        setnames(dataset, old=char_cols, new=paste('#', char_cols))
        if (length(num_cols) > 0) {
          setnames(dataset, old=num_cols, new=paste('Total', num_cols))
        }

        my$dataset <- dataset
      } else {
        updateSelectizeInput(session, 'userFileIdCols', selected=NA)
        updateTextInput(session, inputId = 'meltedKeyName', value = NA)
        updateTextInput(session, inputId = 'meltedValueName', value = NA)

        my$dataset <- melt(dataset,
                           id.vars = id_vars,
                           variable.name = var_name,
                           value.name = value_name,
                           verbose = FALSE)
      }
    }
  })

  observeEvent(input$combineCols, {

    req(input$columnsToCombine)

    # print('Entered combine block')

    updateSelectizeInput(session, 'columnsToCombine', selected=NA)
    updateTextInput(session, 'combinedColName', value = NA)
    updateTextInput(session, 'sepChar', value = NA)

    dataset <- copy(my$dataset)

    col_name <- input$combinedColName

    if (nchar(col_name) > 0) {
      new_col_names <- strsplit(col_name, split=',\\s*')[[1]]

      if (length(new_col_names) == length(input$columnsToCombine)) {
        ren_cols <- duplicated(new_col_names)
        new_col_names[ren_cols] <- paste0(new_col_names[ren_cols], '.1')
        ren_cols <- new_col_names %in% colnames(dataset)
        new_col_names[ren_cols] <- paste0(new_col_names[ren_cols], '.1')
        setnames(dataset, old=input$columnsToCombine, new=new_col_names)
      } else {
        if (col_name %in% colnames(dataset)) { col_name <- paste0(col_name, '.1') }

        sep_char <- ''
        if (nchar(input$sepChar) > 0) {
          sep_char <- input$sepChar
        }

        dataset[[col_name]] <- dataset[, do.call(paste, c(.SD, sep=sep_char)), .SDcols=c(input$columnsToCombine)]
      }
    }

    if (!input$keepCombinedCols) {
      dataset[, c(input$columnsToCombine) := NULL]
    }

    my$dataset <- dataset %>% stringsAsFactors()
  })

  observeEvent(input$separateCols, {
    req(input$columnToSplit)

    dataset <- my$dataset

    split_regex <- input$splitChar
    if (nchar(input$splitChar) == 0) {
      if (grepl(dataset[[input$columnToSplit]][1], pattern='^\\d+')) {
        split_regex <- '\\D+' # if starts with num, split on all non-num.
      } else {
        split_regex <- '[[:punct:]]+|[[:space:]]+'
      }
    }

    split_cells <- strsplit(as.character(dataset[[input$columnToSplit]]), split=split_regex)

    if (length(split_cells) > 0) {
      n_cols <- max(sapply(split_cells, length))
      col_names <- NULL # paste0('V', 1:n_cols)
      for (a in LETTERS) {
        col_names <- paste0(a, 1:n_cols)
        if (!any(col_names %in% colnames(dataset))) {
          break
        }
      }
      dataset <- copy(my$dataset)
      dataset[, c(col_names) := transpose(split_cells)]
      if (!input$keepSplitCols) { dataset[, c(input$columnToSplit) := NULL] }
      my$dataset <- dataset %>% stringsAsFactors()

      updateSelectizeInput(session, 'columnToSplit', selected=NA)
      updateTextInput(session, 'splitChar', value = NA)
    }
  })

  my$dataset_filtered <- reactive({
    req(!is.null(my$dataset),
        class(try(my$dataset,silent=T)) != 'try-error',
        input$dataset_rows_all,
        length(input$dataset_rows_all) <= nrow(my$dataset),
        !all(c(input$dataset_search, input$dataset_search_columns)=='') ||
          length(input$dataset_rows_all) == nrow(my$dataset))

    return(my$dataset[input$dataset_rows_all,])
  })

  setComboBox <- function(session, column, col_names) {
    if (!is.null(input[[column]]) && input[[column]] %in% col_names) {
      updateSelectizeInput(session, column, choices = col_names, selected=input[[column]])
    } else {
      updateSelectizeInput(session, column, choices = col_names)
    }
  }

  observe({
    req(my$dataset)

    # print('Populating column lists')

    col_names <- colnames(my$dataset)

    isolate({
      setComboBox(session, 'dateCol', col_names)

      setComboBox(session, 'groupCol', col_names)

      setComboBox(session, 'featureCol', col_names)

      num_col_names <- col_names[my$dataset[, lapply(.SD, class)] %in% c('numeric', 'integer', 'double')]

      setComboBox(session, 'valueCol', num_col_names)
    })
  })

  observe({
    req(is.null(input$dateCol))

    updateSelectizeInput(session, 'dateFormat', selected = NA)
    updateSelectizeInput(session, 'dateTransform', selected = NA)
  })

  observe({
    req(!is.null(my$dataset_filtered()),
        # class(try(my$dataset_filtered(),silent=T)) != 'try-error',
        !is.null(input$dateCol))

    dates <- as.character(my$dataset_filtered()[[input$dateCol]][1:100])

    # http://gamon.webfactional.com/regexnumericrangegenerator/
    patterns <- c('Year'='^\\d{3,4}$',
                  'Month'='^0*([1-9]|1[0-2])$',
                  'Day'='^0*([0-9]|[12][0-9]|3[01])$',
                  'Day of Year'='^0*([0-9]|[1-8][0-9]|9[0-9]|[12][0-9]{2}|3[0-5][0-9]|36[0-6])$',
                  'ymd_hms'='^\\d{4}.*\\d{1,2}.*\\d{1,2}\\s.+$',
                  'ymd'='^\\d{4}.*\\d{1,2}.*\\d{1,2}',
                  'mdy_hms'='^\\d{1,2}.*\\d{1,2}.*\\d{4}\\s.+$',
                  'mdy'='^\\d{1,2}.*\\d{1,2}.*\\d{4}',
                  'Year-Month'='^\\d{4}.*\\d{1,2}$',
                  'Hour:Minute'='^\\d{1,2}:\\d{2}$',
                  'Hour:Minute:Second'='^\\d{,2}:\\d{2}:\\d{2}$')

    format_match <- ' - Guess - '

    for (f in names(patterns)) {
      if (all(grepl(dates, pattern=patterns[f]))) {
        format_match <- f
        break
      }
    }

    isolate({
      if (!is.null(format_match) && (is.null(input$dateFormat) || input$dateFormat != format_match)) {
        updateSelectizeInput(session, 'dateFormat', selected = format_match)
      }
    })
  })

  observe({
    req(input$dateFormat)

    if (is.null(isolate(input$dateTransform)) || input$dateFormat != isolate(input$dateTransform)) {
      updateSelectizeInput(session, 'dateTransform', selected = input$dateFormat)
    }
  })

  observe({
    req(!is.null(my$dataset_filtered()))
        # class(try(my$dataset_filtered(),silent=T)) != 'try-error')

    if (is.null(input$valueCol)) {
      updateRadioButtons(session, 'plotValues', selected = '# observations')
    } else {
      updateRadioButtons(session, 'plotValues', selected = 'values')
    }
  })

  get_selectize_options <- function(all_options, selected_options) {
    if (length(all_options) > 0 &&
        between(length(selected_options), 0,
                length(all_options),
                incbounds = FALSE)) {
      return(selected_options) # return current selection
    } else {
      return(NA) # returns NA if n_selected is 0 or all.
    }
  }

  decimalPrecision <- reactive({
    if (is.na(input$decimalPrecision)) {
      return(2L)
    }
    return(as.integer(input$decimalPrecision))
  })

  newCols <- reactiveValues(date=NULL, group=NULL, value=NULL)

  my$dataset_grouped <- reactive({

    req(!is.null(my$dataset_filtered()),
        all(is.null(c(input$dateCol, input$dateFormat))) ||
        length(c(input$dateCol, input$dateFormat))==2)

    dataset <- copy(my$dataset_filtered())

    # print('Entered dates_parsed block')

    date_col <- input$dateCol
    group_col <- input$groupCol
    feature_col <- input$featureCol
    value_col <- input$valueCol

    if (is.null(date_col) || is.null(input$dateFormat)) {
      date_col <- NULL
    } else {
      if (input$dateFormat == ' - Guess - ') {
        dataset[, c(date_col) := anytime(dataset[[date_col]])]
      } else {
        date_format_str <- NULL
        date_format_str <- sapply(date_format_list, extract, input$dateFormat) %>% .[!is.na(.)]
        dataset[, c(date_col) := parse_date_time(dataset[[date_col]], orders = date_format_str)]
      }

      if (anyNA(dataset[[date_col]])) {
        dataset[, c(date_col) := NULL]
        if (!is.null(group_col) && group_col == date_col) { group_col <- NULL }
        if (!is.null(feature_col) && feature_col == date_col) { feature_col <- NULL }
        date_col <- NULL
      }
    }

    # print('Entered date_filtered block')

    if (!is.null(isolate(date_col))) {
      if (sum(!is.na(input$dateRange)) == 2) {
        dataset <- dataset[between(get(isolate(date_col)), ymd(input$dateRange[1]), ymd(input$dateRange[2]), incbounds = T), ]
      } else if (!is.na(input$dateRange[1])) {
        dataset <- dataset[get(isolate(date_col)) >= ymd(input$dateRange[1]), ]
      } else if (!is.na(input$dateRange[2])) {
        dataset <- dataset[get(isolate(date_col)) <= ymd(input$dateRange[2]), ]
      }

      # print('Entered days_of_week block')

      if (between(length(input$daysOfWeek), 0, 7, incbounds = F)) {
        dataset <- dataset[weekdays(get(date_col)) %in% input$daysOfWeek,]
      }
    }

    # print('Entered dates_transformed block')

    if (!is.null(isolate(date_col)) && !is.null(input$dateTransform)) {
      date_trans_str <- sapply(date_trans_list, extract, input$dateTransform) %>% .[!is.na(.)]

      if (grepl(date_trans_str, pattern = '%', fixed = TRUE)) {
        dataset[, c(date_col) := format(dataset[[date_col]], date_trans_str)]
        if (all(grepl(dataset[[date_col]], pattern='^\\d+$'))) {
          dataset[, c(date_col) := factor(as.integer(dataset[[date_col]]), ordered = TRUE)] # for week of year
        }
      } else {
        date_trans_FUN <- match.fun(date_trans_str)
        if (input$dateTransform =='Year-Quarter') {
          dataset[, c(date_col) := factor(date_trans_FUN(dataset[[date_col]], with_year=TRUE), ordered=TRUE)]
        } else if (is.null(as.list(args(date_trans_FUN))[['label']])) {
          dataset[, c(date_col) := factor(date_trans_FUN(dataset[[date_col]]), ordered=TRUE)]
        } else {
          dataset[, c(date_col) := factor(date_trans_FUN(dataset[[date_col]], label = TRUE), ordered=TRUE)]
        }
      }

      if (grepl(names(date_trans_str), pattern='Parts', fixed=T)) {
        setnames(dataset, old=date_col, new=input$dateTransform)
        if (!is.null(group_col) && group_col == date_col) { group_col <- input$dateTransform }
        if (!is.null(feature_col) && feature_col == date_col) { feature_col <- input$dateTransform }
        date_col <- input$dateTransform
      }
    }

    # print('Entered agg_within block')

    grouping <- c(date_col, group_col, feature_col)

    has_obs_col <- FALSE # !is.null(dataset[['# observations']])

    if (is.null(value_col)) {
      if (has_obs_col) {
        dataset <- dataset[, .('value'=NA, '# observations'=sum(get('# observations'))), by = grouping]
      } else {
        dataset <- dataset[, .('value'=NA, '# observations'=.N), by = grouping]
      }
    } else {
      if (!is.numeric(dataset[[value_col]])) {
        dataset[, c(value_col) := as.numeric(gsub(get(value_col), pattern = ',', replacement = '', fixed=T))]
      }

      if (is.null(input$aggWithinFUN)) {
        grouping <- c(grouping, value_col)
        if (!is.null(grouping)) {
          dataset <- dataset[, grouping, with=FALSE]
          setnames(dataset, old=value_col, new='value')
        }

        if (!has_obs_col) {
          dataset[, '# observations' := 1]
        }
      } else {

        aggWithinFUN <- match.fun(input$aggWithinFUN)

        if (has_obs_col) {
          dataset <- dataset[, .('value'=aggWithinFUN(get(value_col), na.rm = T),
                                 '# observations'=sum(get('# observations'))), by = grouping]
        } else {
          dataset <- dataset[, .('value'=aggWithinFUN(get(value_col), na.rm = T),
                                 '# observations'=.N), by = grouping]
        }
      }
    }

    # print('Entered agg_between block')

    if (!is.null(value_col) && sum(dataset[['# observations']]) > nrow(dataset)) {
      value_col <- paste(input$aggWithinFUN, value_col, sep = ' ')
    }

    if (!is.null(group_col) && !is.null(input$aggBetweenFUN)) {

      aggBetweenFUN <- match.fun(input$aggBetweenFUN)

      if (identical(aggBetweenFUN, weighted.mean)) {
        dataset <- dataset[, .('value'=weighted.mean(value, w = `# observations`, na.rm = T),
                              'n_groups'=length(unique(get(group_col))), '# observations'=sum(`# observations`)),
                            by = c(date_col, feature_col)]
      } else {
        dataset <-
          dataset[, .('value'=aggBetweenFUN(value, na.rm = T),
                    'n_groups'=length(unique(get(group_col))), '# observations'=sum(`# observations`)),
                  by = c(date_col, feature_col)]
      }

      n_group_col <- paste0('# ', group_col)
      setnames(dataset, old='n_groups', new=n_group_col)

      group_col <- NULL

      if (!is.null(value_col)) {
        value_col <- paste(input$aggBetweenFUN, value_col, sep=' of ')
      }
    }

    if (!is.null(value_col)) {
      setnames(dataset, 'value', value_col)
    } else {
      dataset[, value := NULL]
    }

    # print('Entered rounded_values_filtered block')

    if (!is.null(value_col)) {
      dataset[, c(value_col) := round(dataset[[value_col]], decimalPrecision())]
    }

    if (!is.null(value_col)) {
      value_col <- value_col
    } else if ('# observations' %in% colnames(dataset)) {
      value_col <- '# observations'
    }

    if (!is.null(group_col)) {
      if (!is.factor(dataset[[group_col]])) { # must be int, dbl, or num.
        dataset[, c(group_col) := factor(dataset[[group_col]], ordered = T)]
      } else {
        group_totals <- dataset[, sum(abs(get(value_col)), na.rm=T), by=c(group_col)]
        setorder(group_totals, -V1)
        dataset[, c(group_col) := factor(dataset[[group_col]], levels = group_totals[[group_col]])]
      }
    }

    if (!is.null(feature_col)) {
      if (!is.factor(dataset[[feature_col]])) { # must be int, dbl, or num.
        dataset[, c(feature_col) := factor(dataset[[feature_col]], ordered = T)]
      } else {
        feature_totals <- dataset[, sum(abs(get(value_col)), na.rm=T), by=c(feature_col)]
        setorder(feature_totals, -V1)
        dataset[, c(feature_col) := factor(dataset[[feature_col]], levels = feature_totals[[feature_col]])]
      }
    }

    # print('Entered topN block')

    if (!is.null(group_col) &&
        between(topN$groups(), 0, length(levels(dataset[[group_col]])), incbounds = F)) {
      dataset <- dataset[dataset[[group_col]] %in% (levels(dataset[[group_col]])[1:topN$groups()]),]
    }

    if (!is.null(feature_col) &&
        between(topN$features(), 0, length(levels(dataset[[feature_col]])), incbounds = F)) {
      dataset <- dataset[dataset[[feature_col]] %in% (levels(dataset[[feature_col]])[1:topN$features()]),]
    }

    setorderv(dataset, cols=c(date_col, group_col, feature_col), order = 1)

    isolate({
      newCols$date <- date_col
      newCols$group <- group_col
      newCols$feature <- feature_col
      newCols$value <- value_col
    })

    return(dataset)
  })

  topN <- reactiveValues(groups=0, features=0)

  topN$groups <- reactive({
    # print('Getting topN group value')
    if (is.na(input$nGroups)) {
      Inf
    } else {
      return(as.integer(input$nGroups))
    }
  })

  topN$features <- reactive({
    # print('Getting topN feature value')
    if (is.na(input$nFeatures)) {
      Inf
    } else {
      return(as.integer(input$nFeatures))
    }
  })

  observe({
    req(!is.null(my$dataset_filtered()),
        input$groupCol)

    n_Groups <- length(unique(my$dataset_filtered()[[input$groupCol]]))

    updateSliderInput(session, 'nGroups', max=n_Groups, value=n_Groups)
  })

  observe({
    req(!is.null(my$dataset_filtered()),
        input$featureCol)

    n_Features <- length(unique(my$dataset_filtered()[[input$featureCol]]))

    updateSliderInput(session, 'nFeatures', max=n_Features, value=n_Features)
  })

  output$dataset <- DT::renderDataTable(server=TRUE, options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15), select=FALSE),
                                        filter='top', { my$dataset })

  output$dataset_grouped <- DT::renderDataTable(server=TRUE, # options = list(pageLength = 18, lengthChange=FALSE, select=FALSE),
                                              options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15), select=FALSE),
                                              filter='top', { my$dataset_grouped() })

  my$dataset_p <- reactive({
    req(!is.null(my$dataset_grouped()),
        input$dataset_grouped_rows_all,
        length(input$dataset_grouped_rows_all) <= nrow(my$dataset_grouped()),
        !all(c(input$dataset_grouped_search, input$dataset_grouped_search_columns)=='') ||
        length(input$dataset_grouped_rows_all) == nrow(my$dataset_grouped()))

    dataset <- my$dataset_grouped()[input$dataset_grouped_rows_all, ]

    return(dataset[, lapply(.SD, function(x) { if (is.factor(x)) { factor(x) } else { x } })])
  })

  # columns specific to plots.
  pcols <- reactiveValues(groupCol=NULL, featureCol=NULL, valueCol=NULL)

  observe({
    pcols$group <- newCols$group
    pcols$feature <- newCols$feature

    if (params$swap) {
      isolate({
        tmp <- pcols$group
        pcols$group <- pcols$feature
        pcols$feature <- tmp
      })
    }
  })

  observe({
    pcols$value <- newCols$value

    if (is.null(newCols$value) || input$plotValues == '# observations') {
      pcols$value <- '# observations'
    }
  })

  params <- reactiveValues(swap=FALSE, facetFeatures=FALSE, rotate=FALSE, fixLimsX=FALSE, fixLimsY=FALSE, showLabels=FALSE)

  observe({
    for (param in names(isolate(reactiveValuesToList(params)))) {
      params[[param]] <- param %in% input$plotParams
    }
  })

  observeEvent(params$rotate, {
    fix_y <- params$fixLimsX
    fix_x <- params$fixLimsY
    if (fix_x != fix_y) {
      selected <- input$plotParams

      if (!fix_x) { selected <- selected[selected != 'fixLimsX'] }
      else { selected <- c(selected, 'fixLimsX')  }

      if (!fix_y) { selected <- selected[selected != 'fixLimsY'] }
      else { selected <- c(selected, 'fixLimsY')  }

      updateCheckboxGroupInput(session, 'plotParams', selected = selected)
    }
  })

  toName <- function(text, envir = parent.frame()) {
    if (is.null(text)) {
      return('')
    }
    return(eval(sym(text), envir = envir))
  }

  plot <- reactiveValues(trends=NULL, distribution=NULL)

  n <- reactiveValues(dates=0, groups=0, features=0)

  n$dates <- reactive({
    req(!is.null(my$dataset_p()))

    if (is.null(newCols$date)) {
      return(0)
    } else {
      return(length(unique(my$dataset_p()[[newCols$date]])))
    }
  })

  n$groups <- reactive({
    req(!is.null(my$dataset_p()))

    if (is.null(pcols$group)) {
      return(0)
    } else {
      return(length(unique(my$dataset_p()[[pcols$group]])))
    }
  })

  n$features <- reactive({
    req(!is.null(my$dataset_p()))

    if (is.null(pcols$feature)) {
      return(0)
    } else {
      return(length(unique(my$dataset_p()[[pcols$feature]])))
    }
  })

  output$countPlotMessage <- renderText({
    req(my$dataset_p())

    if (n$groups() > 51) {
      return('Max of 51 groups exceeded.')
    } else if (n$features() > 52) {
      return('Max of 52 features exceeeded.')
    } else if (n$dates() > 1) {
      return('Summed out date column.')
    } else if (is.null(input$aggWithinFUN) && sum(my$dataset_p()[['# observations']], na.rm=T) == nrow(my$dataset_p())) {
      return('Summed within-groups.')
    }
  })

  plot$counts <- reactive({
    req(my$dataset_p())

    # print('Entered counts_plot block')

    dataset <- copy(my$dataset_p())

    date_col <- newCols$date
    group_col <- pcols$group
    feature_col <- pcols$feature
    value_col <- pcols$value

    if (nrow(dataset) > 1 && !is.null(value_col) && all(is.null(c(date_col, group_col, feature_col)))) {
      # do this to prevent several unnecessary layers plotted atop one another
      dataset <- dataset[, .(value=sum(get(value_col))), by=NULL]
      value_col <- paste0('Sum of ', value_col)
      setnames(dataset, old='value', new=value_col)
    }

    if (n$dates() > 1 || (is.null(isolate(input$aggWithinFUN)) && sum(dataset[['# observations']], na.rm=T) == nrow(dataset))) { # contains dupes
      grouping <- c(group_col, feature_col)
      dataset <- dataset[, .('value'=sum(get(value_col), na.rm = T)), by = grouping]
      value_col <- paste0('Sum of ', value_col)
      setnames(dataset, old='value', new=value_col)
      y_lab <- value_col
    }

    y_format <- scales::comma
    y_lab <- value_col
    x_lab <- NULL

    if (input$countValuesAs == '% of Grand Total') {
      dataset[, c(value_col) := list(get(value_col)/sum(get(value_col)))]
      y_lab <- paste(y_lab, paste0('(', input$countValuesAs, ')'))
      y_format <- scales::percent
    } else if (input$countValuesAs == '% of Group Total' || input$countGeom == 'Pie') {
      if (is.null(group_col)) {
        dataset[, c(value_col) := list(get(value_col)/sum(get(value_col)))]
      } else {
        dataset[, c(value_col) := as.numeric(get(value_col))]
        dataset[, c(value_col) := list(get(value_col)/sum(get(value_col))), by=c(group_col)]
      }
      y_lab <- paste(y_lab, '(% of Group Total)')
      y_format <- scales::percent
    }

    no_legend <- FALSE

    if (params$showLabels) {
      dataset[, Label := y_format(get(value_col))]
    }

    if (input$countGeom == 'Bars') {
      p <- ggplot(dataset, aes(x=toName(feature_col), y=toName(value_col),
                               group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col))) +
        geom_bar(stat='identity', position=position_dodge(width=1), na.rm = T)
      # geom_label(aes(label=Label), color='white', alpha=0.9, vjust=0.5, hjust=0.5, na.rm=T, show.legend=F)

      no_legend <- TRUE

      if (params$showLabels) {
        p <- p + geom_label(aes(label=Label), color='white', alpha=1.0, vjust=0.5, hjust=0.5, na.rm=T, show.legend=F,
                            position = p$layers[[1]]$position)
      }

      if (params$rotate) { # && ! params$facetFeatures) {
        p <- p + scale_x_discrete(limits = factor(rev(levels(dataset[[feature_col]]))))
      }
    } else {
      x_text <- NULL # feature_col
      if (is.null(x_text)) { x_text <- '' }
      # Bars stacked
      p <- ggplot(dataset, aes(x=x_text, y=toName(value_col),
                               group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col))) +
              geom_bar(stat='identity', width=1, position=position_stack(reverse=T), na.rm = T)

      if (input$countGeom == 'Pie') {
        p <- p + coord_polar('y', start=0)
        # y_lab <- NULL
      }

      if (params$showLabels) {
        # https://stackoverflow.com/a/14944691
        dataset[, pos := Reduce('+', list(get(value_col)/2,cumsum(c(0,head(get(value_col),-1))))), by=group_col]


        p <- p + geom_label(aes(label=Label, y=pos), color='white', alpha=1.0, vjust=0.5,
                           hjust=0.5, na.rm=T, show.legend=F) # position = p$layers[[1]]$position)
      }
    }

    return(facet_features(p, x_lab=x_lab, y_lab=y_lab, x_format=NULL, y_format=y_format, no_legend = no_legend))
  })

  output$count_plot <- renderPlot({
    req(!is.null(my$dataset_p()),
        n$groups() <= 51, n$features() <= 52)

    return(postProcessPlot(plot$counts()))
  })

  output$trendPlotMessage <- renderText({
    if (is.null(newCols$date)) {
      return('Specify a date column.')
    } else if (n$groups() > 51) {
      return('Max of 51 groups exceeded.')
    } else if (n$features() > 52) {
      return('Max of 52 features exceeeded.')
    } else {
      return('')
    }
  })

  plot$trends <- reactive({

    # print('Entered trends_plot block')

    dataset <- copy(my$dataset_p())

    isolate({ date_col <- newCols$date })

    group_col <- pcols$group
    feature_col <- pcols$feature
    value_col <- pcols$value

    if (nrow(dataset) > 1 && !is.null(value_col) && all(is.null(c(date_col, group_col, feature_col)))) {
      # do this to prevent several unnecessary layers plotted atop one another
      dataset <- dataset[, .(value=round(mean(get(value_col)), isolate(decimalPrecision()))), by=NULL]
      value_col <- paste0('mean ', value_col)
      setnames(dataset, old='value', new=value_col)
    }

    p <- NULL
    x_lab <- date_col
    y_lab <- value_col
    y_format <- scales::comma
    x_format <- NULL

    if (input$trendValuesAs == '% of Grand Total') {
      dataset[, c(value_col) := list(get(value_col)/sum(get(value_col)))]
      y_lab <- paste(y_lab, paste0('(', input$trendValuesAs, ')'))
      y_format <- scales::percent
    } else if (input$trendValuesAs == '% of Group Total') {
      if (is.null(group_col)) {
        dataset[, c(value_col) := list(get(value_col)/sum(get(value_col)))]
      } else {
        dataset[, c(value_col) := as.numeric(get(value_col))]
        dataset[, c(value_col) := list(get(value_col)/sum(get(value_col))), by=c(group_col)]
      }
      y_lab <- paste(y_lab, paste0('(', input$trendValuesAs, ')'))
      y_format <- scales::percent
    }

      n_dateSplits <- 31
      n_dateLabels <- 12

      isolate({
        if (params$facetFeatures) {
          n_cols <- n$groups()
        } else {
          n_cols <- ceiling(n$groups() / 3)
        }
      })

      if (n_cols >= 18) {
        n_dateSplits <- 2
      } else if (n_cols >= 9) {
        n_dateSplits <- 3
      } else if (n_cols >= 3) {
        n_dateSplits <- 4
      }

      n_dateSplits <- min(n$dates(), n_dateSplits)
      n_dateLabels <- min(n$dates(), n_dateSplits, n_dateLabels)

      p <- ggplot(dataset, aes(x=toName(date_col), y=toName(value_col),
                               group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col)))

      showLabels <- params$showLabels

      if (is.null(isolate(input$aggWithinFUN)) && sum(dataset[['# observations']], na.rm=T) == nrow(dataset)) { # contains dupes

      if (input$trendGeom == 'Lines') {
        p <- p + geom_point(size=1, alpha=0.5) + geom_smooth(stat='smooth', method = 'loess', se = F, na.rm = T)
        showLabels <- FALSE
      } else {
        dataset <- dataset[, .('value'=sum(get(value_col), na.rm = T)), by = c(date_col, group_col, feature_col)]
        value_col <- paste0('Sum of ', value_col)
        setnames(dataset, old='value', new=value_col)
        y_lab <- value_col

        p <- p <- ggplot(dataset, aes(x=toName(date_col), y=toName(value_col),
                                      group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col)))

        if (input$trendGeom == 'Bars') {
          p <- p + geom_bar(stat = 'identity', position = position_stack(reverse = TRUE), na.rm = T)  # position = position_fill(reverse = TRUE))
        } else if (input$trendGeom == 'Area') {
          p <- p + geom_area(stat='identity', alpha=1, position = position_identity(), na.rm = T)
        } else if (input$trendGeom == 'Area Stacked') {
          p <- p + geom_area(stat='identity', position = position_stack(reverse = TRUE), na.rm = T)
        }
      }
    } else {
      if (input$trendGeom == 'Lines') {
        p <- p + geom_line(size=1, na.rm = T) + geom_point(size=1, na.rm = T)
      } else if (input$trendGeom == 'Bars') {
        p <- p + geom_bar(stat = 'identity', position = position_stack(reverse = TRUE), na.rm = T)  # position = position_fill(reverse = TRUE))
      } else if (input$trendGeom == 'Area') {
        p <- p + geom_area(alpha=1, position = position_identity(), na.rm = T)
      } else if (input$trendGeom == 'Area Stacked') {
        p <- p + geom_area(position = position_stack(reverse = TRUE), na.rm = T)
      }
    }

    if (is.factor(dataset[[date_col]])) {

      i_splits <- round(seq(from=1, to=length(levels(dataset[[date_col]])), length.out = n_dateSplits))
      date_limits <- levels(dataset[[date_col]])
      date_labels <- date_limits
      date_labels[!(date_labels %in% date_labels[i_splits])] <- ''

      if (params$rotate) { date_limits <- rev(date_limits) }

      p <- p + scale_x_discrete(name=date_col, limits = date_limits, labels = date_labels)

      date_labels <- date_labels[date_labels != '']

    } else {
      date_trans_str <- NULL

      if (is.character(dataset[[date_col]])) {
        date_trans_str <- sapply(date_trans_list, extract, isolate(input$dateTransform)) %>% .[!is.na(.)]
        dataset[, c(date_col) := parse_date_time(get(date_col), orders = date_trans_str)]
      }

      date_labels <- pretty_dates(dataset[[date_col]], n=n_dateSplits)

      scale_x <- scale_x_datetime

      if (is.Date(dataset[[date_col]])) {
        scale_x <- scale_x_date
        date_labels <- as_date(date_labels)
      }

      if (params$rotate) {
        date_labels <- rev(date_labels)
      }

      if (is.null(date_trans_str)) {
        p <- p + scale_x(name=date_col, breaks = date_labels)
      } else {
        p <- p + scale_x(name=date_col, breaks = date_labels, date_labels=date_trans_str)
      }
    }

    if (showLabels) {
      if (length(date_labels) > n_dateLabels) {
        by_n <- floor(length(date_labels) / n_dateLabels)
        date_labels <- date_labels[seq(1, length(date_labels), by=by_n)]
      }
      dataset[, Label := ifelse(dataset[[date_col]] %in% date_labels, y_format(dataset[[value_col]]), NA)]

      label_pos <- position_identity()

      p <- p + geom_label(aes(label=Label), color='white', alpha=1.0,
                          vjust=0.5, hjust=0.5, na.rm=T, show.legend=F,
                          position = p$layers[[1]]$position)
    }

    return(facet_features(p, x_lab=x_lab, y_lab=y_lab, x_format=NULL, y_format=y_format))
  })

  output$trends_plot <- renderPlot({
    req(!is.null(my$dataset_p()),
        !is.null(newCols$date), n$groups() <= 51, n$features() <= 52)

    return(postProcessPlot(plot$trends()))
  })

  output$distPlotMessage <- renderText({
    if (is.null(input$aggWithinFUN)) {
      return('')
    } else {
      return('Consider removing within-groups aggregation.')
    }
  })

  plot$dist <- reactive({

    # print('Entered dist_plot block')

    dataset <- copy(my$dataset_p())

    group_col <- pcols$group
    feature_col <- pcols$feature
    value_col <- pcols$value

    dataset[, Label := as.character(NA)]

    if (params$showLabels) {
      grouping <- c(group_col, feature_col)
      dataset[, ':=' (Mean = round(weighted.mean(get(value_col), w=get('# observations')), digits = decimalPrecision()),
                      SD = round(sd(get(value_col)), digits=decimalPrecision())),
              by=grouping][, c('id', 'AbsErr') := list(1:.N, abs(get(value_col)-Mean))]

      min_ids <- dataset[, .SD[which.min(AbsErr)], by=grouping, .SDcols='id']$id

      dataset[min_ids,]$Label <- paste0('atop(', paste0("mu=='", comma(dataset[min_ids,]$Mean), "', sigma=='", comma(dataset[min_ids,]$SD)), "')")

      dataset[, c('id', 'Mean', 'SD', 'AbsErr') := NULL]
    }

    p <- NULL
    x_lab <- value_col
    y_lab <- NULL

    y_format <- scales::comma
    x_format <- scales::comma

    n_bins <- as.integer(input$bins)
    if (is.na(n_bins) || n_bins <= 0 || n_bins > 100) { n_bins <- 30 }

    bw_adjust <- as.numeric(input$bwadjust)
    if (is.na(bw_adjust) || bw_adjust <= 0 || bw_adjust > 3) { bw_adjust <- 1 }

    if (input$distGeom == 'Histogram') {

      y_lab = 'Binned Frequency'

      p <- ggplot(dataset, aes(x=toName(value_col),
                               group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col))) +
            geom_histogram(aes(y=..count..), bins = n_bins, position = 'identity', color='gray', alpha=1, na.rm = T) +
            geom_label_repel(aes(label=Label), y=0, parse=TRUE, color='white', segment.color = NA, size=4, alpha=1.0, na.rm = TRUE, show.legend=F)

      if (input$showRug) {
        p <- p + geom_rug(aes(y=0), position = position_jitter(height=0), alpha=0.5, show.legend = F, na.rm = T)
      }
    } else if (input$distGeom == 'Density') {

      y_lab = 'Density'
      y_format <- scales::percent

      p <- ggplot(dataset, aes(x=toName(value_col),
                               group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col))) +
            geom_density(alpha=1, adjust=bw_adjust, na.rm = T) +
            geom_label_repel(aes(label=Label), y=0, parse=TRUE, color='white', segment.color = NA, size=4, alpha=1.0, na.rm = TRUE, show.legend=F)

      if (input$showRug) {
        p <- p + geom_rug(aes(y=0), position = position_jitter(height=0), alpha=0.5, show.legend = F, na.rm = T)
      }

    } else {

      x_lab <- NULL
      y_lab <- value_col
      x_format <- NULL

      p <- ggplot(dataset, aes(x=toName(feature_col), y=toName(value_col),
                               group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col)))

      if (input$distGeom == 'Violin') {
        p <- p + geom_violin(position=position_dodge(1), trim = TRUE, scale='count', adjust=bw_adjust, na.rm = T) +
                stat_summary(geom='point', fun.y = mean, size=1, position = position_dodge(1), color='black', show.legend = F, na.rm = T) +
                geom_tufteboxplot(size=1, color='gray90', position=position_dodge(width=1), show.legend=F, na.rm = T)

      } else if (input$distGeom == 'Boxplot') {
        p <- p + geom_boxplot(fill=NA, position = position_dodge(1), na.rm = T)
      }

      p <- p + geom_label(aes(label=Label), position=position_dodge(1), parse = TRUE, color='white', size=4, alpha=1.0, na.rm = TRUE, show.legend=F)

      if (input$showRug) {
        p <- p + geom_rug(aes(x=0), position = position_jitter(width=0), alpha=0.5, show.legend = F, na.rm = T)
      }

      if (params$rotate) {
        p <- p + scale_x_discrete(limits = rev(levels(dataset[[feature_col]])))
      }
    }

    return(facet_features(p, x_lab=x_lab, y_lab=y_lab, x_format=x_format, y_format=y_format))
  })

  output$distribution_plot <- renderPlot({
    req(!is.null(my$dataset_p()))

    return(postProcessPlot(plot$dist()))
  })

  wide <- reactiveValues(features=NULL)

  observeEvent(input$viewPairsPlot, {

    req(pcols$feature, pcols$value, input$valueCol)

    isolate({
      group_col <- pcols$group
      feature_col <- pcols$feature


      value_col <- pcols$value

      dataset <- NULL

      if (is.null(input$aggWithinFUN) || n$features() == nrow(my$dataset_p())) {
        dataset <- my$dataset_filtered()
        value_col <- input$valueCol
      } else {
        dataset <- my$dataset_p()
      }

      id_cols <- paste(colnames(dataset) %>% .[!(. %in% c(feature_col, value_col, '# observations'))],
                       collapse = '+')

      if (length(id_cols) == 0 || id_cols == '') { id_cols <- '.' }
      else { id_cols <- sub(id_cols, pattern='(.+\\s.+)', replacement='`\\1`') }

      f <- paste(id_cols, feature_col, sep='~')

      all_features <- c(as.character(unique(dataset[[feature_col]])))
      wide$features <- all_features

      n_features <- length(all_features)
      n_cells <- n_features ^ 2
      if (!is.null(input$featureX) && !is.null(input$featureY)) {
        n_cells <- 1
      } else if (!is.null(input$featureX) || !is.null(input$featureY)) {
        n_cells <- n_features-1 # -1 because we're not including the density plot.
      }

      if (n_cells > 100) {
        return(NULL)
      }
    })

    my$dataset_wide <- dataset %>% dcast(formula = f, value.var=value_col, fun.aggregate=mean, fill=NA)
  })

  plot$pairs <- eventReactive(input$viewPairsPlot, {
    req(pcols$feature, pcols$value, input$valueCol)

    isolate({

      # print('Entered pairs_plot block')

      scale_xy_continuous <- function(p) {
        return(p + scale_x_continuous(labels=scales::comma) +
                   scale_y_continuous(labels=scales::comma))
      }

      # https://stackoverflow.com/a/37890371
      continuous_fn <- function(data, mapping, pts=list(), smt=list(), ...) {
        scale_xy_continuous(postProcessPlot(
          ggplot(data = data, mapping = mapping, ...) +
            do.call(geom_point, pts) +
            do.call(geom_smooth, smt),
          n_colors = n$groups()
          ))
      }

      density_fn <- function(data, mapping, dns=list(), ...) {
        scale_xy_continuous(postProcessPlot(
          ggplot(data = data, mapping = mapping, ...) +
            do.call(geom_density, dns),
          n_colors=n$groups()
        ))
      }

      # Plot
      continuous_args <- list(continuous_fn,
                         pts=list(size=1, shape=20, alpha=0.5, na.rm=TRUE,
                                  aes(color=toName(pcols$group), fill=toName(pcols$group))),
                         smt=list(method='lm', linetype='dashed', se=F, size=1, na.rm=TRUE,
                                  aes(color=toName(pcols$group), fill=toName(pcols$group))))

      density_args <- list(density_fn,
                           dns=list(alpha=0.5, na.rm=TRUE,
                                    aes(color=toName(pcols$group), fill=toName(pcols$group))))

      feature_columns <- wide$features

      p <- suppressWarnings(
              ggpairs(my$dataset_wide, columns=feature_columns,
                    lower = list(continuous = do.call(wrap, continuous_args)),
                    upper = list(continuous = do.call(wrap, continuous_args)),
                    diag = list(continuous = do.call(wrap, density_args)),
                    legend = 1, title = input$title))

      return(postProcessPlot(p, n_colors = -1)) # ggpairs can't be colored outside of my_fn
    })
  })

  output$pairsPlotMessage <- renderText({
    # isolate({
    if (is.null(pcols$feature) || is.null(input$valueCol)) {
      'Select a feature and value column.'
    } else if (is.null(my$dataset_wide)) {
      'No output generated. Ensure feature count is between 2 and 10.'
    } else if (is.null(input$aggWithinFUN) || n$features() == nrow(my$dataset_p())) {
      'Non-distinct ID columns. Using source dataset.'
    } else {
      'Distinct ID columns. Using grouped dataset.'
    }
    # })
  })

  observe({
    req(my$dataset_wide)

    if (is.null(input$featureX)) {
      updateSelectizeInput(session, 'featureX', choices = wide$features)
    }
    if (is.null(input$featureY)) {
      updateSelectizeInput(session, 'featureY', choices = wide$features)
    }
  })

  output$pairs_plot <- renderPlot({
    # req(pcols$feature, pcols$value)

    input$viewPairsPlot

    isolate({
      p <- plot$pairs()

      i <- 0
      j <- 0

      if (!is.null(input$featureY)) {
        i <- match(input$featureY, p$yAxisLabels)
      }

      if (!is.null(input$featureX)) {
        j <- match(input$featureX, p$xAxisLabels)
      }

      if (i > 0 && j > 0) {
        p <- p[i, j]
      } else if (sum(c(i, j)) > 0) {
        n_col <- p$ncol - 1
        if (n_col > 2) {
          if (n_col %% 2 == 0) {
            n_col <- 2
          } else if (n_col %% 3 == 0) {
            n_col <- 3
          }
        }

        if (i > 0) {
          iter <- 1:p$ncol %>% .[. != i]
          p_list <- lapply(iter, function(j) { p[i, j] })
          p <- grid_arrange_shared_legend(plotlist=p_list, ncol=n_col, title=input$title)
        } else if (j > 0) {
          iter <- 1:p$ncol %>% .[. != j]
          p_list <- lapply(iter, function(i) { p[i, j] })
          p <- grid_arrange_shared_legend(plotlist=p_list, ncol=n_col, title=input$title)
        }
      } else {
        p <- p + theme(legend.position = 'bottom')
      }

      return(p)
    })
  })

  my$theme <- reactive({
    theme(axis.text.x = element_text(size=rel(1.5), hjust=1, angle=30),
          axis.text.y = element_text(size=rel(1.5)),
          # legend.text = element_text(size=rel(1.5)),
          strip.text = element_text(size=rel(1.5)),
          legend.title = element_blank(),
          # legend.title.align = 0.5,
          legend.position = input$legendPos,
          legend.justification = 'top',
          legend.box.background = element_rect(color='black'),
          legend.text = element_text(size=rel(1)),
          # legend.position = 'right',
          # legend.justification = 'right',
          axis.title.y = element_text(size=rel(1.5), face='bold'),
          axis.title.x = element_text(size=rel(1.5), face='bold'),
          # plot.background = element_blank(),
          plot.title = element_text(size=rel(2.0), hjust=0.5, face='bold'),
          plot.subtitle = element_text(size=rel(1.5), hjust=0.5),
          plot.caption = element_text(size=rel(1.5), face = 'italic'))
  })

  facet_features <- function(p, x_lab=NULL, y_lab=NULL, no_legend = FALSE,
                             x_format=scales::comma, y_format=scales::comma) {

    if (params$rotate) {
      p <- p + coord_flip()
    }

    if (!is.null(y_format)) {
      y_limits <- c(NA, NA) # (min, max)
      if (params$fixLimsY && isolate(n$groups()) <= 1) { y_limits[1] <- 0 } # set y-lim to (0, Max).

      p <- p + scale_y_continuous(name=y_lab, labels = y_format, limits = y_limits) #, trans=y_trans) #, expand = c(0.1, 0))
    } else if (!missing(y_lab)) {
      p <- p + labs(y=y_lab)
    }

    if (!is.null(x_format)) {
      x_limits <- c(NA, NA) # (min, max)
      if (params$fixLimsX && isolate(n$groups()) <= 1) { x_limits[1] <- 0 } # set y-lim to (0, Max).
      p <- p + scale_x_continuous(name=x_lab, labels=x_format, limits = x_limits)
    } else if (!missing(x_lab)) {
      p <- p + labs(x=x_lab)
    }

    facet_scales <- 'fixed'

    if (class(p$coordinates)[1] != 'CoordPolar') {
      if (!params$fixLimsY && !params$fixLimsX) {
        facet_scales <- 'free'
      } else if (!params$fixLimsX) {
        facet_scales <- 'free_x'
      } else if (!params$fixLimsY) {
        facet_scales <- 'free_y'
      } else {
        facet_scales <- 'fixed'
      }
    }

    if (!params$facetFeatures) {
      # isolate({
        if (n$groups() > 0) {
          p <- p + facet_wrap(~toName(pcols$group), scales = facet_scales, nrow=3)
        }
        if (n$features() == 0 || no_legend == TRUE) {
          p <- p + guides(color=F, fill=F)
        } else {
          legend_params <- NULL
          if (input$legendPos == 'bottom') {
            legend_params <- guide_legend(title=NULL, direction='horizontal', nrow=1, byrow=T)
          } else { # input$legendPos == 'top'
            legend_params <- guide_legend(title=NULL, direction='vertical', ncol=1, byrow=F)
          }
          p <- p + guides(color=legend_params, fill=legend_params)
        }
      # })
    } else {
      # isolate({
        if (n$groups() <= 1 && n$features() > 5) {
          p <- p + facet_wrap(~toName(pcols$feature), scales = facet_scales, nrow=3) + guides(color=F, fill=F)
          # subtitle <- if (n_Groups > 0) { my$dataset_p()[[pcols$group]][1] }
        } else {
          p <- p + facet_grid(toName(pcols$feature)~toName(pcols$group), scales = facet_scales) + guides(color=F, fill=F)
        }
      # })
    }

    return(p)
  }

  postProcessPlot <- function (p, myTheme=my$theme, baseTheme=theme_bw, n_colors = NULL,
                               title=NULL, subtitle=NULL, caption=NULL) { #xlab=NULL, ylab=NULL) {

    # print('Entered post_process block')

    if (missing(n_colors)) {
      n_colors <- max(1, n$features())
    }

    if (n_colors > 0) { # all except ggpairs plot.
      color_pal <- NULL

      if (is.null(input$plotColors)) {
        # https://stackoverflow.com/a/8197703
        color_pal <- gg_color_hue(n_colors)
      } else if (input$plotColors %in% colors(distinct=T)) {
          # RColorBrewer uses cap-letters
          # color_pal <- RColorBrewer::brewer.pal(n_colors, name=input$plotColors)[1:n_colors]
        color_pal <- rev(gradient_n_pal(c(input$plotColors, 'gray90'))(seq(1, 0, length.out = n_colors)))
      } else {
        color_pal <- match.fun(paste0(input$plotColors, '_pal'))()(n_colors)[1:n_colors]

        color_pal[is.na(color_pal)] <- '#808080'
      }

      p <- p + scale_color_manual(values=color_pal) + scale_fill_manual(values=color_pal)
    }

    if (missing(title)) {
      title <- if (!is.null(p$labels$title)) { p$labels$title } else { input$title }
    }
    if (missing(subtitle)) {
      subtitle <- if (!is.null(p$labels$subtitle)) { p$labels$subtitle } else { input$subtitle }
    }
    # xlab <- NULL
    if (missing(caption)) {
      caption <- if (!is.null(p$labels$caption)) { p$labels$caption } else { input$caption }
    }


    if (missing(baseTheme) && !is.null(input$plotTheme)) {
      baseTheme <- match.fun(paste0('theme_', input$plotTheme))
    }

    p + baseTheme() + my$theme() +
      labs(title=bquote(underline(.(title))),
           subtitle=subtitle,
           # x=xlab, # y=ylab,
           caption=sub(caption, pattern = '^\\s+', replacement = ''))
    # })
  }

  # https://yihui.shinyapps.io/DT-info/
  output$download = downloadHandler('data.csv', content = function(file) {
    fwrite(my$dataset[input$dataset_rows_all,], file)
  })

  output$download_final = downloadHandler('data-filtered.csv', content = function(file) {
    fwrite(my$dataset_grouped()[input$dataset_grouped_rows_all,], file)
  })

}
