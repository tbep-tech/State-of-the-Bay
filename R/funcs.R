#' plot chlorophyll and la annual avg as plotly
wqplotly_plo <- function(datin, bay_segment, yrrng, family, width, height){
  
  # chla
  p1 <- show_thrplot(datin, bay_segment = bay_segment, thr = "chla", yrrng =  yrrng, txtlab = F, labelexp = F) +
    ggtitle(NULL) +
    scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1975, maxyr))

  p1 <- plotly::ggplotly(p1, width = width, height = height) 

  p1$x$data[[4]] <- NULL
  p1$x$data[[3]] <- NULL
  # p1$x$data[[2]]$name <- 'Management target'
  # p1$x$data[[2]]$legendgroup <- 'Management target'
  
  # la
  
  p2 <- show_thrplot(datin, bay_segment = bay_segment, thr = "la", yrrng =  yrrng, txtlab = F, labelexp = F) +
    ggtitle(NULL) +
    scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1975, maxyr))
  
  p2 <- plotly::ggplotly(p2, width = width, height = height) 
  p2$x$data[[4]] <- NULL
  p2$x$data[[3]] <- NULL
  # p2$x$data[[2]]$name <- 'Management target'
  # p2$x$data[[2]]$legendgroup <- 'Management target'
  p2$x$data[[1]]$showlegend <- FALSE
  p2$x$data[[2]]$showlegend <- FALSE
  
  out <- plotly::subplot(p1, p2, nrows = 2, shareX = T, titleY = TRUE) %>%
    plotly::layout(
      legend = list(title = ''),
      font = list(family = family)
      ) %>% 
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(out)
  
}

# get list of water quality results for a given year
wqsum_fun <- function(datin, maxyr){
  
  avedat <- anlz_avedat(datin, partialyr = F) 
  
  cats <- avedat %>% 
    anlz_attain %>% 
    filter(yr == !!maxyr) %>% 
    mutate(
      action = case_when(
        outcome == 'green' ~ '<span style="color:#2DC938; text-shadow: 0 0 4px #333; letter-spacing: 2px">__Stay the Course__</span>',
        outcome == 'yellow' ~ '<span style="color:#E9C318; text-shadow: 0 0 4px #333; letter-spacing: 2px">__Caution__</span>', 
        outcome == 'red' ~ '<span style="color:#CC3231; text-shadow: 0 0 4px #333; letter-spacing: 2px">__On Alert__</span>'
      )
    ) %>% 
    select(bay_segment, action)
  
  trgs <- targets %>% 
    select(bay_segment, chla_target, la_target) %>% 
    pivot_longer(cols = -matches('bay_segment'), names_to = 'var', values_to = 'target') %>% 
    mutate(var = gsub('\\_target$', '', var))
  
  mets <- avedat %>%
    .$ann %>% 
    dplyr::filter(yr == maxyr) %>%
    mutate(var = gsub('^mean\\_', '', var)) %>% 
    filter(!var %in% 'sdm') %>% 
    dplyr::left_join(trgs, by = c('bay_segment', 'var')) %>%
    dplyr::mutate(
      trg = dplyr::case_when(
        val < target ~ 'met', 
        val >= target ~ 'not met'
      )
    ) %>% 
    select(bay_segment, var, trg) %>% 
    pivot_wider(names_from = 'var', values_from = 'trg')
  
  out <- cats %>% 
    full_join(mets, by = 'bay_segment') %>% 
    t %>% 
    as.data.frame
  
  names(out) <- out[1, ]
  
  out <- out %>% 
    apply(2, as.list)
 
  return(out)
  
}

#' plot total load as tn, hyd, or ratio, annual or monthly
ldtot_plo <- function(datin, yval = c('tn_load', 'hy_load', 'tnhy'), addlns = F, 
                      levs = c('All Segments (- N. BCB)', 'Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay'),
                      width = NULL, height = NULL, family){
  
  # ref lines
  lndf <- data.frame(
    bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay'), 
    ln = c(1.08, 1.62, 1.24, 0.97, 1.59)
  )
  
  ylbs <- tibble(
    yval = c('tn_load', 'hy_load', 'tnhy'), 
    ttl = c('Total Nitrogen (tons / yr)', 'Total Hydro Load (mill m3 / yr)', 'TN vs Hydrology ratio')
  ) 
  
  yval <- match.arg(yval)
  
  ttl <- ylbs %>% 
    filter(yval == !!yval) %>% 
    pull(ttl)
  
  for(lev in seq_along(levs)){
    
    toplo <- datin %>% 
      filter(bay_segment %in% !!levs[lev]) %>% 
      rename(
        dt = year, 
        yv = !!yval
      )
    
    p <- plot_ly(toplo, height = height, width = width)  %>% 
      add_trace(x = ~dt, y = ~yv, color = I('#435462'), mode = 'lines+markers', type = 'scatter', showlegend = F, name = levs[lev]) %>% #, marker = list(opacity = 1, size = 4)) %>% 
      add_annotations(
        text = ~unique(bay_segment),
        x = 0.5,
        y = 1.175,
        yref = "paper",
        xref = "paper",
        xanchor = "middle",
        yanchor = "top",
        showarrow = FALSE,
        font = list(size = 15)
      )
    
    if(lev == 2)
      p <- p %>% 
      layout(
        yaxis = list(title = ttl)
      )
    
    if(lev != 2)
      p <- p %>% 
      layout(
        yaxis = list(title = NA)
      )
    
    # horizontal ref line
    if(levs[lev] != 'All Segments (- N. BCB)' & addlns){
      
      ln <- lndf[lndf$bay_segment %in% levs[lev], 'ln']
      
      p <- p %>%  
        add_segments(x = min(toplo$dt), xend = max(toplo$dt), y = ln, yend = ln, line = list(color = 'grey', dash = 3), showlegend = F)
      
    }
    
    nm <- paste0('p', lev)
    
    assign(nm, p)
    
  }

  plts <- grep('^p\\d$', ls(), value = TRUE) 

  out <- subplot(mget(plts), shareX = T, nrows = length(levs), shareY = F, titleY = T) %>%
    layout(
      xaxis = list(title = NA, gridcolor = 'rgba(0,128,110, 0)'),
      plot_bgcolor = '#EEEEEE', 
      font = list(family = family)
    ) %>% 
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(out)
  
}

#' plot pop and tn/hy ratio
ldrat_plo <- function(totanndat, popdat, width = NULL, height = NULL, family){

  toplo <- totanndat %>% 
    filter(grepl('^All\\sSegments', bay_segment)) %>% 
    select(yr = year, tnhy) %>% 
    left_join(popdat, by = 'yr') %>% 
    mutate(
      pop = pop / 1e6
    )
  # browser()
  ay <- list(
    title = list(
      text = "Ratio of TN load\nto Hydrologic Load\n(tons/million m3)",
      font = list(color = "#435462")
    ), 
    tickfont = list(color = "#435462"),
    overlaying = "y",
    gridcolor = '#EEEEEE',
    side = "right"
  )

  out <- plot_ly(toplo, width = width, height = height) %>% 
    add_trace(x = ~yr, y = ~pop, color = I('#D9A650'), type = 'bar', showlegend = T, name = 'Pop.') %>%
    add_trace(x = ~yr, y = ~tnhy, color = I('#435462'), mode = 'lines+markers', type = 'scatter', showlegend = T, yaxis = 'y2', name = 'TN:Hydrology') %>% 
    layout(
      yaxis = list(
        title = list(text = 'Population (millions)', font = list(color = '#D9A650')), 
        tickfont = list(color = '#D9A650'),
        zerolinecolor = '#EEEEEE',
        zerolinewidth = 2,
        gridcolor = '#EEEEEE'
        ),
      yaxis2 = ay,
      xaxis = list(
        title = NA,
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        tickfont = list(color = 'black')
      ),
      plot_bgcolor = '#EEEEEE',
      font = list(family = family)
    ) %>% 
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(out)
  
}

# get list of tbni results for a given year
tbnisum_fun <- function(datin, maxyr){

  # get score intervals for tbni
  perc <- formals(anlz_tbniave)$perc %>% 
    eval
  
  # get segments results for current year
  tbnidsc <- anlz_tbniave(datin) %>% 
    filter(Year == maxyr) %>% 
    select(bay_segment, scr = Segment_TBNI, outcome) 
  
  # get bay wide average
  tbniall <-  tibble(
      bay_segment = 'all', 
      scr = round(mean(tbnidsc$scr), 0)
    ) %>% 
    mutate(
      outcome = findInterval(scr, perc),
      outcome = factor(outcome, levels = c('0', '1', '2'), labels = c('red', 'yellow', 'green')),
      outcome = as.character(outcome)
    )
  
  # combine segment and baywide, create action html
  tbnidsc <- tbnidsc %>% 
    bind_rows(tbniall) %>% 
    mutate(
      action = case_when(
        outcome == 'green' ~ '<span style="color:#2DC938; text-shadow: 0 0 4px #333; letter-spacing: 2px">__Stay the Course__</span>',
        outcome == 'yellow' ~ '<span style="color:#E9C318; text-shadow: 0 0 4px #333; letter-spacing: 2px">__Caution__</span>', 
        outcome == 'red' ~ '<span style="color:#CC3231; text-shadow: 0 0 4px #333; letter-spacing: 2px">__On Alert__</span>'
      )
    )
  
  # format as list of lists
  out <- tbnidsc %>% 
    t %>% 
    as.data.frame
  
  names(out) <- out[1, ]
  
  out <- out %>% 
    apply(2, as.list)
  
  return(out)
  
}


# get list of tbni results for a given year
tbbisum_fun <- function(datin, maxyr, seg){
  
  tbbimed <- anlz_tbbimed(datin, bay_segment = seg, yrrng = c(1993, maxyr))
  
  tbbidsc <- tbbimed %>% 
    filter(yr == maxyr) %>% 
    select(bay_segment, cat = TBBICat) %>% 
    mutate(
      cat = case_when(
        cat == 'Good' ~ '<span style="color: #2DC938; text-shadow: 0 0 4px #333; letter-spacing: 2px"><b>Good</b></span>',
        cat == 'Fair' ~ '<span style="color: #E9C318; text-shadow: 0 0 4px #333; letter-spacing: 2px"><b>Fair</b></span>', 
        cat == 'Poor' ~ '<span style="color: #CC3231; text-shadow: 0 0 4px #333; letter-spacing: 2px"><b>Poor</b></span>'
      )
    )
  
  # format as list of lists
  out <- tbbidsc %>% 
    t %>% 
    as.data.frame
  
  names(out) <- out[1, ]
  
  out <- out %>% 
    apply(2, as.list)
  
  return(out)
  
}

# reactable table function that works for supra/intertidal and subtidal
lngtrmtab_fun <- function(datin, colnm, typ = c('subtidal', 'supratidal'), yrsel = '1988', topyr = '2018', 
                          firstwidth = 240, estout = F, family, fntsz = 14){
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee")
  
  jsfun <- JS("function(rowInfo) {
    var value = rowInfo.row.chg
    if (parseInt(value) >= 0) {
      var color = '#008000E6'
    } else if (parseInt(value) < 0) {
      var color = '#e00000E6'
    } 
    return { color: color, fontWeight: 'bold' }
    }"
  )
  
  typ <- match.arg(typ)
  
  # options that change by input
  rmv <- c('Open Water', 'Oyster Bars')
  ttl <- paste0('Watershed land use change from ', yrsel, ' to ', topyr, ' (acres and % change)')
  if(typ == 'subtidal'){
    rmv <- c(rmv, 'Restorable')
    ttl <- gsub('^Watershed', 'Subtidal', ttl)
  }

  # arrange input data and take chg diff  
  sums <- datin %>%
    filter(!HMPU_TARGETS %in% rmv) %>% 
    spread(name, Acres, fill = NA) %>% 
    rename(chgyr = !!yrsel) %>%
    rename(maxyr = !!topyr) %>% 
    mutate(
      chg = maxyr -  chgyr,
      chgper = 100 * (maxyr - chgyr) / chgyr, 
      chgicon = case_when(
        chgper >= 0 ~ 'arrow-circle-up', 
        chgper < 0 ~ 'arrow-circle-down'
      ), 
      chgcols = case_when(
        chgper >= 0 & HMPU_TARGETS != 'Developed' ~ '#008000E6', 
        chgper < 0 & HMPU_TARGETS != 'Developed' ~ '#e00000E6',
        chgper >= 0 & HMPU_TARGETS == 'Developed' ~ '#e00000E6', 
        chgper < 0 & HMPU_TARGETS == 'Developed' ~ '#008000E6'
      )
    ) %>% 
    rename(val = HMPU_TARGETS)

  names(sums)[names(sums) == 'chgyr'] <- yrsel
  names(sums)[names(sums) == 'maxyr'] <- topyr
  
  totab <- sums %>% 
    mutate(
      chg = formatC(round(chg, 0), format = "d", big.mark = ","),
      chgper = as.character(round(chgper, 0))
    )

  if(estout)
    return(totab)
  
  # get color from totab, but has to be indexed and include all args
  stylefunc <- function(value, index, name) {
    col <- totab[index, 'chgcols'][[1]]
    list(color = col, fontWeight = 'bold')
  } 
  
  out <- reactable(
    totab, 
    columns = list(
      chgcols = colDef(show = F),
      chgicon = colDef(show = F),
      val = colDef(name = colnm, footer = 'Total', minWidth = firstwidth, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      chg = colDef(name = paste0(yrsel, '-', topyr, ' change'), minWidth = 140,
                   style = stylefunc, class = 'sticky right-col-2', headerClass = 'sticky right-col-2', footerClass = 'sticky right-col-2'
      ), 
      chgper = colDef(name = '% change', minWidth = 85,
                      style = stylefunc,
                      format = colFormat(suffix = '%', digits = 0), 
                      align = 'right',
                      class = 'sticky right-col-1', headerClass = 'sticky right-col-1', footerClass = 'sticky right-col-1', 
                      cell = icon_sets(totab, icon_ref = 'chgicon', icon_position = 'right', icon_color_ref = 'chgcols')
                      
      )
    ),
    defaultColDef = colDef(
      footer = function(values){
        if(!is.numeric(values))
          return()
        
        formatC(round(sum(values), 0), format= "d", big.mark = ",")
        
      },
      footerStyle = list(fontWeight = "bold", fontSize = fntsz, fontFamily = family),
      headerStyle = list(fontSize = fntsz, fontFamily = family),
      format = colFormat(digits = 0, separators = TRUE), 
      minWidth = 80, resizable = TRUE
    ),
    defaultPageSize = nrow(sums),
    showPageSizeOptions = F,
    highlight = T,
    wrap = F, 
    style = list(fontSize = paste0(fntsz, 'px'), fontFamily = family)
  )
  
  # add caption
  out <- htmlwidgets::prependContent(out, h5(class = "title", ttl))

  return(out)
  
}

# hmpu database projects table
rstdat_tab <- function(rstdat, maxyr, fntsz = 14, family){

  # data prep
  rstsum <- rstdat %>% 
    select(
      Year = `Year Reported`, 
      Category = `Habitat Type (basic ESA categories)(existing databases)`, 
      Acres, 
      Activity = `Basic Activity (Enhance/Rest)`, 
      `Linear Miles` = `Linear Miles`,
      `Linear Ft` = `Linear Feet`
    ) %>% 
    rowwise() %>% 
    mutate(
      Category = case_when(
        Category == 'estuarine' ~ 'Estuarine', 
        Category == 'Upland' ~ 'Uplands',
        grepl('^Mix', Category) ~ 'Mixed', 
        T ~ Category
      ), 
      Miles = sum(`Linear Miles`,  `Linear Ft` / 5280, na.rm = T)
    ) %>% 
    ungroup() %>% 
    filter(Year <= maxyr) %>% 
    group_by(Category, Activity) %>% 
    summarise(
      tot= n(),
      Acres = sum(Acres, na.rm = T), 
      Miles = sum(Miles, na.rm = T),
      .groups = 'drop'
    ) %>% 
    filter(!is.na(Category)) %>% 
    group_by(Category) %>% 
    mutate(
      tot = sum(tot)
    ) %>% 
    pivot_longer(c('Acres', 'Miles'), names_to = 'var', values_to = 'val') %>% 
    unite('var', Activity, var, sep = ', ') %>% 
    pivot_wider(names_from = 'var', values_from = 'val') %>% 
    select(Category, tot, `Restoration, Acres`, `Restoration, Miles`, `Enhancement, Acres`, `Enhancement, Miles`)

  # yrrng
  yrs <- rstdat %>% 
    pull(`Year Reported`) %>% 
    min(na.rm = T) %>% 
    c(., maxyr)
  
  # table
  tab <- reactable(
    rstsum, 
    columns = list(
      Category = colDef(name = 'Habitat', footer = 'Total',  minWidth = 50, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      tot = colDef(name = 'Total projects', minWidth = 50)
    ),
    defaultColDef = colDef(
      footer = function(values){
        if(!is.numeric(values))
          return()
        
        formatC(round(sum(values), 0), format= "d", big.mark = ",")
        
      },
      headerStyle= list(fontSize = fntsz, fontFamily = family),
      footerStyle = list(fontWeight = "bold", fontSize = fntsz, fontFamily = family),
      format = colFormat(digits = 0, separators = TRUE), 
      minWidth = 80,
      style = list(fontSize = fntsz, fontFamily = family),
      resizable = TRUE
    ),
    showPageSizeOptions = F,
    highlight = T,
    wrap = F
  )
  
  # add title
  ttl <- paste0('Enhancement and restoration projects in Tampa Bay (', yrs[1], '-', yrs[2], ')')
  out <-  htmlwidgets::prependContent(tab, h5(class = "title", ttl))
  
  return(out)
  
}

# reactable table for comms reach statistics
# icons guidance https://kcuilla.github.io/reactablefmtr/articles/icon_sets.html
coms_tab <- function(comdat, category = c('Website', 'Social Media', 'Email Marketing', 'Tarpon Tag'), 
                      maxyr, fntsz = 20, chg = TRUE, showtab = TRUE, family){
  
  category <- match.arg(category)
  
  cats <- list(
    `Website` = c('GA: tbep.org', 'GSC: TBEP.ORG'), 
    `Social Media` = c('TBEP IG', 'TBEP Facebook', 'TBEP Twitter', 'TBEP YouTube'), 
    `Email Marketing` = c('Constant Contact'), 
    `Tarpon Tag` = c('Tarpon Tag')
  )

  ics <- list(
    `GA: tbep.org` = list(
      metric = c('Unique Page Views'), 
      icons = c('eye')
    ),
    `GSC: TBEP.ORG` = list(
      metric = c('Total Clicks'), 
      icons = c('mouse-pointer')
    ),
    `TBEP IG` = list(
      metric = c('Engagements', 'Total Followers'),
      icons = c('heart', 'users') 
    ),
    `TBEP Facebook` = list(
      metric = c('Engagements', 'Total Fans'),
      icons = c( 'heart', 'users') 
    ), 
    `TBEP Twitter` = list(
      metric = c('Engagements', 'Followers'), 
      icons = c('heart', 'users')
      ),
    `TBEP YouTube` = list(
      metric = c('Total Views', 'Total Subscribers'),
      icons = c('film', 'users')
      ), 
    `Constant Contact` = list(
      metric = c('Net new contacts'), 
      icons = c('users')
      ), 
    `Tarpon Tag` = list(
      metric = c('Statewide Registrations'), 
      icons = c('car')
      )
    ) %>% 
    lapply(data.frame) %>% 
    enframe('platform', 'value') %>% 
    unnest('value') %>% 
    mutate(
      tab_name = gsub('^TBEP\\s', '', platform), 
      tab_name = case_when(
        tab_name == 'IG' ~ 'Instagram', 
        grepl('^GA|^GSC', tab_name) ~ 'Website', 
        tab_name == 'Constant Contact' ~ 'Email Marketing',
        T ~ tab_name
      ), 
      tab_metric = gsub('^Total\\s', '', metric), 
      tab_metric = case_when(
        metric == 'Unique Page Views' ~ 'Page Views', 
        metric == 'Net new contacts' ~ 'New Contacts', 
        T ~ tab_metric
      )
    )
  
  platform <- cats[[category]]
  toflt <- ics %>% 
    filter(platform %in% !!platform)

  # filter tarpon tag to dec, since it's a running tally, not new registrations
  if(category == 'Tarpon Tag')
    comdat <- comdat %>% 
      filter(month == 'dec')
  
  # filter social media users to dec, since it's a running tally, not new users
  if(category == 'Social Media')
    comdat <- comdat %>% 
      filter(!(month != 'dec' & metric %in% c('Total Followers', 'Total Fans', 'Followers', 'Total Subscribers')))

  # table as change
  if(chg){
    
    cmpyr <- maxyr - 1
  
    sumdat <- comdat %>% 
      filter(platform %in% toflt$platform) %>% 
      filter(metric %in% toflt$metric) %>% 
      filter(year %in% c(maxyr, cmpyr)) %>% 
      group_by(platform, metric, year) %>%
      summarise(
        val = sum(val), 
        .groups = 'drop'
      ) %>% 
      pivot_wider(names_from = 'year', values_from = 'val') %>% 
      rename(
        maxyr = !!as.character(maxyr), 
        cmpyr = !!as.character(cmpyr)
      ) %>% 
      left_join(toflt, by = c('platform', 'metric')) %>% 
      select(tab_name, tab_metric, icons, cmpyr, maxyr) %>% 
      mutate(
        `% change` = (maxyr - cmpyr) / cmpyr, 
        # `% change` = round(`% change`, 0), 
        `chgicon` = case_when(
          `% change` > 0 ~ 'arrow-alt-circle-up', 
          `% change` < 0 ~ 'arrow-alt-circle-down'
        ),
        `chgcols` = case_when(
          `% change` > 0 ~ 'darkgreen', 
          `% change` < 0 ~ 'red'
        ),
        tab_name = ifelse(duplicated(tab_name), '', tab_name)
      ) 

    out <- reactable(
      sumdat, 
      columns = list(
        tab_name = colDef(
          name = '',
          show = showtab#,
          # minWidth = 100
        ),
        icons = colDef(show = F),
        chgicon = colDef(show = F), 
        chgcols = colDef(show = F),
        tab_metric = colDef(
          # minWidth = 300,
          name = '',
          cell = icon_sets(sumdat, icon_ref = "icons", icon_position = "left", icon_size = fntsz, colors = "black"), 
          align = 'right'
        ), 
        cmpyr = colDef(
          name = as.character(cmpyr), 
          format = colFormat(separators = TRUE), 
          align = 'center'
        ), 
        maxyr = colDef(
          name = as.character(maxyr), 
          format = colFormat(separators = TRUE), 
          align = 'center'
        ),
        `% change` = colDef(
          name = 'Change',
          cell = icon_sets(sumdat, icon_ref = 'chgicon', icon_color_ref = "chgcols", icon_size = fntsz, number_fmt = scales::percent)
        )
      ), 
      style = list(fontSize = paste0(fntsz, 'px'), fontFamily = family),
      defaultColDef = colDef(
        headerStyle = list(fontSize = paste0(fntsz, 'px'), fontFamily = family),
        footerStyle = list(fontSize = paste0(fntsz, 'px'), fontFamily = family)
      ),
      borderless = T, 
      resizable = T, 
      sortable = F,
      theme = reactableTheme(
        headerStyle = list(borderColor = 'white')
      )
    )
    
  }
    
  if(!chg){

    sumdat <- comdat %>% 
      filter(platform %in% toflt$platform) %>% 
      filter(metric %in% toflt$metric) %>% 
      filter(year %in% !!maxyr) %>% 
      group_by(platform, metric, year) %>%
      summarise(
        val = sum(val), 
        .groups = 'drop'
      ) %>% 
      pivot_wider(names_from = 'year', values_from = 'val') %>% 
      rename(
        maxyr = !!as.character(maxyr)
      ) %>% 
      left_join(toflt, by = c('platform', 'metric')) %>% 
      select(tab_name, tab_metric, icons, maxyr) %>% 
      arrange(tab_name, desc(tab_metric)) %>% 
      mutate(
        tab_name = ifelse(duplicated(tab_name), '', tab_name)
      )

    out <- reactable(
      sumdat, 
      columns = list(
        icons = colDef(show = F),
        tab_name = colDef(
          name = '',
          align = 'center',
          show = showtab#,
          # minWidth = 100
        ),
        tab_metric = colDef(
          # minWidth = 300,
          name = '',
          cell = icon_sets(sumdat, icon_ref = "icons", icon_position = "left", icon_size = fntsz, colors = "black"), 
          align = 'center'
        ), 
        maxyr = colDef(
          name = '', 
          format = colFormat(separators = TRUE), 
          align = 'center'
        )
      ), 
      style = list(fontSize = paste0(fntsz, 'px'), fontFamily = family),
      defaultColDef = colDef(
        headerStyle = list(fontSize = paste0(fntsz, 'px'), fontFamily = family),
        footerStyle = list(fontSize = paste0(fntsz, 'px'), fontFamily = family)
      ),
      borderless = T, 
      resizable = T, 
      sortable = F,
      theme = reactableTheme(
        headerStyle = list(borderColor = 'white')
      )
    )
    
  }
  
  return(out)  

}

# create trends plot for different comms data
comssum_plo <- function(comdat, category = c('Website', 'Social Media', 'Email Marketing', 'Tarpon Tag'), 
                        metric = NULL, fntsz = 17, family, width, height){
  
  cats <- list(
    `Website` = c('GA: tbep.org', 'GSC: TBEP.ORG'), 
    `Social Media` = c('TBEP IG', 'TBEP Facebook', 'TBEP Twitter', 'TBEP YouTube'), 
    `Email Marketing` = c('Constant Contact'), 
    `Tarpon Tag` = c('Tarpon Tag')
  )
  
  ics <- list(
    `GA: tbep.org` = list(
      metric = c('Unique Page Views'), 
      icons = c('eye')
    ),
    `GSC: TBEP.ORG` = list(
      metric = c('Total Clicks'), 
      icons = c('mouse-pointer')
    ),
    `TBEP IG` = list(
      metric = c('Engagements', 'Total Followers'),
      icons = c('heart', 'users') 
    ),
    `TBEP Facebook` = list(
      metric = c('Engagements', 'Total Fans'),
      icons = c( 'heart', 'users') 
    ), 
    `TBEP Twitter` = list(
      metric = c('Engagements', 'Followers'), 
      icons = c('heart', 'users')
    ),
    `TBEP YouTube` = list(
      metric = c('Total Views', 'Total Subscribers'),
      icons = c('film', 'users')
    ), 
    `Constant Contact` = list(
      metric = c('Net new contacts'), 
      icons = c('users')
    ), 
    `Tarpon Tag` = list(
      metric = c('Statewide Registrations'), 
      icons = c('car')
    )
  ) %>% 
  lapply(data.frame) %>% 
  enframe('platform', 'value') %>% 
  unnest('value') %>% 
  mutate(
    tab_name = gsub('^TBEP\\s', '', platform), 
    tab_name = case_when(
      tab_name == 'IG' ~ 'Instagram', 
      grepl('^GA|^GSC', tab_name) ~ 'Website', 
      tab_name == 'Constant Contact' ~ 'Email Marketing',
      T ~ tab_name
    ), 
    tab_metric = gsub('^Total\\s', '', metric), 
    tab_metric = case_when(
      metric == 'Unique Page Views' ~ 'Page Views', 
      metric == 'Net new contacts' ~ 'Net New Contacts', 
      T ~ tab_metric
    )
  )

  platform <- cats[[category]]
  toflt <- ics %>% 
    filter(platform %in% !!platform)
  
  if(category %in% c('Social Media')){

    cols <- c('#00806E', '#004F7E', '#5C4A42', '#958984')
    names(cols) <- c('Facebook', 'Instagram', 'Twitter', 'YouTube')
    
    if(metric != 'users')
      userplo <- toflt %>% 
        filter(icons != 'users')
    
    if(metric == 'users')
      userplo <- toflt %>% 
        filter(icons == 'users')

    userplo <- userplo %>% 
      group_by(platform, metric, tab_name, tab_metric) %>% 
      nest() %>% 
      mutate(
        data = purrr::pmap(list(platform, metric, tab_name, tab_metric), function(platform, metric, tab_name, tab_metric){
          
          pltin <- platform
          metin <- metric
          col <- cols[tab_name]
          
          toplo <- comdat %>%
            filter(platform %in% pltin) %>%
            filter(metric %in% metin) %>%
            mutate(
              date = ymd(paste(year, month, '01', sep = '-'))
            )
          
          p <- plot_ly(data = toplo, x =  ~date, width = width, height = height) %>%
            add_trace(y = ~`val`, mode = 'lines+markers', type = 'scatter', name = tab_name,
                      marker = list(color = col, size = 10),
                      line = list(color = '#5C4A42', width = 2, dash = 'dot')
            ) %>%
            # add_annotations(
            #   text = tab_name,
            #   x = 0.5,
            #   y = 1.1,
            #   yref = "paper",
            #   xref = "paper",
            #   xanchor = "middle",
            #   yanchor = "top",
            #   showarrow = FALSE,
            #   font = list(size = fntsz)
            # ) %>% 
            layout(
              xaxis = list(
                title = NA
              ),
              yaxis = list(
                title = tab_metric,
                tickprefix = NULL
              ),
              font = list(family = family, size = fntsz - 1)
            )
          
          return(p)
          
        })
      )
    
    p <- subplot(userplo$data[[2]], userplo$data[[1]], userplo$data[[3]], userplo$data[[4]], nrows = 4, shareX = T, titleY = T)
    
  }
  
  if(category %in% c('Website', 'Email Marketing')){
    
    userplo <- toflt %>% 
      group_by(platform, metric, tab_name, tab_metric) %>% 
      nest() %>% 
      mutate(
        data = purrr::pmap(list(platform, metric, tab_name, tab_metric), function(platform, metric, tab_name, tab_metric){
          
          pltin <- platform
          metin <- metric
          
          toplo <- comdat %>%
            filter(platform %in% pltin) %>%
            filter(metric %in% metin) %>%
            mutate(
              date = ymd(paste(year, month, '01', sep = '-'))
            )
          
          p <- plot_ly(data = toplo, x =  ~date, showlegend = F, width = width, height = height) %>%
            add_trace(y = ~`val`, mode = 'lines+markers', type = 'scatter', name = tab_name,
                      marker = list(color = '#00806E', size = 10),
                      line = list(color = '#5C4A42', width = 2, dash = 'dot')
            ) %>%
            layout(
              xaxis = list(
                title = NA
              ),
              yaxis = list(
                title = tab_metric,
                tickprefix = NULL
              ),
              font = list(family = family, size = fntsz - 1)
            )
          
          return(p)
          
        })
      )
    
    if(category == 'Website')
      p <- subplot(userplo$data[[1]], userplo$data[[2]], nrows = 2, shareX = T, titleY = T)
    
    if(category == 'Email Marketing')
      p <- subplot(userplo$data[[1]], nrows = 1, shareX = T, titleY = T)
    
  }

  p <- p %>% 
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
    
  return(p)
  
}

# reactable table for comms reach statistics, constant contact only
# icons guidance https://kcuilla.github.io/reactablefmtr/articles/icon_sets.html
ccreach_tab <- function(comdat, maxyr, fntsz = 16, family){
  
  fct <- c('Net new contacts', 'Number of Campaigns Sent', 'Open Rate', 'Click Rate')

  sumdat <- comdat %>% 
    filter(platform %in% 'Constant Contact') %>% 
    filter(metric %in% fct) %>% 
    filter(year %in% !!maxyr) %>% 
    group_by(metric, uni) %>%
    nest() %>% 
    mutate(
      val = purrr::pmap(list(data, uni), function(data, uni){
        
        if(uni == 'percent')
          out <- round(mean(data$val, na.rm = T), 0)
        if(uni == 'count')
          out <- sum(data$val, na.rm = T)
        
        return(out)
      
      })
    ) %>% 
    select(metric, uni, val) %>% 
    mutate(
      metric = factor(metric, levels = fct)
    ) %>% 
    arrange(metric) %>% 
    unnest('val') %>% 
    ungroup %>% 
    mutate(
      val = case_when(
        uni == 'percent'~ paste0(val, '%'), 
        T ~ as.character(val)
      ), 
      metric = as.character(metric),
      metric = case_when(
        uni == 'percent' ~ paste0('Avg. Monthly ', metric), 
        T ~ metric
      ), 
      metric = gsub('^Net\\snew\\contacts', 'New Contacts', metric)
    ) %>% 
    select(-uni)
  
  tobnd <- tibble(
    metric = c('All-Industry Average Open Rate', 'All-Industry Average Click Rate'), 
    val = c('16%', '7%')
  )
  
  sumdat <- bind_rows(sumdat, tobnd)
  
  out <- reactable(
    sumdat, 
    columns = list(
      metric = colDef(
        minWidth = 300,
        name = '',
        align = 'right'
      ), 
      val = colDef(
        name = as.character(maxyr), 
        format = colFormat(separators = TRUE), 
        align = 'center'
      )
    ), 
    style = list(fontSize = paste0(fntsz, 'px'), fontFamily = family),
    defaultColDef = colDef(
      headerStyle = list(fontSize = paste0(fntsz, 'px'), fontFamily = family),
      footerStyle = list(fontSize = paste0(fntsz, 'px'), fontFamily = family)
    ),
    borderless = T, 
    resizable = T, 
    theme = reactableTheme(
      headerStyle = list(borderColor = 'white')
    )
  )
  
  return(out)  
  
}

# tally gad efforts
# yrsel not provided, get totals for all data in gaddat, plus 2019/2020 hard-coded
# yrsel provided will get totals for yrsel
gadsum_fun <- function(gaddat, yrsel = NULL){

  out <- gaddat
  
  # filter by year if provided
  if(!is.null(yrsel))
    out <- out %>% 
      filter(year %in% yrsel)

  out <- out %>% 
    mutate(
      nvols = nadults + nyouth
    ) %>% 
    # select(-nadults, -nyouth) %>% 
    pivot_longer(-year, names_to = 'var', values_to = 'val') %>% 
    group_by(var) %>% 
    summarise(
      val = sum(val, na.rm = T), 
      .groups = 'drop'
    )
  
  # final formatting  
  out <- out %>% 
    pivot_wider(names_from = 'var', values_from = 'val') %>% 
    mutate(
      ntons = nlbs / 2e3, 
      ntons = round(ntons, 1)
    ) %>% 
    mutate_all(function(x) format(x, big.mark = ',', scientific = FALSE))
  
   return(out)
    
}

# plot gad efforts
# datin is summmary output from gadsum_fun w/ yrsel not null 
gadsum_plo <- function(datin, h = 3, w = 15, padding = 0, rows = 5, family){ 
  
  box::use(
    emojifont[...]
  )

  txt <- tibble(
    name = c('nevent', 'nvols', 'nlbs', 'nplants', 'npartner'),
    info = c('Event areas are prioritized by the presence of excessive litter & native habitat degradation, often overlapping with neighborhoods that have historically not received the support to facilitate restorative activities.',
             paste(datin$nadults, 'adults &', datin$nyouth, 'youths helped to protect and restore the bay this season.'),
             'Including trash, invasive plants & marine debris.', 
             "Native plants increase the bay's resiliency a& restore crucial wildlife habitat.",
             'Our partners play an invaluable role in recruiting volunteers to help us put in work!'
    ), 
    txtadd = c('EVENTS', 'VOLUNTEERS', 'LBS REMOVED', 'PLANTS INSTALLED', 'PARTNERS'),
    icon = paste0('fa-', c('calendar', 'users', 'trash', 'tree', 'handshake-o')), 
    txtcols = c("#08306B", "#08306B", "#F7FBFF", "#F7FBFF", "#F7FBFF")
  )
  
  cols <- nrow(txt) / rows
  
  toplo <- datin %>% 
    pivot_longer(everything()) %>% 
    inner_join(txt, by = 'name') %>% 
    unite('value', value, txtadd, sep = ' ') %>% 
    mutate(
      h = h,
      w = w,
      icon = emojifont::fontawesome(icon),
      font_family = 'fontawesome-webfont',
      name = factor(name, levels = rev(txt$name))
    ) %>%  
    arrange(name) %>% 
    mutate(
      x = rep(seq(0, (!!w + padding) * cols - 1, !!w + padding), times = rows),
      y = rep(seq(0, (!!h + padding) * rows - 1, !!h + padding), each = cols),
      info = str_wrap(info, 75)
    )

  p <- ggplot(toplo, aes(x, y, height = h, width = w, label = info)) +
    geom_tile(aes(fill = name)) +
    geom_text(fontface = "bold", size = 10, family = family,
              aes(label = value, x = x - w/2.2, y = y + h/4, color = name), hjust = 0) +
    geom_text(size = 5, lineheight = 1.7, family = family,
              aes(color = name, label = info, x = x - w/2.2, y = y - h/6), hjust = 0) +
    coord_fixed() +
    scale_fill_brewer(type = "cont", palette = "Blues", direction = -1) +
    scale_color_manual(values = toplo$txtcols) +
    geom_text(size = 20, aes(label = icon, family = font_family,
                             x = x + w/2.5, y = y + h/8), alpha = 0.25) +
    theme_void() +
    guides(
      fill = 'none', 
      color = 'none'
    )
  
  return(p)
  
}

# create a crosstalk widget of map and table
# datin is gaddat
gadmap_fun <- function(datin){
  
  tomap <- datin %>% 
    filter(!is.na(lng)) %>% 
    mutate(
      Volunteers = nadults + nyouth
    ) %>% 
    select(
      Event = event, 
      Year = year, 
      Volunteers, 
      `Lbs. of trash removed` = nlbs, 
      `Plants installed` = nplants, 
      lng = lng, 
      lat = lat, 
      Description = descrip
    ) %>% 
    unite(lab, c('Event', 'Description'), sep = ': ', remove = F) %>% 
    mutate(
      lab = str_wrap(lab, 50),
      lab = paste0('<b>', lab),
      lab = gsub(':', '</b>:', lab),
      lab = gsub('\\n', '<br/>', lab),
      lab = lapply(lab, HTML)
    ) %>% 
    as.data.frame()
  
  sd <- SharedData$new(tomap)
  
  # put both elements in a list if arrange by row
  out <- bscols(list(
    leaflet(sd) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addMarkers(lng = ~lng, lat = ~lat, label = ~lab),
    datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%", rownames = F,
              options=list(deferRender=TRUE, scrollY=300, scroller=F, dom = 't', pageLength = nrow(tomap),
                           columnDefs = list(list(visible=FALSE, 
                                                  targets=c(0, 6, 7, 8))))
    )
  ))
  
  return(out)
  
}
  
# get grant funding summary
# datin is tberfdat, bmgdat, or dcgdat
# yrsel not provided, get totals for all data
# yrsel provided will get totals for yrsel
grntsum_fun <- function(datin, yrsel = NULL, rnd = c('M', 'k')){
  
  rnd <- match.arg(rnd)
  
  if(any(!names(datin) %in% c("year", "title", "lead", "total", "admin_total", "matching")))
    stop('Check input names...')
  
  txt <- c(1e6, 1e3)
  names(txt) <- c('M', 'k')
  rndv <- txt[rnd]
  
  if(!is.null(yrsel))
    datin <- datin %>% 
      filter(year == yrsel)

  # add admin costs to tberf
  if('admin_total' %in% names(datin))
    datin <- datin %>% 
      mutate(
        admin_total = ifelse(is.na(admin_total), 0, admin_total),
        total = total + admin_total,
      ) %>% 
      select(-admin_total)
  
  out <- datin %>% 
    pivot_longer(cols = matches('total|matching')) %>% 
    group_by(name) %>% 
    summarise(
      n = n(),
      value = sum(value, na.rm = T)
    ) %>% 
    mutate(
      value = round(value / rndv, 1), 
      value = paste0('$', value, rnd),
      n = formatC(n, format = "d", big.mark = ",")
    ) %>% 
    pivot_wider()

  return(out)
  
}

# plotly graphic of running totals for grants
# currently works for license plate sales (comdat), bay mini grants (bmgdat), and tberf (tberfdat)
grntsum_plo <- function(datin, family, width, height){
  
  ylb <- 'Annual total awarded'
  tickprf <- '$'
  
  # comdat input
  if('platform' %in% names(datin)){
    
    ylb <- 'Monthly statewide registrations'
    tickprf <- NULL
    
    toplo <- comdat %>% 
      filter(platform == 'Tarpon Tag') %>% 
      filter(metric == 'Statewide Registrations') %>% 
      mutate(dy = 1) %>% 
      unite('date', year, month, dy, sep ='-') %>% 
      mutate(date = ymd(date))
    
  }
  
  # tberf input
  if('admin_total' %in% names(datin)){
    
    datin <- datin %>% 
      mutate(
        admin_total = ifelse(is.na(admin_total), 0, admin_total),
        total = total + admin_total,
      ) %>% 
      select(-admin_total)
    
  }
  
  # tberf or bmg input
  if(!'platform' %in% names(datin)){
    
    toplo <- datin %>% 
      group_by(year) %>% 
      summarise(
        val = sum(total, na.rm = T), 
        .groups = 'drop'
      ) %>% 
      rename(date = year)
    
  }
  
  p <- plot_ly(data = toplo, x =  ~date, width = width, height = height) %>% 
    add_trace(y = ~`val`, mode = 'lines+markers', type = 'scatter', 
              marker = list(color = '#00806E', size = 15), 
              line = list(color = '#5C4A42', width = 2, dash = 'dot')
    ) %>% 
    layout(
      xaxis = list(
        title = NA
      ), 
      yaxis = list(
        title = ylb, 
        tickprefix = tickprf
      ), 
      font = list(family = family, size = 18)
    ) %>% 
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(p)
  
}

grnt_tab <- function(..., yrsel, fntsz = 20, family){
  
  ics <- list(
    levs = c('newlead', 'n', 'total'),
    labs = c('New partners', 'Total projects', 'Total funds awarded'),
    icons = c('handshake', 'tools', 'dollar-sign')
  )
  
  # datin
  dat <- bind_rows(...)
  
  # check input columns
  if(any(!names(dat) %in% c("year", "title", "lead", "total", "admin_total", "matching")))
    stop('Check input names...')
  
  # add admin costs to tberf
  dat <- dat %>% 
    mutate(
      admin_total = ifelse(is.na(admin_total), 0, admin_total),
      total = total + admin_total,
    ) %>% 
    select(-admin_total)
  
  nototab <- dat %>% 
    filter(year != yrsel)
  
  # find new partners
  totab <- dat %>% 
    filter(year == yrsel) %>% 
    summarise(
      newlead = sum(!lead %in% nototab$lead), 
      total = sum(total, na.rm = T), 
      n = n()
    ) %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(
      metric = factor(name, levels = ics$levs, labels = ics$labs), 
      icons = factor(name, levels = ics$levs, labels = ics$icons),
      icons = as.character(icons), 
      value = formatC(value, format = "d", big.mark = ","), 
      value = case_when(
        name == 'total' ~ paste0('$', value), 
        T ~value
      )
    ) %>% 
    arrange(metric) %>% 
    select(name, icons, metric, value)
  
  out <- reactable(
    totab, 
    columns = list(
      icons = colDef(show = F),
      name = colDef(show = F),
      metric = colDef(
        # minWidth = 200,
        name = '',
        cell = icon_sets(totab, icon_ref = "icons", icon_position = "left", icon_size = fntsz, colors = "black"), 
        align = 'center'
      ), 
      value = colDef(
        # minWidth = 200,
        name = '', 
        format = colFormat(separators = TRUE), 
        align = 'center'
      )
    ), 
    style = list(fontSize = paste0(fntsz, 'px'), fontFamily = family),
    borderless = T, 
    resizable = T, 
    sortable = F,
    defaultColDef = colDef(
      headerStyle = list(fontSize = paste0(fntsz, 'px'), fontFamily = family),
      footerStyle = list(fontSize = paste0(fntsz, 'px'), fontFamily = family)
    ),
    theme = reactableTheme(
      headerStyle = list(borderColor = 'white')
    )
  )
  
  return(out)
  
}

# seagrass coverage plot, uses seagrass data obj from tbeptools
sgcov_plo <- function(seagrass, family){
    
  ##
  # data prep
  
  # extra years for padding
  exyrs <- seq(1950, 1953)
  
  toplo <- tibble(
    Year = c(exyrs, seq(1982, 2020))
  ) %>%
    left_join(seagrass, by = 'Year') %>%
    mutate(
      Acres = Acres / 1000
    )
  
  # label for last bar 
  lastlab <- seagrass %>% 
    filter(Year == max(Year)) %>% 
    pull(Acres) %>% 
    round(0) %>% 
    format(big.mark = ',') %>% 
    paste(., 'acres')
  
  # y loc for last bar label
  lasty <- seagrass %>% 
    filter(Year == max(Year)) %>% 
    pull(Acres) %>% 
    `/`(1000) %>% 
    `-`(1)
  
  ##
  # base ggplot
  
  # axis labels
  lbs <- toplo$Year
  lbs[lbs %in% exyrs[-1]] <- ''
  brks <- lbs
  lbs[as.numeric(lbs) %% 2 != 0] <- ''
  
  p <- ggplot(toplo, aes(x = factor(Year), y = Acres)) +
    with_shadow(geom_bar(fill = '#00806E', stat = 'identity', colour = 'black', width = 1.3), sigma = 2.7, x_offset = 0, y_offset = 0) +
    geom_segment(x = 0, xend = 2, y = 38, yend = 38, col = 'red', size = 2) +
    geom_segment(x = 4, xend = 42, y = 38, yend = 38, col = 'red', size = 2) +
    geom_segment(x = 42, xend = 44, y = 40, yend = 40, col = 'red', size = 2) +
    annotate("text", label = "Seagrass Coverage Goal", x = 4, y = 40.5, color = 'red', size = 5, hjust = 0, family = family) +
    annotate('text', x = 43, y = lasty, label = lastlab, angle = 90, hjust = 1, vjust = 0.4) + 
    scale_x_discrete(breaks = brks, labels = lbs, expand = c(0.04, 0.04)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1 * max(toplo$Acres, na.rm = T))) +
    theme_grey(base_family = family) +
    theme(
      axis.line = element_line(),
      panel.background = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.title.x = element_blank(),
      legend.position = 'none',
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    ) +
    labs(
      y = 'Seagrass Coverage (x1,000 acres)'
    )
  
  ##
  # top, bottom axis line breaks
  
  gt <- ggplotGrob(p)
  
  is_axisb <- which(gt$layout$name == "axis-b")
  is_axist <- which(gt$layout$name == "axis-t")
  is_axisl <- which(gt$layout$name == "axis-l")
  is_axisr <- which(gt$layout$name == "axis-r")
  
  axisb <- gt$grobs[[is_axisb]]
  xline <- axisb$children[[1]]
  
  # location of break, break type
  xline$y <- unit(rep(1, 4), "npc")
  xline$x <- unit(c(0, 0.06, 1, 0.105), "npc")
  xline$id <- c(1, 1, 2, 2)
  xline$arrow <- arrow(angle = 90, length = unit(0.07, 'inches'))
  
  axisb$children[[1]] <- xline
  axist <- xline
  axisl <- gt$grobs[[is_axisl]]
  
  gt$grobs[[is_axisb]] <- axisb
  gt$grobs[[is_axist]] <- axist
  gt$grobs[[is_axisr]] <- axisl$children[[1]]
  
  grid.newpage(); grid.draw(gt)
  
}

# summarize seagrass results by reference year
sgsum_fun <- function(seagrass, sgmaxyr, refyr = 1982){
  
  penult <- seagrass$Year[which(seagrass$Year == sgmaxyr) - 1]
  
  tocmp <- seagrass %>% 
    filter(Year %in% c(refyr, penult, sgmaxyr)) %>% 
    select(Year, Acres) %>% 
    pivot_wider(names_from = 'Year', values_from = 'Acres') %>% 
    unlist()
  
  refcmp <- tocmp[as.character(sgmaxyr)] - tocmp[as.character(refyr)]
  refdir <- ifelse(sign(refcmp) == 1, 'increased', 'decreased')
  
  reccmp <- tocmp[as.character(sgmaxyr)] - tocmp[as.character(penult)]
  recdir <- ifelse(sign(reccmp) == 1, 'increased', 'decreased')
  
  out <- list(
    refcmp =  formatC(round(abs(refcmp), 0), format = "d", big.mark = ","),
    refdir = refdir,
    reccmp =  formatC(round(abs(reccmp), 0), format = "d", big.mark = ","),
    recdir = recdir,
    penult = penult
  )
  
  return(out)
  
}

# alluvial plot function, for HMPU targets
# https://www.data-to-viz.com/graph/sankey.html
alluvout2 <- function(datin, fluccs, family, maxyr, width, height, mrg){
  
  ttl <- paste('True change analysis, watershed land use from 1990 (left) to', maxyr, '(right)')
  
  if(any(grepl('^Seagrass', datin$source)))
    ttl <- paste('True change analysis, subtidal habitats (all categories) from 1988 (left) to', maxyr, '(right)')
  
    
  clp <- fluccs %>%
    pull(HMPU_TARGETS) %>% 
    unique %>% 
    c('Coastal Uplands', .) %>% 
    sort
  
  sumdat <- datin %>% 
    rename(Acres = value) %>% 
    mutate(
      target = gsub(',\\s[0-9]+$', '', target),
      source = gsub(',\\s[0-9]+$', '', source)
    ) %>% 
    group_by(target, source) %>% 
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    na.omit() %>% 
    group_by(target, source) %>% 
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    select(source = source, target = target, value = Acres) %>% 
    data.frame(stringsAsFactors = F)
  sumdat$source <- paste(sumdat$source, " ", sep="")
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name=c(as.character(sumdat$source), as.character(sumdat$target)) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  sumdat$IDsource=match(sumdat$source, nodes$name)-1 
  sumdat$IDtarget=match(sumdat$target, nodes$name)-1
  
  # custom color scale
  cols <- c('#004F7E', '#00806E', '#427355', '#958984', '#5C4A42', 'grey') %>% 
    colorRampPalette
  ncol <- sumdat[, c('source', 'target')] %>% 
    unlist() %>% 
    unique %>% 
    gsub('\\s$', '', .) %>% 
    unique %>% 
    length()
  colin <- cols(ncol) %>% 
    paste(collapse = '", "') %>% 
    paste('d3.scaleOrdinal(["', ., '"])')
  
  # margins for long text labels
  mrgs <- list(0, mrg, 0, 0)
  names(mrgs) <- c('top', 'right', 'bottom', 'left')
  
  out <- sankeyNetwork(Links = sumdat, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget", colourScale = colin,
                       Value = "value", NodeID = "name", height = height, width = width, fontFamily = family,
                       sinksRight = F, units = 'acres', nodeWidth=50, fontSize=13, nodePadding=10, 
                       margin = mrgs)
  
  # add caption
  out <- htmlwidgets::prependContent(out, h5(class = "title", ttl))

  out <- htmlwidgets::onRender(
    out,
    '
    function(out,x){
    // select all our node text
    d3.select(out)
    .selectAll(".node text")
    .filter(function(d) { return d.name.endsWith(" "); })
    .attr("x", x.options.nodeWidth - 55)
    .attr("text-anchor", "end");
    }
    '
  )
  
  return(out)
  
}
