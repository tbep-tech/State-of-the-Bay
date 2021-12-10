#' plot total load as tn, hyd, or ratio, annual or monthly
ldtot_plo <- function(datin, yval = c('tn_load', 'hy_load', 'tnhy'), addlns = F, 
                      levs = c('All Segments (- N. BCB)', 'Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay'),
                      width = NULL, height = NULL){
  
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
      add_trace(x = ~dt, y = ~yv, color = I('blue'), mode = 'lines+markers', type = 'scatter', showlegend = F) %>% #, marker = list(opacity = 1, size = 4)) %>% 
      add_annotations(
        text = ~unique(bay_segment),
        x = 0.5,
        y = 1.2,
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
      plot_bgcolor = 'rgba(0,128,110, 0.1)' 
      # yaxis = list(gridcolor = '#FFFFFF')
    )
  
  return(out)
  
}

#' plot pop and tn/hy ratio
ldrat_plo <- function(totanndat, popdat, width = NULL, height = NULL){
  
  toplo <- totanndat %>% 
    filter(grepl('^All\\sSegments', bay_segment)) %>% 
    select(yr = year, tnhy) %>% 
    left_join(popdat, by = 'yr') %>% 
    mutate(
      pop = pop / 1e6
    )
  
  ay <- list(
    title = list(
      text = "TN vs Hydrology ratio\nTampa Bay total",
      font = list(color = "blue")
    ), 
    tickfont = list(color = "blue"),
    overlaying = "y",
    side = "right"
  )
  
  out <- plot_ly(toplo, width = width, height = height) %>% 
    add_trace(x = ~yr, y = ~pop, color = I('tomato1'), type = 'bar', showlegend = T, name = 'Pop.') %>%
    add_trace(x = ~yr, y = ~tnhy, color = I('blue'), mode = 'lines+markers', type = 'scatter', showlegend = T, yaxis = 'y2', name = 'TN:hydrology') %>% 
    layout(
      yaxis = list(
        title = list(text = 'Population (millions)', font = list(color = 'red')), 
        tickfont = list(color = 'red')
        ),
      yaxis2 = ay
    ) %>% 
    layout(
      plot_bgcolor = 'rgba(0,128,110, 0.1)',
      xaxis = list(
        title = NA,
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        xaxis = list(title = NA, gridcolor = 'rgba(0,128,110, 0)')
      ),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      )
    )
  
  return(out)
  
}

#' color function for tidal creek reactable table cells
colfun <- function(x){
  
  out <- case_when(
    x == 'No Data' ~ 'lightblue', 
    x == 'Monitor' ~ '#00ff00', 
    x == 'Caution' ~ 'yellow', 
    x == 'Investigate' ~ 'orange', 
    x == 'Prioritize' ~ 'coral'
  )
  
  return(out)
  
}

#' tidal creek reactable table
crkrct_tab <- function(dat, tidalcreeks, colfun){
    
  totab <- dat %>% 
    inner_join(tidalcreeks, by = c('id', 'wbid', 'JEI', 'class', 'name')) %>% 
    mutate(`Length (km)` = round(Creek_Length_m / 1000, 2)) %>% 
    filter(score != 'No Data') %>% 
    select(Name= name, wbid, JEI, `Length (km)`, monitor, caution, investigate, prioritize, score)

  out <- reactable(totab, 
            columns = list(
              score = colDef(
                style = function(value){
                  list(background = colfun(value))
                }), 
              `Length (km)` = colDef(
                aggregate = 'sum', 
                format = colFormat(digits = 2),
                cell = function(value) {
                  
                  width <- paste0(value / max(totab$`Length (km)`, na.rm = T) * 100, "%")
                  value <- format(value, width = 9, justify = "right")
                  bar <- div(
                    class = "bar-chart",
                    style = list(marginRight = "6px"),
                    div(class = "bar", style = list(width = width, backgroundColor = "#958984"))
                  )
                  div(class = "bar-cell", span(class = "number", value), bar)
                }
              ), 
              # wbid = colDef(
              #   aggregate = 'count'
              # ), 
              # JEI = colDef(
              #   aggregate = 'count'
              # ),
              monitor = colDef(
                aggregate = 'sum'
              ), 
              caution = colDef(
                aggregate = 'sum'
              ),  
              investigate = colDef(
                aggregate = 'sum'
              ),  
              prioritize = colDef(
                aggregate = 'sum'
              )
              
            ), 
            groupBy = 'score',
            filterable = T, pageSizeOptions = c(10, 20, nrow(totab)), defaultPageSize = 10,
            showPageSizeOptions = T, compact = T
  )
  
  return(out)
  
}

# reactable table function that works for supra/intertidal and subtidal
lngtrmtab_fun <- function(datin, colnm, typ = c('subtidal', 'supratidal'), yrsel = '1988', topyr = '2018', firstwidth = 240, estout = F){
  
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
      chgper = 100 * (maxyr - chgyr) / chgyr
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
  
  out <- reactable(
    totab, 
    columns = list(
      val = colDef(name = colnm, footer = 'Total', minWidth = firstwidth, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      chg = colDef(name = paste0(yrsel, '-', topyr, ' change'), minWidth = 140,
                   style = jsfun, class = 'sticky right-col-2', headerClass = 'sticky right-col-2', footerClass = 'sticky right-col-2'
      ), 
      chgper = colDef(name = '% change', minWidth = 85,
                      style = jsfun,
                      format = colFormat(suffix = '%', digits = 0), 
                      class = 'sticky right-col-1', headerClass = 'sticky right-col-1', footerClass = 'sticky right-col-1'
                      
      )
    ),
    defaultColDef = colDef(
      footer = function(values){
        if(!is.numeric(values))
          return()
        
        formatC(round(sum(values), 0), format= "d", big.mark = ",")
        
      },
      footerStyle = list(fontWeight = "bold"),
      format = colFormat(digits = 0, separators = TRUE), 
      minWidth = 80, resizable = TRUE
    ),
    defaultPageSize = nrow(sums),
    showPageSizeOptions = F,
    highlight = T,
    wrap = F
  )
  
  # add caption
  out <- htmlwidgets::prependContent(out, h5(class = "title", ttl))

  return(out)
  
}

# hmpu database projects table
rstdat_tab <- function(rstdat, maxyr){

  # data prep
  rstsum <- rstdat %>% 
    select(
      Year = `Year Reported`, 
      Category = `Habitat Type (basic ESA categories)(existing databases)`, 
      Acres, 
      Activity = `Basic Activity (Enhance/Rest)`, 
      `Linear Ft` = `Linear Feet`
      ) %>% 
    mutate(
      Category = case_when(
        Category == 'estuarine' ~ 'Estuarine', 
        Category == 'Upland' ~ 'Uplands',
        grepl('^Mix', Category) ~ 'Mixed', 
        T ~ Category
      )
    ) %>% 
    filter(Year <= maxyr) %>% 
    group_by(Category, Activity) %>% 
    summarise(
      tot= n(),
      Acres = sum(Acres, na.rm = T), 
      Feet = sum(`Linear Ft`, na.rm = T),
      .groups = 'drop'
    ) %>% 
    filter(!is.na(Category)) %>% 
    group_by(Category) %>% 
    mutate(
      tot = sum(tot)
    ) %>% 
    pivot_longer(c('Acres', 'Feet'), names_to = 'var', values_to = 'val') %>% 
    unite('var', Activity, var, sep = ', ') %>% 
    pivot_wider(names_from = 'var', values_from = 'val')

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
      footerStyle = list(fontWeight = "bold"),
      format = colFormat(digits = 0, separators = TRUE), 
      minWidth = 80, resizable = TRUE
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
reach_tab <- function(comdat, platform = c('Be Floridian FB', 'TBEP Facebook', 'TBEP IG', 'TBEP YouTube', 'TBEP Unsplash', 'Constant Contact'), 
                      maxyr, fntsz = 16, chg = TRUE){
  
  platform <- match.arg(platform)
  
  ics <- list(
    `Be Floridian FB` = list(  
      fct = c('Total Impressions', 'Total Engagements', 'Post Link Clicks', 'Total Fans'),
      icons = c('volume-up', 'heart', 'mouse-pointer', 'users')
      ), 
    `TBEP Facebook` = list(
      fct = c('Total Impressions', 'Total Engagements', 'Post Link Clicks', 'Total Fans'),
      icons = c('volume-up', 'heart', 'mouse-pointer', 'users') 
      ), 
    `TBEP IG` = list(
      fct = c('Total Impressions', 'Total Engagements', 'Profile Actions', 'Followers'),
      icons = c('volume-up', 'heart', 'mouse-pointer', 'users') 
      ),
    `TBEP YouTube` = list(
      fct = c('Total Views', 'Watch Time (Hours)', 'Subscriber gain/loss'),
      icons = c('film', 'clock', 'users')
      ), 
    `TBEP Unsplash` = list(
      fct = c('All Time Views', 'All Time Downloads'),
      icons = c('', '')
      ),
    `Constant Contact` = list(
      fct = c('Net new contacts', 'Number of Campaigns Sent', 'Click Rate', 'Open Rate'), 
      icons = c('', '', '', '')
      )
    )
  
  fct <- ics[[platform]]$fct
  icons <- ics[[platform]]$icons
  
  # table as change
  if(chg){
    
    cmpyr <- maxyr - 1
  
    sumdat <- comdat %>% 
      filter(platform %in% !!platform) %>% 
      filter(metric %in% fct) %>% 
      filter(year %in% c(maxyr, cmpyr)) %>% 
      group_by(metric, year) %>%
      summarise(
        val = sum(val), 
        .groups = 'drop'
      ) %>% 
      pivot_wider(names_from = 'year', values_from = 'val') %>% 
      rename(
        maxyr = !!as.character(maxyr), 
        cmpyr = !!as.character(cmpyr)
      ) %>% 
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
        metric = factor(metric, levels = fct),
        icons = factor(metric, levels = fct, labels = icons), 
        icons = as.character(icons)
      ) %>% 
      arrange(metric)
    
    out <- reactable(
      sumdat, 
      columns = list(
        icons = colDef(show = F),
        chgicon = colDef(show = F), 
        chgcols = colDef(show = F),
        metric = colDef(
          minWidth = 300,
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
      style = list(fontSize = paste0(fntsz, 'px')),
      borderless = T, 
      resizable = T, 
      theme = reactableTheme(
        headerStyle = list(borderColor = 'white')
      )
    )
    
  }
    
  if(!chg){
    
    sumdat <- comdat %>% 
      filter(platform %in% !!platform) %>% 
      filter(metric %in% fct) %>% 
      filter(year %in% !!maxyr) %>% 
      group_by(metric, year) %>%
      summarise(
        val = sum(val), 
        .groups = 'drop'
      ) %>% 
      pivot_wider(names_from = 'year', values_from = 'val') %>% 
      rename(
        maxyr = !!as.character(maxyr)
      ) %>% 
      mutate(
        metric = factor(metric, levels = fct),
        icons = factor(metric, levels = fct, labels = icons), 
        icons = as.character(icons)
      ) %>% 
      arrange(metric)
    
    out <- reactable(
      sumdat, 
      columns = list(
        icons = colDef(show = F),
        metric = colDef(
          minWidth = 300,
          name = '',
          cell = icon_sets(sumdat, icon_ref = "icons", icon_position = "left", icon_size = fntsz, colors = "black"), 
          align = 'right'
        ), 
        maxyr = colDef(
          name = as.character(maxyr), 
          format = colFormat(separators = TRUE), 
          align = 'center'
        )
      ), 
      style = list(fontSize = paste0(fntsz, 'px')),
      borderless = T, 
      resizable = T, 
      theme = reactableTheme(
        headerStyle = list(borderColor = 'white')
      )
    )
    
  }
  
  return(out)  

}

# reactable table for comms reach statistics, constant contact only
# icons guidance https://kcuilla.github.io/reactablefmtr/articles/icon_sets.html
ccreach_tab <- function(comdat, maxyr, fntsz = 16){
  
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
    style = list(fontSize = paste0(fntsz, 'px')),
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

  # these are tallies prior to 2021
  priors <- c(
      nevent = 14,
      npartner = 15,
      nvols = 581, # not differentiated between adult/youth
      nlbs = 1736,
      nplants = 216
    ) %>% 
    enframe('var', 'priorval')
  
  if(is.null(yrsel))
    out <- gaddat %>% 
      mutate(
        nvols = nadults + nyouth
      ) %>% 
      select(-event, -descrip, -lat, -lng, -nadults, -nyouth) %>% 
      pivot_longer(-year, names_to = 'var', values_to = 'val') %>% 
      group_by(var) %>% 
      summarise(
        val = sum(val, na.rm = T), 
        .groups = 'drop'
      ) %>% 
      full_join(priors, by = 'var') %>% 
      mutate(val = val + priorval) %>% 
      select(-priorval)
  
  if(!is.null(yrsel))
    out <- gaddat %>% 
      filter(year == yrsel) %>% 
      mutate(
        nvols = nadults + nyouth
      ) %>% 
      select(-event, -descrip, -lat, -lng) %>% 
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
gadsum_plo <- function(datin, h = 3, w = 15, padding = 0, rows = 5){

  txt <- tibble(
    name = c('nevent', 'nvols', 'nlbs', 'nplants', 'npartner'),
    info = c('Event areas are prioritized by the presence of excessive litter and native habitat degradation, often overlapping with neighborhoods that have historically not received the support to facilitate restorative activities.',
             paste(datin$nadults, 'adults and', datin$nyouth, 'youths helped to protect and resotre the bay this season.'),
             'Including trash, invasive plants & marine debris.', 
             "Native plants increase the bay's resiliency and restore crucial wildlife habitat.",
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
      icon = fontawesome(icon),
      font_family = 'fontawesome-webfont',
      name = factor(name, levels = rev(txt$name))
    ) %>%  
    arrange(name) %>% 
    mutate(
      x = rep(seq(0, (!!w + padding) * cols - 1, !!w + padding), times = rows),
      y = rep(seq(0, (!!h + padding) * rows - 1, !!h + padding), each = cols),
      info = str_wrap(info, 75)
    )

  p <-  ggplot(toplo, aes(x, y, height = h, width = w, label = info)) +
    geom_tile(aes(fill = name)) +
    geom_text(fontface = "bold", size = 10,
              aes(label = value, x = x - w/2.2, y = y + h/4, color = name), hjust = 0) +
    geom_text(size = 5, lineheight = 0.7,
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
  out <- bscols(
    leaflet(sd) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addMarkers(lng = ~lng, lat = ~lat, label = ~lab),
    datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%", rownames = F,
              options=list(deferRender=TRUE, scrollY=300, scroller=F, dom = 't', pageLength = nrow(tomap),
                           columnDefs = list(list(visible=FALSE, 
                                                  targets=c(0, 6, 7, 8))))
    )
  )
  
  return(out)
  
}
  
# get tberf funding summary
# datin is tberfdat
# yrsel not provided, get totals for all data
# yrsel provided will get totals for yrsel
tberfsum_fun <- function(datin, yrsel = NULL){
  
  if(!is.null(yrsel))
    datin <- datin %>% 
      filter(year == yrsel)

  out <- datin %>% 
    summarise(
      n = n(),
      total = sum(total, na.rm = T), 
      matching = sum(matching, na.rm = T), 
    ) %>% 
    mutate(
      total = round(total / 1e6, 1), 
      total = paste0('$', total, 'M'),
      matching = round(matching / 1e6, 1), 
      matching = paste0('$', matching, 'M')
    )
  
  return(out)
  
}