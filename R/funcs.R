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
      xaxis = list(title = NA, gridcolor = '#FFFFFF')
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
    title = "TN vs Hydrology ratio\nTampa Bay total",
    tickfont = list(color = "blue"),
    overlaying = "y",
    side = "right"
  )
  
  out <- plot_ly(toplo, width = width, height = height) %>% 
    add_trace(x = ~yr, y = ~pop, color = I('tomato1'), type = 'bar', showlegend = T, name = 'Pop.') %>%
    add_trace(x = ~yr, y = ~tnhy, color = I('blue'), mode = 'lines+markers', type = 'scatter', showlegend = T, yaxis = 'y2', name = 'TN:hydrology') %>% 
    layout(
      yaxis = list(title = 'Population (millions)', tickfont = list(color = 'red')),
      yaxis2 = ay
    ) %>% 
    layout(
      plot_bgcolor='#e5ecf6',
      xaxis = list(
        title = NA,
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
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
