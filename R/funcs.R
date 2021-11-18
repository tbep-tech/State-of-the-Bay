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

#' plot tn, pop est, and ratio of the two
ldrat_plo <- function(totanndat, popdat, width = NULL, height = NULL){
  
  toplo <- totanndat %>% 
    filter(grepl('^All\\sSegments', bay_segment)) %>% 
    select(yr = year, tn_load) %>% 
    left_join(popdat, by = 'yr') %>%  
    mutate(
      lb_per_ind = tn_load * 2000 / pop
    )
  
  p1 <- plot_ly(toplo, height = height, width = width)  %>% 
    add_trace(x = ~yr, y = ~tn_load, color = I('blue'), mode = 'lines+markers', type = 'scatter', showlegend = F) %>% #, marker = list(opacity = 1, size = 4)) %>% 
    add_annotations(
      text = 'Nitrogen load',
      x = 0.5,
      y = 1.2,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    ) %>% 
    layout(
      yaxis = list(title = 'Tons'), 
      xaxis = list(title = '')
    )
  
  p2 <- plot_ly(toplo, height = height, width = width)  %>% 
    add_trace(x = ~yr, y = ~pop / 1e6, color = I('tomato1'), type = 'bar', showlegend = F) %>% #, marker = list(opacity = 1, size = 4)) %>% 
    add_annotations(
      text = 'Bay area population',
      x = 0.5,
      y = 1.2,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    ) %>% 
    layout(
      yaxis = list(title = 'Millions'), 
      xaxis = list(title = '')
    )
  
  p3 <- plot_ly(toplo, height = height, width = width)  %>% 
    add_trace(x = ~yr, y = ~lb_per_ind, color = I('lightblue'), type = 'bar', showlegend = F) %>% #, marker = list(opacity = 1, size = 4)) %>% 
    add_annotations(
      text = 'Nitrogen per individual',
      x = 0.5,
      y = 1.2,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    ) %>% 
    layout(
      yaxis = list(title = 'lbs / person'), 
      xaxis = list(title = '')
    )
  
  out <- subplot(p1, p2, p3, shareX = T, nrows = 3, shareY = F, titleY = T, margin = c(0.03)) %>% 
    layout(
      xaxis = list(title = NA, gridcolor = '#FFFFFF')
    )
  
  
  return(out)
  
}