library(plotly)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- readRDS("Desktop/WQD7001 - Shiny App/tourism_yearly.rds")

fig <- df %>%
  filter(Country %in% c("China"))

fig <- fig %>% accumulate_by(~Year)

fig <- fig %>%
  plot_ly(
    x = ~Year, 
    y = ~Arrivals,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    fill = 'tozeroy',
    fillcolor = 'rgba(168,216,234,0.5)',
    line = list(simplyfy = F)
  )
fig <- fig %>% layout(
  xaxis = list(
    title = "Year",
    zeroline = F
  ),
  yaxis = list(
    title = "Number of Arrivals",
    zeroline = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

fig

