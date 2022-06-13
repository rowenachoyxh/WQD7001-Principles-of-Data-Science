library(plotly)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- readRDS("Desktop/WQD7001 - Shiny App/tourism_monthly.rds")

fig <- df %>%
  filter(Country %in% c("China","India")) %>%
  filter(Year == 2015) 

fig$month <- match(fig$month,month.abb)


fig <- fig %>% accumulate_by(~month)


fig <- fig %>%
  plot_ly(
    x = ~month, 
    y = ~Arrivals,
    split = ~Country,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
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

