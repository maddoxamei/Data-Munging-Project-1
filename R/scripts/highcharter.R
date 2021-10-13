library(highcharter)

hc.line <- function(hc, xlab, ylab, zlab=NULL, ymax=NULL, xbands=NULL, ybands=NULL){
  hc<- hc %>%
    highcharter::hc_xAxis(
      title = list(text = xlab), plotBands = xbands ) %>%
    highcharter::hc_yAxis( min=0, max=ymax,
              title = list(text = ylab), plotBands = ybands) 
  return(hc)
}
  
hc.scatter <- function(hc, xlab, ylab, zlab=NULL, ymax=NULL, xbands=NULL, ybands=NULL){
  tooltip <- "<b>{point.name}</b>
          <br/>{series.xAxis.axisTitle.textStr}: {point.x}
          <br/>{series.yAxis.axisTitle.textStr}: {point.y}"
  hc <- hc.line(hc, xlab, ylab, zlab, ymax, xbands, ybands)
  hc %>%
    highcharter::hc_tooltip(
      pointFormat = if(is.null(zlab)) tooltip 
      else paste(tooltip, "<br/>",zlab,": {point.z}") ) %>% 
    highcharter::hc_plotOptions(bubble=list(minSize="1%",maxSize="10%"))
}

income.brackets <- list(
  list(label=list(text="Tier 1"), 
       color='rgba(68, 170, 213, 0.1)', from=0,to=10000),
  list(label=list(text="Tier 2"), 
       color='rgba(0, 0, 0, 0)', from=10000,to=40000),
  list(label=list(text="Tier 3"), 
       color='rgba(68, 170, 213, 0.1)', from=40000,to=85000),
  list(label=list(text="Tier 4"), 
       color='rgba(0, 0, 0, 0)', from=85000,to=160000),
  list(label=list(text="Tier 5"), 
       color='rgba(68, 170, 213, 0.1)', from=160000,to=200000),
  list(label=list(text="Tier 6"), 
       color='rgba(0, 0, 0, 0)', from=200000,to=500000),
  list(label=list(text="Tier 7"), 
       color='rgba(68, 170, 213, 0.1)', from=500000)
  )

# hchart(mtcars, 'scatter', hcaes(wt, mpg, z=qsec, group=cyl),
#        maxSize = "10%") %>% 
#   hc_colors(c("#00AFBB", "#E7B30", "#E7B800", "#FC4E07")) 


# hc_title(text="Time series plot of Inflation Rates for World",align="center") %>%
#   hc_subtitle(text="Data Source: IMF",align="center") %>%
#   hc_add_theme(hc_theme_elementary())
