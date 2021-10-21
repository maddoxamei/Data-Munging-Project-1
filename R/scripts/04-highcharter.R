library(highcharter)
library(magrittr)

opts <- getOption("highcharter.lang")
opts$thousandsSep <- ","
options(highcharter.lang = opts)

plot.jitter <- function(data, scatter.hcaes, boxdata){
  hc <- highchart()%>% hc_xAxis(type='category')%>%
    hc_add_series_list(boxdata) %>% 
    hc_add_series(
      data = data, scatter.hcaes,
      type = "scatter")%>%
    hc_plotOptions(scatter = list(
      marker = list(
        radius = 3,
        symbol = "circle",
        lineWidth = 1
      )
    ))%>%
    hc_plotOptions(scatter = list(jitter = list(x = .1, y = 0)))%>%
    hc_tooltip(pointFormat='{series.yAxis.axisTitle.textStr}: {point.y}')
  return(hc)
}

plot.violin <- function(data, var, group, boxdata){
  ds <- NULL
  # var <- deparse(substitute(var))
  # group <- deparse(substitute(group))
  if( !is.factor(data[[group]]) ) data[[group]] <- as.factor(data[[group]])
  for(x in levels(data[[group]])){
    if( any(!is.na(data[data[[group]]==x,var])) ) next;
    density <- density(data[data[[group]]==x,var], na.rm=T)
    if( length(grep("e", max(density$y))) == 0 ) multiple <- -1/10
    else multiple <- -(as.numeric(gsub(".*e", "", max(density$y)))+1)
    idx <- match(x, levels(data[[group]]))-1
    ds <- c(ds, list(list(data = cbind(density$y*10^multiple+idx,density$x), name=x, type="area"),
                     list(data = cbind(-density$y*10^multiple+idx,density$x), name=x, type="area")))
  }
  if( is.null(ds) ) return( highchart() )
  hc <- highchart()%>% hc_xAxis(type='category')%>%
    hc_add_series_list(ds)%>%hc_chart(inverted=T)%>%
    hc_add_series_list(boxdata) %>% 
    hc_plotOptions(area = list(fillOpacity=0.3, color = '#437bcc',
                               lineWidth=0, 
                               enableMouseTracking=F,
                               linkedTo=':previous'),
                   boxplot = list(color="black", 
                                  tooltip = list(
                                    headerFormat = '<b>{point.x} Years</b><br />',
                                    pointFormat = 'Maximum: {point.high}<br/>Upper quartile: {point.q3}<br/>Median: {point.median}<br/>Lower quartile: {point.q1}<br/>Minimum: {point.low}<br/>')),
                   scatter = list(tooltip=list(
                     headerFormat = '<b>{point.x} Years</b><br />',
                     pointFormat='{series.yAxis.axisTitle.textStr}: {point.y}')))%>%
    hc_legend(enabled=F)
  return(hc)
}

plot.violin.jitter <- function(data, var, group, boxdata, scatter.hcaes){
  hc <- plot.violin(data, var, group, boxdata) %>%
    hc_add_series(data = data, scatter.hcaes,
                type = "scatter")%>%
    hc_plotOptions(scatter = list(
      marker = list(
        radius = 3,
        symbol = "circle",
        lineWidth = 1
      )
    ))%>%
    hc_plotOptions(scatter = list(jitter = list(x = .1, y = 0)))
  return(hc)
}

hc.label <- function(hc, xlab, ylab,
                     title=NULL, subtitle=NULL){
  hc<- hc %>%
    highcharter::hc_xAxis( title = list(text = xlab)) %>%
    highcharter::hc_yAxis( title = list(text = ylab) ) %>%
    highcharter::hc_title(text=title ,align="center") %>%
    highcharter::hc_subtitle(text=subtitle,align="center") %>%
    highcharter::hc_tooltip(valuePrefix='$', valueDecimals=2)
  return(hc)
}

hc.axis <- function(hc, ymin=NULL, ymax=NULL, 
                    xbands=NULL, ybands=NULL, ...){
  hc<- hc %>%
    highcharter::hc_xAxis( plotBands = xbands ) %>%
    highcharter::hc_yAxis( min=ymin, max=ymax, 
                           plotBands = ybands) 
  return(hc)
}
  
hc.scatter <- function(hc, zlab, boldtip=""){
  tooltip <- paste("<b>",boldtip,"</b>
          <br/>{series.xAxis.axisTitle.textStr}: {point.x}
          <br/>{series.yAxis.axisTitle.textStr}: {point.y}")
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

