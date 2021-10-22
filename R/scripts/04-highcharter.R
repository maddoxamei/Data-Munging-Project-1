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
    #if( any(!is.na(data[data[[group]]==x,var])) ) next;
    density <- density(data[data[[group]]==x,var], na.rm=T)
    if( length(grep("e", max(density$y))) == 0 ) multiple <- -1/10
    else multiple <- -(as.numeric(gsub(".*e", "", max(density$y)))+1)
    idx <- match(x, levels(data[[group]]))-1
    ds <- c(ds, list(list(data = cbind(density$y*10^multiple+idx,density$x), name=x, type="area", zIndex=2),
                     list(data = cbind(-density$y*10^multiple+idx,density$x), name=x, type="area", zIndex=2)))
  }
  #if( is.null(ds) ) return( highchart() )
  hc <- highchart()%>% hc_xAxis(type='category')%>%
    hc_add_series_list(ds)%>%hc_chart(inverted=T)%>%
    hc_add_series_list(boxdata, zIndex=2) %>% 
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

plot.violin.2 <- function(hc, data, var, group, subgroup, boxdata){
  ds <- NULL
  # var <- deparse(substitute(var))
  # group <- deparse(substitute(group))
  if( !is.factor(data[[group]]) ) data[[group]] <- as.factor(data[[group]])
  if( !is.factor(data[[subgroup]]) ) data[[subgroup]] <- as.factor(data[[subgroup]])
  
  sub.len <- length(levels(data[[subgroup]]))
  if( sub.len == 1 ) x2.inc <- 0
  else x2.inc <- (1/sub.len)/1.73#1.73
  offset <- -2/3*sub.len + 10/3 - sub.len%%2/3
  
  x.idx <- 0
  for(x in levels(data[[group]])){
    
    for( x2 in levels(data[[subgroup]])){
      subset <- subset(data, data[[group]]==x & data[[subgroup]]==x2, select=var)
      x2.idx <- match(x2, levels(data[[subgroup]]))-1
      
      if( sum(!is.na(subset[[var]])) < 2 ) next
      density <- density(subset[[var]], na.rm=T)
      if( length(grep("e", max(density$y))) == 0 ) multiple <- -1/10
      else multiple <- -(as.numeric(gsub(".*e", "", max(density$y)))+1)
      idx <- x.idx - (x2.inc/offset) + (x2.idx*x2.inc)
      ds <- c(ds, list(list(data = cbind(density$y*10^multiple+idx,density$x), name=x, type="area", colorIndex=x2.idx, zIndex=2),
                       list(data = cbind(-density$y*10^multiple+idx,density$x), name=x, type="area", colorIndex=x2.idx, zIndex=2)))
      x2.idx <- x2.idx + 1
    }
    x.idx <- x.idx + 1
  }
  boxdata$zIndex <- 4
  hc %>% hc_xAxis(type='category')%>%
    hc_add_series_list(ds)%>%
    hc_add_series_list(boxdata) %>%
    hc_tooltip(
      headerFormat = '<b>{point.x} Years</b><br/>')%>%
    hc_plotOptions(area = list(fillOpacity=0.3,
                               lineWidth=0, 
                               linkedTo=':previous',
                               tooltip=list(
                                 headerFormat = '<b>{series.name}</b><br />',
                                 pointFormat='{series.yAxis.axisTitle.textStr}: {point.y}<br/>
                                              Density: {point.x}'),
                   scatter = list(tooltip=list(
                     headerFormat = '<b>{point.x} Years</b><br />',
                     pointFormat='{series.yAxis.axisTitle.textStr}: {point.y}'))))
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
                     title=NULL, subtitle=NULL,
                     valuePrefix = '$'){
  hc<- hc %>%
    highcharter::hc_xAxis( title = list(text = xlab)) %>%
    highcharter::hc_yAxis( title = list(text = ylab) ) %>%
    highcharter::hc_title(text=title ,align="center") %>%
    highcharter::hc_subtitle(text=subtitle,align="center") %>%
    highcharter::hc_tooltip(valuePrefix=valuePrefix, valueDecimals=2)
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

income.cutoffs <- c(0, 10000, 40000, 85000, 160000, 200000, 500000)
  
income.brackets <- lapply(1:length(income.cutoffs), function(x){
  list(label=list(text=paste("Tier",x)), 
       from=q[x], to=q[x+1], 
       color=if(x%%2==0) 'white' else 'rgba(68, 170, 213, 0.1)')})
