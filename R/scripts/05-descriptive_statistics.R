library(highcharter)
library(magrittr)

school.distribution <- function(){
  highchart() %>% 
    hc_add_series(count(csb.data, year, ccbasic), "column", 
                  hcaes(x=year, y=n, group = ccbasic), 
                  colorIndex = c(0:(nrow(ccbasic.labels)-1)), 
                  stacking='percent')%>%
    hc.label("Year", "Proportion","School Distribution", valuePrefix = '')%>%
    hc_xAxis(crosshair=T)%>%
    hc_tooltip(shared=T, useHTML=T,
               headerFormat='<b>{point.x}</b><br/>', 
               pointFormat='<span style="color:{point.color}">●</span>{series.name}:{point.percentage:.1f}%</b> ({point.y:.0f})<br/>')
}

earnings.percentiles <- function(){
  tb <- NULL
  for(years in c("06", "08", 10)){
    x <- NULL
    for(data in split(csb.data, csb.data$year)){
      low <- mean(data[[paste("pct10_earn_wne_p", years, sep = "")]], na.rm=T)
      q1 <- mean(data[[paste("pct25_earn_wne_p", years, sep = "")]], na.rm=T)
      median <- mean(data[[paste("md_earn_wne_p", years, sep = "")]], na.rm=T)
      q3 <- mean(data[[paste("pct75_earn_wne_p", years, sep = "")]], na.rm=T)
      high <- mean(data[[paste("pct90_earn_wne_p", years, sep = "")]], na.rm=T)
      if( is.nan(high) ) high <- -1
      x <- append(x, list(list(name=unique(data$year), low=low, q1=q1, median=median, q3=q3, high=high)))
    }
    tb <- dplyr::bind_rows(tb, list(tibble::as_tibble(list(name=paste(years, "Years"), data=list(x), id=years, type="boxplot"))))
  }
  
  highchart() %>%
    hc_add_series_list(tb)%>%
    hc_tooltip(headerFormat='<b>{point.x}</b><br/><span style="color:{point.color}">●</span> {series.name} After Entry<br/>',
               pointFormat='
                 90th Percentile: {point.high}<br/>
                 75th Percentile: {point.q3}<br/>
                 Median: {point.median}<br/>
                 25th Percentile: {point.q1}<br/>
                 10th percentile: {point.low}') %>% 
    hc.label("Year", "Earnings", 
             "Average Earnings after entry") %>% 
    hc_xAxis(categories=levels(as.factor(csb.data$year))) %>%
    hc.axis(ymin = 0, ybands = income.brackets)
}

violin.id.plots <- function(hc, var, id){
  data <- reshape2::melt(csb.data[grep(paste(var,"|",id, sep=''), colnames(csb.data))], id=id)
  plot.violin.2(hc, data, "value", id, "variable", 
                data_to_boxplot(data, value, id, variable))%>%
    hc_xAxis(categories=levels(data[[id]])) %>%
    hc.axis(ymin = 0, ymax = max(density(data$value, na.rm = T)$x),
            ybands = income.brackets)
}

earnings.comprehensive <- function(hc, var, id, type){
  data <- reshape2::melt(csb.data[grep(paste("sd|", id, sep=''), colnames(csb.data))], id=id) 
  colnames(data)[1] <- "id"
  data %<>%
    dplyr::group_by(id, variable) %>% dplyr::summarise(sd_mean=mean(value, na.rm=T))
  highchart() %>%
    violin.id.plots("mn",id)%>%
    hc_add_series(data=data, type=type,
                  hcaes(x=as.factor(id), y=sd_mean, group=variable),
                  colorIndex=c(0:(length(unique(data$variable))-1)), 
                  zIndex=if(type=="column") 0 else 5) %>%
    hc_plotOptions(line=list(connectNulls=T, dashStyle='ShortDashDot'),
                   column=list(color='white', borderColor='black',  
                               boarderWidth = 3, dashStyle='ShortDashDot'))
}

tuition.stream <- function(){
  highchart() %>% 
    #hc_yAxis_multiples(create_yaxis(naxis = 2, lineWidth = 2)) %>%
    hc_add_series(csb.data.year.ccbasic, "streamgraph", hcaes(year, tuitionfee_in_mean, group = ccbasic), 
                  yAxis=0, colorIndex = c(0:(nrow(ccbasic.labels)-1))) %>%
    hc_add_yAxis(title=list(text="In-State"), relative=2)%>%
    hc_add_series(csb.data.year.ccbasic, "streamgraph", hcaes(year, tuitionfee_out_mean, group = ccbasic), 
                  yAxis=1, colorIndex = c(0:(nrow(ccbasic.labels)-1))) %>%
    hc_add_yAxis(title=list(text="Out-of-State"), relative=2)%>%
    hc_tooltip(valuePrefix='$', valueDecimals=2,
               headerFormat='<b>{point.x}</b><br>
               <span style="color:{point.color}">●</span><b>{series.name}</b><br/>', 
               pointFormat='{series.yAxis.axisTitle.textStr}:{point.y}') %>%
    highcharter::hc_title(text="Tuition Cost",align="center")
}

cost.stream <- function(){
  highchart() %>% 
    #hc_yAxis_multiples(create_yaxis(naxis = 2, lineWidth = 2)) %>%
    hc_add_series(csb.data.year.ccbasic, "streamgraph", hcaes(year, costt4_a_mean, group = ccbasic), yAxis=0) %>%
    hc_add_yAxis(title=list(text="Academic Year"), relative=2)%>%
    hc_add_series(csb.data.year.ccbasic, "streamgraph", hcaes(year, costt4_p_mean, group = ccbasic), yAxis=1) %>%
    hc_add_yAxis(title=list(text="Program Year"), relative=2)%>%
    hc_tooltip(valuePrefix='$', valueDecimals=2,
               headerFormat='<b>{point.x}</b><br>', pointFormat='{series.yAxis.axisTitle.textStr}:{point.y}') %>%
    highcharter::hc_title(text="Cost of Attendence",align="center")
}

cost.to.earnings <- function(data, point, subtitle){
  highcharter::hchart(
    data, "scatter", highcharter::hcaes(tuitionfee_in_mean, 
                                                   mn_earn_wne_p10_mean, 
                                                   z = sd_earn_wne_p10_mean, 
                                                   group = ccbasic)) %>%
    hc.label("In-State Tuition","Mean Earnings","Cost to Earnings in 10 Years",subtitle) %>%
    hc.scatter("Standard Deviation of Earnings (Size)", point) %>%
    hc.axis(ymax=150000, ybands=income.brackets)
}

cost.to.earnings.line <- function(data,subtitle){
  x <- broom::augment(lm(mn_earn_wne_p10_mean~tuitionfee_in_mean+ccbasic, data=data)) %>% dplyr::arrange(tuitionfee_in_mean)
  hc <- highcharter::hchart(
    x, "line", highcharter::hcaes(tuitionfee_in_mean, 
                                        .fitted, 
                                        group = ccbasic))%>%
    hc.label("In-State Tuition","Mean Earnings","Cost to Earnings in 10 Years",subtitle)%>%
    hc.axis(ymax=150000, ybands=income.brackets)
  return(hc)
}
