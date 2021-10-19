library(highcharter)
library(magrittr)

school.distribution <- function(){
  p<- ggplot2::ggplot(data = csb.data, 
                      ggplot2::aes(x = year, fill = as.factor(ccbasic))) + 
    ggplot2::geom_bar(position = "fill") + 
    ggplot2::labs(x = "Year", title = "Distribution of Schools") + 
    ggplot2::coord_flip() + 
    ggplot2::scale_fill_discrete(name="School Type")
  
  return(plotly::ggplotly(p, tooltip=c("x","y")))
}

earnings.percentiles <- function(){
  hc <- highchart()
  for(years in c("06", "08", 10)){
    low <- mean(csb.data[[paste("pct10_earn_wne_p", years, sep = "")]], na.rm=T)
    q1 <- mean(csb.data[[paste("pct25_earn_wne_p", years, sep = "")]], na.rm=T)
    median <- mean(csb.data[[paste("md_earn_wne_p", years, sep = "")]], na.rm=T)
    q3 <- mean(csb.data[[paste("pct75_earn_wne_p", years, sep = "")]], na.rm=T)
    high <- mean(csb.data[[paste("pct90_earn_wne_p", years, sep = "")]], na.rm=T)
    
    x <- list(list(name=years, low=low, q1=q1, median=median, q3=q3, high=high))
    hc %<>% hc_add_series_list(tibble::as_tibble(list(name=paste(years, "years"), data=list(x), id=NA, type="boxplot")))
  }
  
  hc%>%
    hc_tooltip(headerFormat='<span style="color:{point.color}">●</span><b>{series.name}</b><br/>',
               pointFormat='
                 90th Percentile: {point.high}<br/>
                 75th Percentile: {point.q3}<br/>
                 Median: {point.median}<br/>
                 25th Percentile: {point.q1}<br/>
                 10th percentile: {point.low}') %>% 
    hc.label("Years After Entry", "Earnings", 
             "Average Earnings after entry") %>% 
    hc_xAxis(labels=list(enabled=F))
}

earnings.sd <- function(){
  csb.data.sd <- reshape2::melt(csb.data[grep("sd", colnames(csb.data))])
  plot.violin(csb.data.sd, "value", "variable", 
              data_to_boxplot(csb.data.sd, value, variable, add_outliers = T))%>%
    hc.label("Years After Entry", "Earnings", "Standard Deviation of Earnings") %>%
    hc_xAxis(categories=gsub(".*p", "", levels(csb.data.sd$variable)))
}

tuition.stream <- function(){
  highchart() %>% 
    #hc_yAxis_multiples(create_yaxis(naxis = 2, lineWidth = 2)) %>%
    hc_add_series(csb.data.year.ccbasic, "streamgraph", hcaes(year, tuitionfee_in_mean, group = ccbasic), yAxis=0) %>%
    hc_add_yAxis(title=list(text="In-State"), relative=2)%>%
    hc_add_series(csb.data.year.ccbasic, "streamgraph", hcaes(year, tuitionfee_out_mean, group = ccbasic), yAxis=1) %>%
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
