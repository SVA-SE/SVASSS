####----Data frames and objects

# D.F. number 1
z=data.frame(dates=ISOweek2date(BOV.weekly@dates[,1]),BOV.weekly@observed[,12])
#z=data.frame(dates=ISOweek2date(BOV.weekly@dates[BOV.weekly@dates$year%in%year,1]),BOV.weekly@observed[BOV.weekly@dates$year%in%year,12])

ggplot(z, aes(dates,BOV.weekly@observed[,12]))+
  geom_point(aes(group=BOV.weekly@observed[,12]))

#Object number 1
b <- plot_ly(z, x = ~dates, y = ~BOV.weekly@observed[,12])

subplot(
  add_lines(b, size = ~BOV.weekly@observed[,12], name = "default", color = I("black"),
            sizes = 1, fill = "tozeroy"))

#D.F. number 2
w=data.frame(dates=ISOweek2date(BOV.weekly@dates[,1]),BOV.weekly@baseline)

ggplot(w, aes(dates,Systemic))+
  geom_point(aes(group=Systemic))

#Object number 2
c <- plot_ly(w, x = ~dates, y = ~Systemic)

subplot(
  add_lines(c, size = ~Systemic, name = "default", color = I("black"),
            sizes = 1, fill = "tozeroy"))

#UCL D.F.

xyz=data.frame(dates=ISOweek2date(BOV.weekly@dates[,1]),BOV.weekly@UCL)
xyz2=data.frame(dates=ISOweek2date(BOV.weekly@dates[,1]),BOV.weekly@UCL*1.5)
xyz3=data.frame(dates=ISOweek2date(BOV.weekly@dates[,1]),BOV.weekly@UCL*2)


#Alarm D.F.

alarms=data.frame(dates=ISOweek2date(BOV.weekly@dates[,1]),BOV.weekly@alarms)

alarms_hw=data.frame(dates=ISOweek2date(BOV.weekly@dates[,1]),BOV.weekly@alarms[,12,1])
alarms_ewma=data.frame(dates=ISOweek2date(BOV.weekly@dates[,1]),BOV.weekly@alarms[,12,2])

ggplot(alarms, aes(dates,BOV.weekly@alarms[,12,1]))+
  geom_point(aes(group=BOV.weekly@alarms[,12,1]))

ggplot(alarms, aes(dates,BOV.weekly@alarms[,12,2]))+
  geom_point(aes(group=BOV.weekly@alarms[,12,2]))



####Potential SVASSS Plot----




p <- plot_ly(z, x = ~dates) %>% 
  add_trace(y = ~xyz3[,12],
            name = 'UCL3', type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            fillcolor='rgba(0,80,80,0.2)',
            fill = 'tozeroy',
            width = 1) %>%
  add_trace(y = ~xyz2[,12],
            name = 'UCL2', type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            fillcolor='rgba(0,90,80,0.2)',
            fill = 'tozeroy',
            width = 1) %>%
  add_trace(y = ~xyz[,12],
            name = 'UCL1', type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            fillcolor='rgba(0,100,80,0.2)',
            fill = 'tozeroy',
            width = 1) %>%
  add_trace(y = ~BOV.weekly@observed[,12],
            name = 'Systemic Obs', type = 'scatter', mode = 'lines+markers',
            line = list(shape = "linear")) %>%
  add_trace(y = ~w[,12],
            name = 'Baseline', type = 'scatter', mode = 'lines',
            linetype = I(1),
            line = list(shape = "spline", dash = 'dot')) %>%
  add_bars(y = ~BOV.weekly@alarms[,12,1],
           name = 'Alarms HW', type = 'scatter', mode = 'lines',
           marker = list(color = 'rgba(55, 128, 191, 0.7)',
                         line = list(color = 'rgba(55, 128, 191, 0.7)',
                                     width = 4))) %>%
  add_bars(y = ~BOV.weekly@alarms[,12,2],
           name = 'Alarms EWMA', type = 'scatter', mode = 'lines',
           marker = list(color = 'rgba(55, 128, 191, 0.7)',
                         line = list(color = 'rgba(219, 64, 82, 0.7)',
                                     width = 4))) %>%
  layout(title = "SVASSS Graphic",
         paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE)) %>%
  rangeslider()



subplot(p) #This runs the plot




####Testing of windows in new plot----

year <- c(2017:2018)
rows <- BOV.weekly@dates$year%in%year

window <- 80
end <- length(BOV.weekly@dates$year)
start <- end - window + 1
rows <- start:end




pp <- plot_ly(z, x = ~dates[rows], y = ~BOV.weekly@observed[rows,12],
             name = 'Systemic Obs', type = 'scatter',
             mode = 'lines') %>%
  add_trace(y = ~xyz3[,12],
            name = 'UCL', type = 'scatter', mode = 'lines',
            linetype = I(1),
            fill = 'tozeroy',
            width = 1) %>%
  add_trace(y = ~xyz2[,12],
            name = 'UCL', type = 'scatter', mode = 'lines',
            linetype = I(1),
            fill = 'tozeroy',
            width = 1) %>%
  add_trace(y = ~xyz[,12],
            name = 'UCL', type = 'scatter', mode = 'lines',
            linetype = I(1),
            fill = 'tozeroy',
            width = 1) %>%
  add_trace(y = ~w[,12],
            name = 'Baseline', type = 'scatter', mode = 'lines',
            linetype = I(1),
            line = list(shape = "spline")) %>%
  add_bars(y = ~BOV.weekly@alarms[,12,1],
           name = 'Alarms HW', type = 'scatter', mode = 'lines',
           marker = list(color = 'rgba(55, 128, 191, 0.7)',
                         line = list(color = 'rgba(55, 128, 191, 0.7)',
                                     width = 4))) %>%
  add_bars(y = ~BOV.weekly@alarms[,12,2],
           name = 'Alarms EWMA', type = 'scatter', mode = 'lines',
           marker = list(color = 'rgba(55, 128, 191, 0.7)',
                         line = list(color = 'rgba(219, 64, 82, 0.7)',
                                     width = 4))) %>%
  layout(title = "SVASSS Graphic",
         xaxis = list(title = ""),
         yaxis = list(title = "")) 


subplot(pp)
