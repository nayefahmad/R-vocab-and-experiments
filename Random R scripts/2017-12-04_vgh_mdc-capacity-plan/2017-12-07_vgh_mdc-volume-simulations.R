
#*******************************************
# MDC VOLUMES SIMULATIONS  
#*******************************************

# rm(list = ls())

source("2017-12-05_vgh_mdc-treatment-duration-and-volumes.R")
source("capacity-required-full-fy_function.R")
source("2017-12-04_ggplot-theme.R")


# TODO: ---------------
# > WRITE RESULTS 
# > fix axis labels 



#********************************************
# CURRENT STATE ANALYSIS: ---------- 
# > Sim results by treatment: -------------
sim1.results.by.treatment <- 
      do.call(rbind, 
              lapply(1:1000,
                     capacity.required.full.fy,  # fn name 
                           services.list = df2.volumes,  # other args 
                           durations.data = list1.durations, 
                           num.bays = 9,
                           hr.per.day = 10.25,
                           days.open = 260,
                           full.results = 3))  # %>% print 

sim1.results.summary.by.treatment <- 
      sim1.results.by.treatment %>% 
      group_by(replication) %>% 
      summarize(total.time = sum(total.time))
      
sim1.results.summary.by.treatment %<>% 
      mutate(x = rep(paste(nrow(sim1.results.summary.by.treatment), "replications"),
                     nrow(sim1.results.summary.by.treatment)))


# > graph the result:-----
y.min <- min(sim1.results.summary.by.treatment$total.time) %/% 100 *100
y.max <- max(sim1.results.summary.by.treatment$total.time)

p1.total.time.boxplot <- 
      ggplot(sim1.results.summary.by.treatment, aes(x=x, y=total.time)) + 
            geom_boxplot() + 
            stat_summary(fun.y = mean, col="red", geom="point") + 
            scale_y_continuous(limits = c(y.min, y.max),
                               breaks = seq(y.min, y.max, 100)) + 
            labs(title = "Distribution of total hours required to meet \nvolume level of FY2017", 
                 subtitle = "\n\n\nCapacity available = 21204 hours") + 
            mytheme; p1.total.time.boxplot





#********************************************
# FUTURE STATE ANALYSIS: ---------- 
# > Sim result by treatment: ----------
sim2.results.by.treatment <- 
      do.call(rbind, 
              lapply(1:1000,
                     capacity.required.full.fy,  # fn name 
                     services.list = df4.volumes,  # other args 
                     durations.data = list2.durations, 
                     num.bays = 9,
                     hr.per.day = 10.25,
                     days.open = 260,
                     full.results = 3))  # %>% print 

sim2.results.summary.by.treatment <- 
      sim2.results.by.treatment %>% 
      group_by(replication) %>% 
      summarize(total.time = sum(total.time))

sim2.results.summary.by.treatment %<>% 
      mutate(x = rep(paste(nrow(sim2.results.summary.by.treatment), "replications"),
                     nrow(sim2.results.summary.by.treatment)))


# > graph the result:-----
y.min <- min(sim2.results.summary.by.treatment$total.time) %/% 100 *100
y.max <- max(sim2.results.summary.by.treatment$total.time)

p2.total.time.boxplot <- 
      ggplot(sim2.results.summary.by.treatment, aes(x=x, y=total.time)) + 
      geom_boxplot() + 
      stat_summary(fun.y = mean, col="red", geom="point") + 
      scale_y_continuous(limits = c(y.min, y.max),
                         breaks = seq(y.min, y.max, 100)) + 
      labs(title = "Distribution of total hours required \nto meet future state volume", 
           subtitle = "\nED IV-antibiotics volume added to current state \nIV-antibiotics duration uniformly distributed between 0.5-1.5 hrs \nCapacity available = 21204 hours") + 
      mytheme; p2.total.time.boxplot


# FINAL RESULTS: ------
# single boxplot: ------
final.results <- data.frame(current.state = sim1.results.summary.by.treatment$total.time, 
                            future.state = sim2.results.summary.by.treatment$total.time)
final.results <- melt(final.results)

# set y-axis range: 
y.min <- min(sim1.results.summary.by.treatment$total.time) %/% 100 *100
y.max <- max(sim2.results.summary.by.treatment$total.time)

p3.combined.boxplot <- 
      ggplot(final.results, aes(x=variable, y=value)) + 
      geom_boxplot() + 
      # facet_wrap(~variable) +
      stat_summary(fun.y = mean, col="red", geom="point") + 
      scale_y_continuous(limits = c(y.min, y.max),
                         breaks = seq(y.min, y.max, 1000)) + 
      labs(title = "Distribution of total hours required", 
           subtitle = "\nFuture state: ED IV-antibiotics volume added \nCapacity available = 23985 hours") + 
      mytheme; p3.combined.boxplot 







#******************************************
# write current state results: -----------
write.csv(sim1.results.by.treatment, 
          file = paste0(output.path, "/2017-12-20_vgh_mdc-sim-results.csv"), 
          row.names = FALSE)


pdf(file = paste0(output.path, "/2017-12-20_vgh-mdc-total-fy-hours-distribution.pdf"), 
    height = 5, width = 7)
p1.total.time.boxplot
dev.off()


# write future state results: -----------
write.csv(sim2.results.by.treatment, 
          file = paste0(output.path, "/2017-12-20_vgh_mdc-sim-results-future.csv"), 
          row.names = FALSE)


pdf(file = paste0(output.path, "/2017-12-20_vgh-mdc-total-fy-hours-distribution-future.pdf"), 
    height = 5, width = 7)
p2.total.time.boxplot
dev.off()
