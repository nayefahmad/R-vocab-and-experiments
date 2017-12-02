

#******************************************
# Function for basic data wrangling on data output from sql
#******************************************


# TODO: -------------

#*********************


clean.los <- function(df){
      
      # function takes a df, does some cleaning, outputs a df
      require("dplyr")
      require("tidyr")
      
      names(df) <- tolower(names(df))
      
      mutate(df, 
             admissionnursingunitcode = as.factor(admissionnursingunitcode), 
             adjustedadmissiondate = mdy(adjustedadmissiondate), 
             adjusteddischargedate = mdy(adjusteddischargedate), 
             transferdate = mdy(transferdate)) %>% 
            
            rename(ad.unitcode = admissionnursingunitcode,
                   from.unit = fromnursingunitcode, 
                   to.unit = tonursingunitcode, 
                   ad.date = adjustedadmissiondate, 
                   ad.time = adjustedadmissiontime, 
                   dis.date = adjusteddischargedate,
                   dis.time = adjusteddischargetime, 
                   t.date = transferdate, 
                   t.time = transfertime) %>% 
            unite(col = id, 
                  c(a.continuumid, accountnumber), 
                  sep = "-") %>% 
            mutate(id = as.factor(id)) %>% 
            
            # join dates and times: 
            unite(col = ad.dtime, 
                  c(ad.date, ad.time), sep=" ") %>%
            unite(col=dis.dtime, 
                  c(dis.date, dis.time), sep=" ") %>%
            unite(col=t.dtime, 
                  c(t.date, t.time), sep=" ") %>% 
            
            # change datetimes to date format: 
            mutate(ad.dtime = ymd_hm(ad.dtime), 
                   dis.dtime = ymd_hm(dis.dtime), 
                   t.dtime = ymd_hm(t.dtime))
      
}
