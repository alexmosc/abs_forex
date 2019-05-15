
# clear environment

rm(list = ls()); gc()


## load libs

library(data.table)
library(ggplot2)
library(magrittr)
library(quantmod)
library(scales)

currs <- c(
     'eur',
     'chf',
     'gbp',
     'cad',
     'jpy',
     'rub',
     'cny',
     'cnh',
     'aud',
     'nok',
     'sek',
     'nzd',
     'try',
     'czk',
     'xau',
     'hkd',
     'mxn',
     'usd'
)

## download and bind historic rates for pairs --------------

rate.env <- new.env()

hist_day_length <- 180L

day_seq <- seq.Date(
     from = as.Date(Sys.Date() - hist_day_length)
     , to = as.Date(Sys.Date() - 1L)
     , by = 1
)

for(i in seq_along(currs)) for(j in seq_along(currs)[-seq_len(i)]){
     cat(currs[i], currs[j], "\n")
     quantmod::getFX(
          toupper(
               paste0(currs[i]
                      , '/'
                      , currs[j]
               )
          )
          , from = day_seq[1]
          , to = day_seq[length(day_seq)]
          , env = rate.env
     )
}

obs_rates = as.list(rate.env)


## run daily loop

simul_dt <- data.table()

for(
     i in as.Date(day_seq)
)
{

     l_obs_rates = log(
                         sapply(
                              obs_rates
                              , function(x)
                              {
                                   dt = as.data.table(x)
                                   
                                   val <- unlist(dt[index == i, 2, with = F])
                                   
                                   names(val) <- NULL
                                   
                                   val

                              }
                              )
                       )
     
     currs = toupper(currs)
     
     coef_matr = do.call(cbind, lapply(currs, function(currency){
          ifelse(grepl(paste0("^", currency), names(l_obs_rates)), 1,
                 ifelse(grepl(paste0(currency, "$"), names(l_obs_rates)), -1, 
                        0
                 ))
     }))
     
     colnames(coef_matr) = currs
     
     regr_dat = data.frame(l_obs_rates, coef_matr)
     
     # lm
     
     lm_mod = lm(l_obs_rates ~ . + 0, data = regr_dat)
     
     summary(lm_mod)
     
     lm_coefs <- exp(coef(lm_mod))
     
     lm_dt <- as.data.table(t(lm_coefs))
     lm_dt[, dates := i]
     
     simul_dt <- rbind(
          simul_dt
          , lm_dt
     )
     
     print(i)
     
}

simul_dt_melt <- 
     melt(
          simul_dt
          , id.vars = 'dates'
          , measure.vars = colnames(simul_dt)[!colnames(simul_dt) == 'dates']
     )

simul_dt_melt[, dates := as.Date(as.integer(dates))]

ggplot(
     data = simul_dt_melt
) +
     facet_wrap(
          ~ variable
          , scales = 'free'
          ) +
     geom_line(
          aes(
               x = dates
               , y = value
               , color = variable
          )
          , size = 1
          , alpha = 0.5
     ) +
     scale_x_date(breaks = '1 month') +
     theme_minimal()


## correlations --------

diff_vals <- simul_dt_melt[, value / shift(value, 1), by = variable] %>% na.omit(.)

cor.table <- data.table()

for(
     i in unique(diff_vals$variable)
)
{
     for(
          j in unique(diff_vals$variable)
     )
     {
          
          cor.table <- rbind(
               cor.table
               , data.table(
                    left_name = i
                    , right_name = j
                    , cor.val = cor(
                         diff_vals[variable == i, V1]
                         , diff_vals[variable == j, V1]
                    )
               )
          )
     }
     
}

diff_vals_corr <- 
     dcast(
          cor.table
          , left_name ~ right_name
          , sum
          , value.var = 'cor.val'
     )



