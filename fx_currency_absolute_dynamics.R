
# clear environment

rm(list = ls()); gc()


## load libs

library(data.table)
library(ggplot2)
library(magrittr)
library(quantmod)
library(scales)


## set vars -------------

currs <- c(
     'usd',
     'eur',
     'chf',
     'gbp',
     'cad'
)

pairs <- c(
     'eurusd',
     'gbpusd',
     'eurchf',
     'eurgbp',
     'gbpchf',
     'usdchf',
     'cadchf',
     'gbpcad',
     'usdcad',
     'eurcad'
)


## download and bind historic rates for pairs --------------

hist_day_length <- 180L

day_seq <- seq.Date(
     from = as.Date(Sys.Date() - hist_day_length)
     , to = as.Date(Sys.Date() - 1L)
     , by = 1
     )

rate.env <- new.env()

for(
     x in pairs
)
{
     quantmod::getFX(
          toupper(
               paste0(substr(x, start=1, stop=3)
                      , '/'
                      , substr(x, start=4, stop=6)
               )
          )
          , from = day_seq[1]
          , to = day_seq[length(day_seq)]
          , env = rate.env
     )
}

rate_list <- lapply(
     ls(envir = rate.env)
     , function(x){
          
          dt <- as.data.table(get(x, envir = rate.env))
          
          names(dt) <- c(
               'dates',
               'bids'
          )
          
          dt[, pairs := tolower(x)]
          
          dt
     }
)

rates <- rbindlist(rate_list)


############
## RUN DYNAMIC PRICE LOOP -------------------------------

min_rand <- 0
max_rand <- 1
one_start <- 1

set.seed(111)

usd <- one_start# runif(1, min_rand, max_rand)
eur <- one_start# runif(1, min_rand, max_rand)
chf <- one_start# runif(1, min_rand, max_rand)
gbp <- one_start# runif(1, min_rand, max_rand)
cad <- one_start# runif(1, min_rand, max_rand)

# regularizer

alpha <- 1e-5

# symbolic task

express <- expression(
     (eurusd - eur / usd) ^ 2 +
          (gbpusd - gbp / usd) ^ 2 +
          (eurchf - eur / chf) ^ 2 +
          (eurgbp - eur / gbp) ^ 2 +
          (gbpchf - gbp / chf) ^ 2 +
          (usdchf - usd / chf) ^ 2 +
          
          (cadchf - cad / chf) ^ 2 +
          (gbpcad - gbp / cad) ^ 2 +
          (usdcad - usd / cad) ^ 2 +
          (eurcad - eur / cad) ^ 2 +
          (usd^2 + eur^2 + chf^2 + gbp^2 + cad^2) * alpha
)

# solution params

iter_max <- 1e+3

lr <- 1e-3

min_tolerance <- 0.000001

# define gradient

rm(grad_desc_func)

grad_desc_func <- function(
     lr,
     curr_list
)
{
     
     derivs <- character(length(curr_list))
     deriv_vals <- numeric(length(curr_list))
     grad_vals <- numeric(length(curr_list))
     
     # symbolic derivatives
     
     derivs <- sapply(
          curr_list,
          function(x){
               D(express, x)
          }
     )
     
     # derivative values
     
     deriv_vals <- sapply(
          derivs,
          function(x){
               eval(x)
          }
     )
     
     # gradient values
     
     grad_vals <- -deriv_vals * lr
     
     # gradient clipping to [-1;1]
     
     grad_vals[grad_vals > 1 | grad_vals < -1] <- 
          sign(grad_vals[grad_vals > 1 | grad_vals < -1])
     
     grad_vals
     
}

# result data table

simul_dt <- data.table()

for(
     s in day_seq
)
{
     
     # snapshot of values at time t
     
     pair_values <- split(rates[dates == s, .(pairs, bids)], by = 'pairs', keep.by = T)
     
     lapply(
          pair_values
          , function(x)
          {
               
               pair_name <- x[['pairs']]
               
               pair_value <- x[['bids']]
               
               assign(pair_name, pair_value, envir = .GlobalEnv)
               
          }
     )
     
     # get gradient values
     
     progress_list <- list()
     
     for(
          i in seq_len(iter_max)
     )
     {
          
          grad_deltas <- grad_desc_func(lr, curr_list = currs)
          
          currency_vals <- sapply(
               currs
               , function(x)
               {
                    
                    # update currency values
                    
                    current_val <- get(x, envir = .GlobalEnv)
                    
                    new_delta <- grad_deltas[x]
                    
                    new_val <- current_val + new_delta
                    
                    names(new_val) <- NULL
                    
                    # change values of currencies by gradient descent step in global env
                    
                    assign(x, new_val , envir = .GlobalEnv)
                    
                    # save history of values for later plotting
                    
                    new_val
                    
               }
          )
          
          progress_list[[i]] <- c(
               currency_vals, 
               eval(express)
                                  )
          
          if(
               eval(express) < min_tolerance
          )
          {
               
               break('solution was found')
               
          }
               
     }

     
     # check results 
     
     progress_dt <- rbindlist(
          lapply(
               progress_list
               , function(x)
               {
                    as.data.frame(t(x))
               }
          )
     )
     
     colnames(progress_dt)[length(colnames(progress_dt))] <- 'error'
     
     progress_dt[, steps := 1:nrow(progress_dt)]
     
     progress_dt_melt <-
          melt(
               progress_dt
               , id.vars = 'steps'
               , measure.vars = colnames(progress_dt)[colnames(progress_dt) != 'steps']
          )
     
     progress_dt_melt[, simul := as.Date(as.integer(s))]
     
     simul_dt <- rbind(
          simul_dt
          , progress_dt_melt
     )
     
     print(
          paste(
               'step = ', as.Date(as.integer(s))
          )
     )
     
}

setorder(simul_dt, variable, simul, steps)

final_simul_dt <- 
     simul_dt[
          , .(value = .SD[steps == max(steps), value])
          , by = .(simul, variable)
     ]

colnames(final_simul_dt) <- c("dates", "pairs", "bids")

final_simul_dt <- rbind(
     final_simul_dt
     , rates
)

ggplot(
     data = final_simul_dt[pairs != 'error']
       ) +
     facet_wrap(
          ~ pairs
          , scales = 'free'
          , ncol = 3) +
     geom_line(
          aes(
               x = dates
               , y = bids
               , color = pairs
               , group = pairs
          )
          , size = 1
          , alpha = 0.5
     ) +
     scale_x_date(breaks = '1 month') +
     theme_minimal()


# correlations

diff_vals <- final_simul_dt[pairs != 'error', diff(bids), by = pairs]

cor.table <- data.table()

for(
     i in unique(diff_vals$pairs)
    )
{
     for(
          j in unique(diff_vals$pairs)
     )
     {
     
     cor.table <- rbind(
          cor.table
          , data.table(
               left_name = i
               , right_name = j
               , cor.val = cor(
                    diff_vals[pairs == i, V1][-1]
                    , diff_vals[pairs == j, V1][-length(diff_vals[pairs == j, V1])]
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

