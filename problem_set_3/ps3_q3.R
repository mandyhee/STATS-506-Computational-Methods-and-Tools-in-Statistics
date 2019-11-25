#### Question 3 ######
data(mtcars)
mtcars = data.table(mtcars)

# part a ----------------------------------
dt = mtcars[ , .(mpg, cyl, disp, hp, wt)]

dt1 = dt[order(cyl) ,`:=`(disp_gc = disp - mean(disp), 
             hp_gc = hp - mean(hp), 
             wt_gc = wt - mean(wt)), 
          by = cyl
          ][, .(dispXmpg = sum(mpg*disp_gc), vdisp = var(disp_gc), 
                hpXmpg = sum(mpg*hp_gc), vhp = var(hp_gc), 
                wtXmpg = sum(mpg*wt_gc), vwt = var(wt_gc), n = .N), 
            by = cyl
            ][, `:=`(beta_cyl_disp = dispXmpg / {vdisp*{n-1}}, 
                     beta_cyl_hp = hpXmpg / {vhp*{n-1}}, 
                     beta_cyl_wt = wtXmpg / {vwt*{n-1}}), 
              by = cyl]

# part b ----------------------------------

regres_dt = function(dt, res, var, group){
  dt2 = dt[, .(get(res), get(var), get(group))]
  dt_beta = dt2[order(V3) ,.(V1, V2, var_gc = V2 - mean(V2)), by = V3
              ][, .(varXres = sum(V1*var_gc), v_var = var(var_gc), n = .N), by = V3
                    ][, `:=`(beta_var = varXres / {v_var*{n-1}}), by = V3]
  print(dt_beta)
}

## test ##
dt_disp = regres_dt(dt, 'mpg', 'disp', 'cyl')
dt_hp = regres_dt(dt, 'mpg', 'hp', 'cyl')
dt_wt = regres_dt(dt, 'mpg', 'wt', 'cyl')


#part c ----------------------------------
df = mtcars %>% select(mpg, cyl, disp, hp, wt)

#beta_cyl = df %>%
  #group_by(cyl) %>% 
  #mutate(disp_gc = disp - mean(disp), hp_gc = hp - mean(hp), wt_gc = wt - mean(wt)) %>%
  #summarize(dispXmpg = sum(mpg*disp_gc), vdisp = var(disp_gc), 
            #hpXmpg = sum(mpg*hp_gc), vhp = var(hp_gc), 
            #wtXmpg = sum(mpg*wt_gc), vwt = var(wt_gc), n = n()) %>%
  #mutate(beta_cyl_disp = dispXmpg / {vdisp*{n-1}}, 
         #beta_cyl_hp = hpXmpg / {vhp*{n-1}}, beta_cyl_wt = wtXmpg / {vwt*{n-1}})

## define function for summarize_at ##
Xmpg = function(a, b){
  return(sum(a*b))
}

df1 = df %>%
  group_by(cyl) %>% 
  mutate(disp_gc = disp - mean(disp), hp_gc = hp - mean(hp), wt_gc = wt - mean(wt)) %>%
  summarize_at(vars(c('disp_gc', 'hp_gc', 'wt_gc')), funs(Xmpg(., b=mpg), var, n = length)) %>%
  mutate(beta_cyl_disp = disp_gc_Xmpg / {disp_gc_var*{disp_gc_n-1}}, 
         beta_cyl_hp = hp_gc_var / {hp_gc_var*{hp_gc_n-1}}, 
         beta_cyl_wt = wt_gc_Xmpg / {wt_gc_var*{wt_gc_n-1}}) %>%
  select(-(disp_gc_n:wt_gc_n))



#part d ----------------------------------
regres_df = function(df, res, indep, group){
  df_beta = df %>%
    group_by(!!group) %>% 
    mutate(indep_gc =  !!indep - mean(!!indep)) %>%
    summarize_at('indep_gc', funs(Xmpg(., b = !!res), var, n = length)) %>%
    mutate(beta = Xmpg / {var*{n-1}})
  print(df_beta)
}

## test ##
res = sym("mpg")
group = sym("cyl")
disp = sym("disp")
hp = sym("hp")
wt = sym("wt")
df_disp = regres_df(df, res, disp, group)
df_hp = regres_df(df, res, hp, group)
df_wt = regres_df(df, res, wt, group)
