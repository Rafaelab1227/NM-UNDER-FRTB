# Set the directory of the folder

# Set seed to ensure that the results are reproducible
set.seed(1234)

# Required packages
req.packages <-
  c(
    "reshape2",
    "dplyr",
    "ggplot2",
    "tidyverse",
    "ggpubr",
    "scales",
    "ggthemes",
    "treemapify",
    "ggrepel",
    "readxl"
  )
new.packages <-
  req.packages[!(req.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
invisible(lapply(req.packages, library, character.only = TRUE))

# Load data
load("./Results/Results3.RData")

# Load description of models to change names
Descriptionmodels <- read_excel("./Data/Descriptionmodels.xlsx")

rm(RMSE)
# Load RMSE from results of partition 80% 20%
loadlocal = local({
  load("./Performance/Performance2/Performance.RData")
  environment()
})
tools:::makeLazyLoadDB(loadlocal, "RMSE")
lazyLoad('RMSE')

# Load time performance from results of partition 92.48% 7.58%
loadlocal = local({
  load("./Performance/Performance3/Performance.RData")
  environment()
})
tools:::makeLazyLoadDB(loadlocal, "model_time")
lazyLoad('model_time')

# Set directory to keep the results
setwd("./Final Results")


# 1. PLA test  ------------------------------------------------------
# Calculation of the Spearman Correlation and the Kolmogorov- Smirnov
PLAtest <- data.frame(
  id = NA,
  fr = NA,
  model = NA,
  rs = NA,
  ks = NA
)
# Definition of the subset of models
index <- c(3, 4, 5, 6, 7, 16)
# Training set
year_day <- max(data_nomods$date) - 365
pos_year <- which(data_nomods$date == year_day)

training.samples <- seq(1:(pos_year - 1))

# Iterate over each NMRF
for (i in 1:length(non_sample)) {
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  RTPL <- fr_m[-training.samples, , drop = FALSE][, 1]
  for (m in index) {
    if (!is.na(vars.ls[[i]][[m]][2])) {
      if (!is.na(vars.ls[[i]][[m]]$predictions)) {
        HPL <- vars.ls[[i]][[m]]$predictions$predictions
        rs <- cor(RTPL, HPL, method = "spearman")
        ks <- as.numeric(ks.test(RTPL, HPL)[[1]][1])
      } else{
        rs <- NA
        ks <- NA
      }
    } else{
      rs <- NA
      ks <- NA
    }
    test <- c(i, names(fr_m), paste("model.", m), rs, ks)
    PLAtest <- rbind(PLAtest, test)
  }
}


# 2. Definition of NMRF by zones -----------------------------------------------------
PLAtest <- PLAtest[-1, , drop = TRUE]
PLAtest$Zone <- ifelse(PLAtest$rs > 0.8 & PLAtest$ks < 0.09,
                       1,
                       ifelse(PLAtest$rs < 0.7 |
                                PLAtest$ks > 0.12, 3, 2))

PLAtest$Zone <-
  ifelse(is.na(PLAtest$rs) | is.na(PLAtest$ks), 4, PLAtest$Zone)

# Return best zone for each NMRF
PLAtest.r <- dcast(select(PLAtest,-ks,-rs), fr ~ model)
PLAtest.r$model.min <-
  colnames(PLAtest.r)[apply(PLAtest.r, 1, which.min)]
PLAtest.r$model.min <- str_replace_all(PLAtest.r$model.min, " ", "")
PLAtest.r$zone.min <-
  do.call(pmin, c(PLAtest.r[, 3:dim(PLAtest.r)[2]], list(na.rm = TRUE)))

# Ranking of time performance
time_rank <- select(model_time, Model, rank)

# Best model with less RMSE in  80% 20%, if more than one take the computational effciency as reference to rank and obtain the best option
names(RMSE) <-
  gsub(
    x = names(RMSE),
    pattern = ("results.|.RMSE"),
    replacement = ""
  )
names(RMSE) <- str_replace_all(names(RMSE), " ", "")
RMSE$model.min.RMSE2 <-
  colnames(RMSE[, 9:14])[apply(RMSE[, 9:14], 1, which.min)]
RMSE$model.min.RMSE <- NA

for (i in (1:dim(RMSE)[1])) {
  RMSE.min <- min(RMSE[i, 9:14], na.rm = TRUE)
  RMSE.min.name <- colnames(RMSE[which(RMSE[i, ] == RMSE.min)])
  if (length(RMSE.min.name) > 1) {
    subset_time <- time_rank[which(time_rank$Model %in% RMSE.min.name), ]
    RMSE.min.name <-
      subset_time[subset_time$rank == max(subset_time$rank), 1]
    RMSE[i, match('model.min.RMSE', colnames(RMSE))] <-
      RMSE.min.name
  } else{
    RMSE[i, match('model.min.RMSE', colnames(RMSE))] <- RMSE.min.name
  }
}



PLAtest.min <- PLAtest.r
names(PLAtest.min) <- str_replace_all(names(PLAtest.min), " ", "")
#Column to put the minimum model, changes from model.min only there is more than model with the same min.zone
PLAtest.min$fmodel <- NA


for (i in (1:dim(PLAtest.min)[1])) {
  zmin <- PLAtest.min[i, match('zone.min', colnames(PLAtest.min))]
  fr <- PLAtest.min[i, match('fr', colnames(PLAtest.min))]
  zones <-
    colnames(PLAtest.min[i, 2:7][which(PLAtest.min[i, 2:7] == zmin)])
  # If more than one returns the best zone take the one with minimum RMSE in the 80% and 20%
  if (length(zones) > 1) {
    RMSE.min <- min(RMSE[RMSE$fr == fr, which(colnames(RMSE) %in% zones)])
    RMSE.min.name <-
      colnames(RMSE[RMSE$fr == fr, ][which(RMSE[i, ] == RMSE.min)])
    if (length(RMSE.min.name) > 1) {
      # Ranking based on time, picks the model which returns the best time for all the calculations
      subset_time <-
        time_rank[which(time_rank$Model %in% RMSE.min.name), ]
      RMSE.min.name <-
        subset_time[subset_time$rank == max(subset_time$rank), 1]
      PLAtest.min[i, match('fmodel', colnames(PLAtest.min))] <-
        RMSE.min.name
    } else{
      PLAtest.min[i, match('fmodel', colnames(PLAtest.min))] <-
        RMSE.min.name
    }
  } else{
    PLAtest.min[i, match('fmodel', colnames(PLAtest.min))] <-
      PLAtest.min[i, match('model.min', colnames(PLAtest.min))]
  }
}

#Left join of the model which achieved the minimum RMSE in the 80% train and 20% test partition
PLAtest.min <-
  left_join(PLAtest.min, select(RMSE, fr, model.min.RMSE , non_sample))

# Summary of stable cases
PLAtest.RMSE.stab <- PLAtest.min
PLAtest.RMSE.stab$stab <-
  ifelse(PLAtest.RMSE.stab$fmodel == PLAtest.RMSE.stab$model.min.RMSE,
         1,
         0)

PLAtest.RMSE.stab <- PLAtest.RMSE.stab %>% summarise(sum(stab))

# Summary of model choose by the minimum zone and the minimum RMSE
PLAtest.RMSE <-
  PLAtest.min %>% group_by(fmodel, model.min.RMSE) %>% summarise(count =
                                                                   n())

PLAtest.RMSE <-
  PLAtest.RMSE %>% group_by(fmodel) %>% mutate(countT = sum(count, na.rm =
                                                              TRUE)) %>%
  group_by(fmodel, model.min.RMSE, add = TRUE) %>%
  mutate(per = paste0(round(100 * count / countT, 1), '%'), label = per)
# Change names
names(PLAtest.RMSE)[1:2] <- c('Model.best.zone', 'Model.min.RMSE')
# Order data frame
PLAtest.RMSE <-
  PLAtest.RMSE[with(PLAtest.RMSE, order(Model.best.zone, count)),]

#Comparison of  number of NMRF by model based on minimum RMSE and best zone
pla_plot_RMSE.MIN <-
  ggplot(PLAtest.RMSE,
         aes(Model.best.zone, Model.min.RMSE, fill = count)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  xlab("Best zone") + ylab("Minimum RMSE") +
  scale_fill_gradient(low = "powderblue", high = "steelblue4") +
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = NULL,
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 10),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
  )
pla_plot_RMSE.MIN

jpeg(
  "plot_f.jpeg",
  width = 10,
  height = 3,
  units = 'in',
  res = 300
)
pla_plot_RMSE.MIN
dev.off()

# Definition of results
Min.vars.ls <- sapply(as.character(non_sample), function(x)
  NULL)

Methods <- select(PLAtest.min, fr, fmodel, non_sample, zone.min)
Methods$Zone <-
  ifelse(Methods$zone.min == "1",
         "GREEN",
         ifelse(
           Methods$zone.min == "2",
           "AMBER",
           ifelse(Methods$zone.min == "3", "RED", "NA")
         ))


# Data frame with NMRF and predictors for creating proxies
Predictors <- data.frame(NULL)

#Creation of list of the results based on the picked method for each NMRF
for (i in 1:dim(Methods)[1]) {
  model.min <-
    as.numeric(gsub(
      x = Methods[i, 2],
      pattern = ("model."),
      replacement = ""
    ))
  var <- Methods[i, 3] - 1
  #Model 16 presents a different structure for the results, for that it will be transform
  if (model.min != 16) {
    Min.vars.ls[[i]] <- vars.ls[[var]][[model.min]]
    Min.vars.ls[[i]]$model <- Methods[i, 2]
    # Dataframe to keep predictors
    datatemp <-
      cbind(Methods[i, 1],
            Min.vars.ls[[i]]$model,
            vars.ls[[var]][[model.min]]$results$var_coef[[1]])
    Predictors <- rbind(Predictors, datatemp)
  } else{
    vars_Selected <- vars.ls[[var]][[model.min]]$results$var_select
    linear <- vars.ls[[var]][[model.min]]$results$var_linear
    non_linear <- vars.ls[[var]][[model.min]]$results$var_nonlinear
    Min.vars.ls[[i]]$results$names_linear <- vars_Selected[linear]
    Min.vars.ls[[i]]$results$names_nonlinear <-
      vars_Selected[non_linear]
    Min.vars.ls[[i]] <- vars.ls[[var]][[model.min]]
    Min.vars.ls[[i]]$model <- Methods[i, 2]
    # Dataframe to keep predictors
    vars_selected_no <-
      unique(c(vars_Selected[linear], vars_Selected[non_linear]))
    datatemp <-
      cbind(Methods[i, 1], Min.vars.ls[[i]]$model, vars_selected_no)
    colnames(datatemp) <- c('V1', 'V2', 'V3')
    Predictors <- rbind(Predictors, datatemp)
  }
  
}


#3. Characterization of predictors ------------------------------------------
Predictors1 <- Predictors
names(Predictors) <- c('fr', 'model', 'pred_fr')
mod_n_pred <- mod_n
names(mod_n_pred) <- paste0('pred_', colnames(mod_n_pred))
Predictors <- left_join(Predictors, mod_n_pred)
Predictors <- Predictors[Predictors$pred_fr != '(Intercept)', ]

#Join with characteristics of the NMRF
Predictors <- left_join(Predictors, nomod_n)

# Characterization based on Type of NMRF and type of predictors

Predictors.type <-
  ungroup(Predictors) %>% group_by(type, pred_type) %>% summarise(count = n())

Predictors.type <-
  Predictors.type %>% group_by(type) %>% mutate(countT = sum(count, na.rm =
                                                               TRUE)) %>%
  group_by(type, pred_type, add = TRUE) %>%
  mutate(
    per = paste0(round(100 * count / countT, 1), '%'),
    label = paste0(count, ", (", per, ")"),
    percentage = round(100 * count / countT, 1)
  )

pred_plot_type <-
  ggplot(Predictors.type, aes(type, pred_type, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  xlab("NMRF type") + ylab("Selected MRF type") +
  scale_fill_gradient(low = "powderblue", high = "steelblue4") +
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = 'none',
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
  )
pred_plot_type



# Characterization based on Term of NMRF and term of predictors

Predictors.term <-
  ungroup(Predictors) %>% group_by(term, pred_term) %>% summarise(count = n())

Predictors.term <-
  Predictors.term %>% group_by(term) %>% mutate(countT = sum(count, na.rm =
                                                               TRUE)) %>%
  group_by(term, pred_term, add = TRUE) %>%
  mutate(
    per = paste0(round(100 * count / countT, 1), '%'),
    label = paste0(count, ", (", per, ")"),
    percentage = round(100 * count / countT, 1)
  )

pred_plot_term <-
  ggplot(Predictors.term, aes(term, pred_term, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  xlab("NMRF term") + ylab("Selected MRF term") +
  scale_fill_gradient(low = "powderblue", high = "steelblue4") +
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = 'none',
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
  )
pred_plot_term


# Characterization based on the currency of NMRF and the currency of predictors

Predictors.cur <-
  ungroup(Predictors) %>% group_by(currency, pred_currency) %>% summarise(count = n())

Predictors.cur <-
  Predictors.cur %>% group_by(currency) %>% mutate(countT = sum(count, na.rm =
                                                                  TRUE)) %>%
  group_by(currency, pred_currency, add = TRUE) %>%
  mutate(
    per = paste0(round(100 * count / countT, 1), '%'),
    label = per,
    percentage = round(100 * count / countT, 1)
  )

pred_plot_cur <-
  ggplot(Predictors.cur, aes(currency, pred_currency, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  xlab("NMRF currency") + ylab("Selected MRF currency") +
  scale_fill_gradient(low = "powderblue", high = "steelblue4") +
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = 'none',
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
  )
pred_plot_cur

jpeg(
  "plot_p_2.jpeg",
  width = 8,
  height = 3,
  units = 'in',
  res = 300
)
pred_plot_cur
dev.off()

# Characterization based on the type of NMRF and the term of predictors
Predictors.type.term <-
  ungroup(Predictors) %>% group_by(type, pred_type, term) %>% summarise(count = n())

Predictors.type.term <-
  Predictors.type.term %>% group_by(term) %>% mutate(countT = sum(count, na.rm =
                                                                    TRUE)) %>%
  group_by(type, pred_type, term, add = TRUE) %>%
  mutate(
    per = paste0(round(100 * count / countT, 1), '%'),
    label = paste0(count, ", (", per, ")"),
    percentage = round(100 * count / countT, 1)
  )

pred_plot_type_term <-
  ggplot(Predictors.type.term, aes(type, pred_type, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  xlab("NMRF type") + ylab("Selected MRF type") +
  scale_fill_gradient(low = "powderblue", high = "steelblue4") +
  facet_wrap( ~ factor(term),  ncol = 2, strip.position = "bottom") +
  theme(
    panel.background = element_rect(
      fill = "white",
      colour = "#6D9EC1",
      size = 2
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text.x =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

pred_plot_type_term



# Characterization based on the type of NMRF and the type of predictors, considering term in both cases
Predictors.ftype.term <-
  ungroup(Predictors) %>% group_by(type, pred_type, term, pred_term) %>% summarise(count = n())

Predictors.ftype.term <-
  Predictors.ftype.term %>% group_by(term, type) %>% mutate(countT = sum(count, na.rm =
                                                                           TRUE)) %>%
  group_by(type, pred_type, term, pred_term, add = TRUE) %>%
  mutate(
    per = paste0(round(100 * count / countT, 1), '%'),
    label = paste0(count, ", (", per, ")"),
    percentage = round(100 * count / countT, 1)
  )

levels(Predictors.ftype.term$term) <- c('NMRF:lt', 'NMRF:st')
levels(Predictors.ftype.term$type) <- c('NMRF:CCS', 'NMRF:SC')

pred_plot_ftype_term <-
  ggplot(Predictors.ftype.term,
         aes(pred_type, pred_term, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  xlab("Selected MRF type") + ylab("Selected MRF term") +
  scale_fill_gradient(low = "powderblue", high = "steelblue4") +
  facet_grid(factor(type) ~ factor(term)) +
  theme(
    panel.background = element_rect(
      fill = "white",
      colour = "#6D9EC1",
      size = 2
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text.x =  element_text(size = 12, color = "white"),
    strip.text.y =  element_text(size = 12, color = "white"),
    axis.text = element_text(size = 15)
  )

pred_plot_ftype_term

jpeg(
  "plot_p_1.jpeg",
  width = 10,
  height = 5,
  units = 'in',
  res = 300
)
pred_plot_ftype_term
dev.off()


# Characterization based on the type of NMRF and the type of predictors, considering currency in both cases
Predictors.fcur.type <-
  ungroup(Predictors) %>% group_by(type, pred_type, currency, pred_currency) %>%
  summarise(count = n())

Predictors.fcur.type <-
  Predictors.fcur.type %>% group_by(currency, type) %>% mutate(countT = sum(count, na.rm =
                                                                              TRUE)) %>%
  group_by(type, pred_type, currency, pred_currency, add = TRUE) %>%
  mutate(
    per = paste0(round(100 * count / countT, 0), '%'),
    label = per,
    percentage = round(100 * count / countT, 1)
  )

levels(Predictors.fcur.type$type) <- c('NMRF:CCS', 'NMRF:SC')

pred_plot_fcur_type <-
  ggplot(Predictors.fcur.type,
         aes(pred_type, pred_currency, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  xlab("Selected MRF type") + ylab("Selected MRF currency") +
  scale_fill_gradient(low = "powderblue", high = "steelblue4") +
  facet_grid(factor(type) ~ factor(currency)) +
  theme(
    panel.background = element_rect(
      fill = "white",
      colour = "#6D9EC1",
      size = 2
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text.x =  element_text(size = 10, color = "white"),
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

pred_plot_fcur_type

jpeg(
  "plot_p_3.jpeg",
  width = 13,
  height = 6,
  units = 'in',
  res = 300
)
pred_plot_fcur_type
dev.off()

# Characterization based on the type of NMRF and the type of predictors, considering currency in both cases
Predictors.fcur.type2 <-
  ungroup(Predictors) %>% group_by(type, pred_type, currency, pred_currency) %>%
  summarise(count = n())

Predictors.fcur.type2 <-
  Predictors.fcur.type2 %>% group_by(type, pred_type) %>% mutate(countT =
                                                                   sum(count, na.rm = TRUE)) %>%
  group_by(type, pred_type, currency, pred_currency, add = TRUE) %>%
  mutate(
    per = paste0(round(100 * count / countT, 0), '%'),
    label = per,
    percentage = round(100 * count / countT, 1)
  )

pred_plot_fcur_type2 <-
  ggplot(Predictors.fcur.type2,
         aes(currency, pred_currency, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  xlab("NMRF currency and type") + ylab("Selected MRF currency and type") +
  scale_fill_gradient(low = "powderblue", high = "steelblue4") +
  facet_grid(factor(pred_type) ~ factor(type)) +
  theme(
    panel.background = element_rect(
      fill = "white",
      colour = "#6D9EC1",
      size = 2
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text.x =  element_text(size = 10, color = "white"),
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

pred_plot_fcur_type2

#4. Characterization of the NMRF by best zone---------------------------------------------

# Composition of NMRF by zone
PLAtest.res <-
  PLAtest.r %>% group_by(zone.min) %>% summarise(count = n())
PLAtest.res$zone.min <-
  ifelse(
    PLAtest.res$zone.min == "1",
    "GREEN",
    ifelse(
      PLAtest.res$zone.min == "2",
      "AMBER",
      ifelse(PLAtest.res$zone.min == "3", "RED", "NA")
    )
  )
PLAtest.res <-
  mutate(
    PLAtest.res,
    percentage = count / sum(count),
    label = paste0(count, ", ", scales::percent(round(percentage, digits =
                                                        2)))
  )

plot_f_1 <-
  ggplot(PLAtest.res, aes(x = 2, y = percentage, fill = zone.min)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_label(aes(y = percentage, label = label, size = 40),
             color = "white",
             show.legend = FALSE) +
  scale_fill_manual(
    name = "Zone",
    values = c("darkgoldenrod3", "darkolivegreen3", "coral2"),
    labels = c("Amber", "Green", "Red")
  ) +
  theme_void()

plot_f_1

jpeg(
  "plot_f_1.jpeg",
  width = 4,
  height = 3,
  units = 'in',
  res = 300
)
plot_f_1
dev.off()

# Need a separation based on currency because of the units of the NMRFs
fr_m <- data_nomods[, i, drop = FALSE]

non_sample_cur <- data.frame(non_sample)
non_sample_cur$fr <- NA

for (i in 1:length(non_sample)) {
  non_sample_cur[i, 2] <- colnames(data_nomods)[non_sample_cur[i, 1]]
}

non_sample_cur <- left_join(non_sample_cur, nomod_n)


# Definition of the minimm zone
PLAtest_t <- PLAtest.r
PLAtest_t <- left_join(PLAtest_t, non_sample_cur)
PLAtest_t$zone.min <-
  ifelse(
    PLAtest_t$zone.min == "1",
    "GREEN",
    ifelse(
      PLAtest_t$zone.min == "2",
      "AMBER",
      ifelse(PLAtest_t$zone.min == "3", "RED", "NA")
    )
  )

#Proxies of NMRF classified by their best zone per  term and type
# Proxies of NMRF classified by their best zone and term
tot_term <- PLAtest_t %>% group_by() %>% mutate(countT = n()) %>%
  group_by(zone.min, term, countT, add = TRUE) %>%
  summarise(count = n()) %>%
  mutate(per = paste0(round(100 * count / countT, 1), '%'),
         label = paste0(count, ", (", per, ")"))


pla_plot_term <- tot_term %>%
  ggplot(aes(x = reorder(term, count), y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(
    zone.min, levels = c("GREEN", "AMBER", "RED")
  ))) +
  scale_fill_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2"),
    labels = c("Green", "Amber", "Red")
  ) +
  geom_label_repel(
    aes(
      label = reorder(label, factor(zone.min)),
      color = factor(zone.min, levels = c("GREEN", "RED", "AMBER"))
    ),
    hjust = 0.5,
    position = position_dodge(width = 0),
    vjust = -0.25,
    show.legend = FALSE,
    force = 1
  ) +
  scale_color_manual(
    values = c("darkolivegreen3", "coral2", "darkgoldenrod3"),
    labels = NULL
  ) +
  ylim(0, 2500) +
  xlab("Term") + ylab("NMRF") + ggtitle("Total methodology NMRF proxies by term") +
  theme(
    panel.background = element_rect(
      fill = "#BFD5E3",
      colour = "#6D9EC1",
      size = 2,
      linetype = "solid"
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

pla_plot_term

# Proxies of NMRF classified by their best zone and type

tot_type <-
  ungroup(PLAtest_t) %>% group_by() %>% mutate(countT = n()) %>%
  group_by(zone.min, type, countT, add = TRUE) %>%
  summarise(count = n()) %>%
  mutate(per = paste0(round(100 * count / countT, 1), '%'),
         label = paste0(count, ", (", per, ")"))

#tot_type2 <- tot_type[order(tot_type$count, decreasing = TRUE),]

pla_plot_type <- tot_type %>%
  ggplot(aes(x = type, y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(
    zone.min, levels = c("GREEN", "AMBER", "RED")
  ))) +
  scale_fill_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2"),
    labels = c("Green", "Amber", "Red")
  ) +
  geom_label_repel(
    aes(label = label, color = factor(
      zone.min, levels = c("GREEN", "AMBER", "RED")
    )),
    hjust = -0.20,
    position = position_dodge(width = 0),
    vjust = 1,
    show.legend = FALSE,
    force = 1
  ) +
  scale_color_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2"),
    labels = NULL
  ) +
  ylim(0, 3200) +
  xlab("Type") + ylab("NMRF") + ggtitle("Total methodology NMRF proxies by type") +
  theme(
    panel.background = element_rect(
      fill = "#BFD5E3",
      colour = "#6D9EC1",
      size = 2,
      linetype = "solid"
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

pla_plot_type

# Arrange plot to combine term and type graphs
plot_f_2 <- ggarrange(
  pla_plot_term,
  pla_plot_type ,
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 1
)

plot_f_2


jpeg(
  "plot_f_2.jpeg",
  width = 7,
  height = 3,
  units = 'in',
  res = 300
)
plot_f_2
dev.off()

# Poxies of NMRF categorized by zone and currency
tot_cur <- PLAtest_t %>% group_by() %>% mutate(countT = n()) %>%
  group_by(zone.min, currency, countT, add = TRUE) %>%
  summarise(count = n()) %>%
  mutate(per = paste0(round(100 * count / countT, 1), '%'),
         label = paste0(count, ", (", per, ")"))


pla_plot_cur <- tot_cur %>%
  ggplot(aes(x = currency, y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(
    zone.min, levels = c("GREEN", "AMBER", "RED")
  ))) +
  scale_fill_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2"),
    labels = c("Green", "Amber", "Red")
  ) +
  geom_label_repel(
    aes(label = label, color = factor(
      zone.min, levels = c("GREEN", "AMBER", "RED")
    )),
    hjust = 0.5,
    position = position_dodge(width = 0),
    vjust = -0.25,
    show.legend = FALSE,
    force = 1
  ) +
  scale_color_manual(
    values = c("darkolivegreen3", "coral2", "darkgoldenrod3"),
    labels = NULL
  ) +
  xlab("Currency") + ylab("NMRF") + ggtitle("Total methodology NMRF proxies by currency") +
  theme(
    panel.background = element_rect(
      fill = "#BFD5E3",
      colour = "#6D9EC1",
      size = 2,
      linetype = "solid"
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

pla_plot_cur

# Poxies of NMRF categorized by zone and currency and type
tot_cur_type <- PLAtest_t %>% group_by() %>% mutate(countT = n()) %>%
  group_by(zone.min, currency, type, countT, add = TRUE) %>%
  summarise(count = n()) %>%
  mutate(per = paste0(round(100 * count / countT, 1), '%'),
         label = paste0(count, ", (", per, ")"))


# Pla test characterization
PLAtest_char <- PLAtest
PLAtest_char <- left_join(PLAtest_char, non_sample_cur)

# 5. Characterization of proxies of NMRF by model ---------------------------------------------------
# Composition of NMRF by zone and model
pla_total <-
  PLAtest_char %>% group_by(model) %>% mutate(countT = n()) %>%
  group_by(Zone, countT, add = TRUE) %>%
  summarise(count = n()) %>%
  mutate(per = paste0(round(100 * count / countT, 1), '%'),
         label = paste0(count, ", (", per, ")"))

pla_total$Zone <-
  ifelse(pla_total$Zone == "1",
         "GREEN",
         ifelse(
           pla_total$Zone == "2",
           "AMBER",
           ifelse(pla_total$Zone == "3", "RED", "NA")
         ))

plot_f_3 <- pla_total %>%
  ggplot(aes(x = model, y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(
    Zone, levels = c("GREEN", "AMBER", "RED", "NA")
  ))) +
  scale_fill_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2", "steelblue3"),
    labels = c("Green", "Amber", "Red", "TNA")
  ) +
  geom_label(
    aes(label = label, color = factor(
      Zone, levels = c("GREEN", "AMBER", "RED", "NA")
    )),
    position = position_dodge(width = 0.9),
    vjust = -0.25,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2", "steelblue3"),
    labels = NULL
  ) +
  facet_wrap( ~ factor(Zone, levels = c("GREEN", "AMBER", "RED", "NA")),
              ncol = 1,
              strip.position = "left") +
  ylim(0, 4000) +
  xlab("Models") + ylab("Zones") + ggtitle("NMRF proxies by zones") +
  theme(
    panel.background = element_rect(
      fill = "#BFD5E3",
      colour = "#6D9EC1",
      size = 2,
      linetype = "solid"
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

plot_f_3

jpeg(
  "plot_f_3.jpeg",
  width = 10,
  height = 4,
  units = 'in',
  res = 300
)
plot_f_3
dev.off()


# Composition of NMRF by zone and model per term

pla_term <-
  PLAtest_char %>% group_by(model) %>% mutate(countT = n()) %>%
  group_by(Zone, term, countT, add = TRUE) %>%
  summarise(count = n()) %>%
  mutate(per = paste0(round(100 * count / countT, 1), '%'),
         label = paste0(count, ", (", per, ")"))

pla_term$Zone <-
  ifelse(pla_term$Zone == "1",
         "GREEN",
         ifelse(
           pla_term$Zone == "2",
           "AMBER",
           ifelse(pla_term$Zone == "3", "RED", "NA")
         ))

plot_f_4 <- pla_term %>%
  ggplot(aes(x = term, y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(
    Zone, levels = c("GREEN", "AMBER", "RED", "NA")
  )),
  position = position_dodge()) +
  scale_fill_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2", "steelblue3"),
    labels = c("Green", "Amber", "Red", "TNA")
  ) +
  geom_label(
    aes(label = label, color = factor(
      Zone, levels = c("GREEN", "AMBER", "RED", "NA")
    )),
    position = position_dodge(width = 0.9),
    vjust = -0.25,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2", "steelblue3"),
    labels = NULL
  ) +
  facet_wrap( ~ model,  ncol = 1, strip.position = "left") +
  ylim(0, 2010) +
  xlab("Term") + ylab("Models") + ggtitle("NMRF proxies by zones and term") +
  theme(
    panel.background = element_rect(
      fill = "white",
      colour = "#6D9EC1",
      size = 2,
      linetype = "solid"
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

plot_f_4

jpeg(
  "plot_f_4.jpeg",
  width = 7,
  height = 3,
  units = 'in',
  res = 300
)
plot_f_4
dev.off()

# Composition of NMRF by zone and model per type

pla_type <-
  PLAtest_char %>% group_by(model) %>% mutate(countT = n()) %>%
  group_by(Zone, type, countT, add = TRUE) %>%
  summarise(count = n()) %>%
  mutate(per = paste0(round(100 * count / countT, 1), '%'),
         label = paste0(count, ", (", per, ")"))

pla_type$Zone <-
  ifelse(pla_type$Zone == "1",
         "GREEN",
         ifelse(
           pla_type$Zone == "2",
           "AMBER",
           ifelse(pla_type$Zone == "3", "RED", "NA")
         ))

plot_f_5 <- pla_type %>%
  ggplot(aes(x = type, y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(
    Zone, levels = c("GREEN", "AMBER", "RED", "NA")
  )),
  position = position_dodge()) +
  scale_fill_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2", "steelblue3"),
    labels = c("Green", "Amber", "Red", "TNA")
  ) +
  geom_label(
    aes(label = label, color = factor(
      Zone, levels = c("GREEN", "AMBER", "RED", "NA")
    )),
    position = position_dodge(width = 0.9),
    vjust = -0.25,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2", "steelblue3"),
    labels = NULL
  ) +
  facet_wrap( ~ model,  ncol = 1, strip.position = "left") +
  ylim(0, 3500) +
  xlab("Type") + ylab("Model") + ggtitle("NMRF proxies by Type") +
  theme(
    panel.background = element_rect(
      fill = "white",
      colour = "#6D9EC1",
      size = 2,
      linetype = "solid"
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12)
  )

plot_f_5

jpeg(
  "plot_f_5.jpeg",
  width = 7,
  height = 3,
  units = 'in',
  res = 300
)
plot_f_5
dev.off()


# Composition of NMRF by zone and model per currency

pla_cur <-
  PLAtest_char %>% group_by(model, currency) %>% mutate(countT = n()) %>%
  group_by(Zone, countT, add = TRUE) %>%
  summarise(count = n()) %>%
  mutate(per = paste0(round(100 * count / countT, 0), '%'),
         label = paste0(count, ", (", per, ")"))

pla_cur$Zone <-
  ifelse(pla_cur$Zone == "1",
         "GREEN",
         ifelse(
           pla_cur$Zone == "2",
           "AMBER",
           ifelse(pla_cur$Zone == "3", "RED", "NA")
         ))


plot_f_6 <- pla_cur %>%
  mutate(label = if_else(Zone == "GREEN", as.character(label), NA_character_)) %>%
  ggplot(aes(x = currency, y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(
    Zone, levels = c("GREEN", "AMBER", "RED", "NA")
  ))) +
  scale_fill_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2", "steelblue3"),
    labels = c("Green", "Ambar", "Red", "NA")
  ) +
  geom_label(
    aes(label = label, color = factor(
      Zone, levels = c("GREEN", "AMBER", "RED", "NA")
    )),
    position = position_dodge(width = 0),
    vjust = -0.5,
    show.legend = FALSE
  ) +
  ylim(0, 1200) +
  scale_color_manual(
    values = c("darkolivegreen3", "darkgoldenrod3", "coral2", "steelblue3"),
    labels = NULL
  ) +
  facet_wrap( ~ model,  ncol = 1, strip.position = "left") +
  xlab("Currency") + ylab("Models") + ggtitle("NMRF proxies by currency") +
  theme(
    panel.background = element_rect(
      fill = "#BFD5E3",
      colour = "#6D9EC1",
      size = 2,
      linetype = "solid"
    ),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12),
  )

plot_f_6

jpeg(
  "plot_f_6.jpeg",
  width = 13,
  height = 10,
  units = 'in',
  res = 300
)
plot_f_6
dev.off()

# Remove innecesary data sets to retrieved just the needed for the next step
rm(list = ls()[!ls() %in% c('Methods', 'Min.vars.ls', 'vars.ls')])

save.image("./FinalModMap.RData")
