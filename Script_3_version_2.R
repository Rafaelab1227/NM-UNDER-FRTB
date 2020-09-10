# Set the directory of the folder

# Set seed to ensure that the results are reproducible
set.seed(1234)

# Required packages
req.packages <-
  c(
    "reshape2",
    "dplyr",
    "ggplot2",
    "readxl",
    "ggpubr",
    "scales",
    "ggthemes",
    "treemapify"
  )
new.packages <-
  req.packages[!(req.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
invisible(lapply(req.packages, library, character.only = TRUE))

# Load data
#Uncomment lines based on the results that will be analysed
#load("./Results/Results2.RData")
load("./Results/Results3.RData")
Descriptionmodels <- read_excel("./Data/Descriptionmodels.xlsx")


# Set directory based on which results are used
# Uncooment the necessary line
# Conservative partition (80% training and 20% testing)
#setwd("./Performance/Performance2")
# Regulator partition (92.48% training and 7.58% for testing)
setwd("./Performance/Performance3")

# Index of the models that will be evaluated
index <- c(3, 4, 5, 6, 7, 16)
index_model <- paste0('results.model.', index)
model_comp <- data.frame(mget(index_model[1]))
names(model_comp)[1] <- "sample"
model_comp$sample <- as.character(model_comp$sample)

# Extract from enviorment the results for all models
for (i in 2:length(index)) {
  modelx <- data.frame(mget(index_model[i]))
  names(modelx)[1] <- "sample"
  modelx$sample <- as.character(modelx$sample)
  model_comp <- left_join(model_comp, modelx, by = 'sample')
}

# Create RMSE data frame with results
RMSE <- model_comp[, grepl("RMSE" , names(model_comp))]
model_comp$RMSE <- colnames(RMSE)[apply(RMSE, 1, which.min)]

# Create Rsquared data frame with results
Rsquared <- model_comp[, grepl("Rsquared" , names(model_comp))]
model_comp$Rsquared <-
  colnames(Rsquared)[apply(Rsquared, 1, which.max)]

# Create MAE data frame with results
MAE <- model_comp[, grepl("MAE" , names(model_comp))]
model_comp$MAE <- colnames(MAE)[apply(MAE, 1, which.min)]


# 1. First part performance based on the count of NMRF--------
# Count based on the number of NMRF that achieved their maximum performance under each methodology
# Determine number of NMRF, based on the maximum performance, for each model
results100.RMSE <-
  model_comp %>% group_by(RMSE) %>% summarise(RMSE.min = n())
results100.Rsquared <-
  model_comp %>% group_by(Rsquared) %>% summarise(Rsquared.max = n())
results100.MAE <-
  model_comp %>% group_by(MAE) %>% summarise(MAE.min = n())

# Change in column names
names(results100.RMSE)[1] <- 'Model'
names(results100.Rsquared)[1] <- 'Model'
names(results100.MAE)[1] <- 'Model'

results100.RMSE$Model <- sub('.RMSE', '', results100.RMSE$Model)
results100.Rsquared$Model <-
  sub('.Rsquared', '', results100.Rsquared$Model)
results100.MAE$Model <- sub('.MAE', '', results100.MAE$Model)

results100 <- full_join(results100.RMSE, results100.Rsquared)
results100 <- full_join(results100, results100.MAE)

results100$Total <- rowSums(results100[2:4], na.rm = TRUE)

# Data frame for maximum Rsquared, identifies the ones that are above 0.75
Rsquared$max <- do.call(pmax, c(Rsquared, list(na.rm = TRUE)))
Rsquared$max.name <- colnames(Rsquared)[apply(Rsquared, 1, which.max)]
Rsquared$max.th <- ifelse(Rsquared$max > 0.75, 1, 0)

count0.thr <-
  Rsquared %>% group_by(max.name) %>% summarise(max.th.count = sum(max.th))
total0.thr <- sum(count0.thr$max.th.count)

# Table for results MAE RMSE
table_RMSEMAE <- results100
table_RMSEMAE <-
  mutate(
    table_RMSEMAE,
    RMSE.percentage = RMSE.min / length(non_sample),
    MAE.percentage = MAE.min / length(non_sample)
  )


# Change names
results100.2 <- melt(results100[, 1:5], id.vars = 'Model')
results100.2$Model <- sub('results.', '', results100.2$Model)

results100.2 <-
  left_join(results100.2, select(Descriptionmodels, Model, Full))

results100.2[is.na(results100.2)] <- 0


# Plot of count for each model of the NMRF that achieved its maximum performance in that model
plot_1 <-
  ggplot(results100.2, aes(
    x = reorder(Full, value),
    y = value,
    color = variable
  )) +
  geom_point(size = 5) + geom_segment(aes(
    x = Full,
    y = 0,
    yend = value,
    xend = Full
  ), size = 2) +
  coord_flip() + facet_grid(. ~ variable, scales = "free") + coord_flip() +
  geom_label(aes(
    label = round(value),
    color = variable,
    hjust = 1,
    vjust = 0
  ), size = 4) +
  theme(
    panel.background = element_rect(
      fill = "#BFD5E3",
      colour = "#6D9EC1",
      size = 2,
      linetype = "solid"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(
      color = "#6D9EC1",
      fill = "navy",
      size = 1.5,
      linetype = "solid"
    ),
    strip.text.x = element_text(size = 12, color = "white"),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank()
  )

jpeg(
  "plot_1.jpeg",
  width = 13,
  height = 7,
  units = 'in',
  res = 300
)
plot_1
dev.off()

# 2.Second part average  measures ---------------------------------------------------

normalized <- function(y) {
  x <- y[!is.na(y)]
  x <- (x - max(x)) / (min(x) - max(x))
  y[!is.na(y)] <- x
  return(y)
}


# Need a separation based on currency because of the units of the NMRFs
fr_m <- data_nomods[, i, drop = FALSE]

non_sample_cur <- data.frame(non_sample)
non_sample_cur$fr <- NA

for (i in 1:length(non_sample)) {
  non_sample_cur[i, 2] <- colnames(data_nomods)[non_sample_cur[i, 1]]
}

non_sample_cur <- left_join(non_sample_cur, nomod_n)

# MSE ---------------------------------------------------------------------
# Data frame with minimum of the MAE per NMRF

# Total MAE per currency
MAE <- cbind(MAE, currency = non_sample_cur[, 4])
MAE_total <-
  MAE %>% group_by(currency) %>% summarise_all(funs(sum(., na.rm = TRUE)))

# NAs in MAE calculation
MAE_total_NAs <-
  MAE %>% group_by(currency) %>% summarise_all(funs(sum(is.na(.))))
MAE_total_NAs.2 <-
  data.frame(colSums(MAE_total_NAs[, 2:dim(MAE_total_NAs)[2]]))
MAE_total_NAs.2$Model <- rownames(MAE_total_NAs.2)
MAE_total_NAs.2$Model <-
  gsub("results.|.MAE", '', MAE_total_NAs.2$Model)

# Change names
MAE_total_NAs.2 <-
  left_join(MAE_total_NAs.2, select(Descriptionmodels, Model, Full))
names(MAE_total_NAs.2)[1] <- "Total.NAs"
MAE_total_NAs.2 <- select(MAE_total_NAs.2, Full, Total.NAs)
MAE_total_NAs.2 <-
  mutate(MAE_total_NAs.2, Nas = Total.NAs / length(non_sample))


# Average MAE per currency
MAE_average <-
  MAE %>% group_by(currency) %>% summarise_all(funs(mean(., na.rm = TRUE)))
MAE_average$min <-
  colnames(MAE_average)[apply(MAE_average, 1, which.min)]
MAE_average$min_1 <-
  apply(MAE_average, 1, function(x)
    names(sort(x, na.last = TRUE)[1]))
MAE_average$min_2 <-
  apply(MAE_average, 1, function(x)
    names(sort(x, na.last = TRUE)[2]))


# Normalize MAE
MAE_average_NORM <-
  MAE %>% group_by(currency) %>% summarise_all(funs(mean(., na.rm = TRUE)))
MAE_average_NORM <-
  t(apply(MAE_average_NORM[, 2:dim(MAE_average_NORM)[2]], 1, normalized))
rownames(MAE_average_NORM) <- as.character(MAE_average$currency)
MAE_average_NORM.2 <- melt(MAE_average_NORM, id.vars = 'Model')
MAE_average_NORM.2$Model <-
  gsub("results.|.MAE", '', MAE_average_NORM.2$Var2)

# Change names
MAE_average_NORM.2 <-
  left_join(MAE_average_NORM.2, select(Descriptionmodels, Model, Full))
names(MAE_average_NORM.2)[1] <- "variable"
MAE_average_NORM.2[is.na(MAE_average_NORM.2)] <- 0
MAE_average_NORM.2$Model <-
  gsub("model.", '', MAE_average_NORM.2$Model)
MAE_average_NORM.2$color.code <-
  ifelse(
    MAE_average_NORM.2$value > 0.9,
    "0",
    ifelse(
      MAE_average_NORM.2$value > 0.7,
      "1",
      ifelse(MAE_average_NORM.2$value < 0.3, "3", "2")
    )
  )

# Plot of the average MAE per currency
plot_2 <-
  ggplot(MAE_average_NORM.2, aes(x = reorder(Model, as.numeric(Model)), y =
                                   value)) +
  geom_bar(stat = "identity", aes(fill = color.code)) +
  scale_fill_manual(
    values = c(
      "darkolivegreen4",
      "darkolivegreen3",
      "darkgoldenrod3",
      "coral2"
    ),
    labels = c(">0.9", ">0.7 & <= 0.9", ">=0.3 & <= 0.7", "<0.3")
  ) +
  facet_wrap( ~ variable,  ncol = 1, strip.position = "left") +
  xlab("Models") + ylab("Currency") + ggtitle("Normalized average MAE") +
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
    axis.text.y = element_blank(),
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank()
  )

plot_2


MAE_total_NAs.2.2 <- select(MAE_total_NAs.2,-Total.NAs)
MAE_total_NAs.2.2 <-
  mutate(MAE_total_NAs.2.2,
         Complete.cases = 1 - Nas,
         Model = c(3:7, 16))
MAE_total_NAs.2.2 <-
  melt(select(MAE_total_NAs.2.2, -Full), id.vars = 'Model')

MAE_total_NAs.2.2[MAE_total_NAs.2.2 == 0] <- NA

# Plot of the NAs for MAE
plot_4 <- ggplot(MAE_total_NAs.2.2,
                 aes(x = factor(Model),
                     y = value,
                     fill = variable)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(label = scales::percent(value)),
            size = 4,
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#6D9EC1", "steelblue4")) +
  labs(x = "Model") +
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
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(size = rel(5))
  )

plot_4

jpeg(
  "plot_4.jpeg",
  width = 7,
  height = 2,
  units = 'in',
  res = 300
)
plot_4
dev.off()

# Total MAE of No NA cases
MAE_total_compl <-
  MAE %>% group_by(currency) %>% summarise_all(funs(sum(., na.rm = FALSE)))
MAE_total_compl$min <-
  colnames(MAE_total_compl)[apply(MAE_total_compl, 1, which.min)]
MAE_total_compl$min_2 <-
  apply(MAE_total_compl, 1, function(x)
    names(sort(x)[2]))
MAE_total_compl$min_3 <-
  apply(MAE_total_compl, 1, function(x)
    names(sort(x)[3]))

# MAE join with name of curve

MAE <- cbind(non_sample_cur, MAE)
MAE <- MAE[, -dim(MAE)[2]]


# RMSE --------------------------------------------------------------------
# Data frame with minimum of the RMSE per NMRF
RMSE <- cbind(RMSE, currency = non_sample_cur[, 4])
RMSE_total <-
  RMSE %>% group_by(currency) %>% summarise_all(funs(sum(., na.rm = TRUE)))
RMSE_total_compl <-
  RMSE %>% group_by(currency) %>% summarise_all(funs(sum(., na.rm = FALSE)))
RMSE_total_compl$min <-
  colnames(RMSE_total_compl)[apply(RMSE_total_compl, 1, which.min)]
RMSE_total_compl$min_2 <-
  apply(RMSE_total_compl, 1, function(x)
    names(sort(x)[2]))
RMSE_total_compl$min_3 <-
  apply(RMSE_total_compl, 1, function(x)
    names(sort(x)[3]))

# NAS in RMSE
RMSE_total_NAs <-
  RMSE %>% group_by(currency) %>% summarise_all(funs(sum(is.na(.))))

# Normalized RMSE
RMSE_average_NORM <-
  RMSE %>% group_by(currency) %>% summarise_all(funs(mean(., na.rm = TRUE)))
RMSE_average_NORM <-
  t(apply(RMSE_average_NORM[, 2:dim(RMSE_average_NORM)[2]], 1, normalized))
rownames(RMSE_average_NORM) <- as.character(MAE_average$currency)
RMSE_average_NORM.2 <- melt(RMSE_average_NORM, id.vars = 'Model')
RMSE_average_NORM.2$Model <-
  gsub("results.|.RMSE", '', RMSE_average_NORM.2$Var2)

# Change names
RMSE_average_NORM.2 <-
  left_join(RMSE_average_NORM.2, select(Descriptionmodels, Model, Full))
names(RMSE_average_NORM.2)[1] <- "variable"
RMSE_average_NORM.2[is.na(RMSE_average_NORM.2)] <- 0
RMSE_average_NORM.2$Model <-
  gsub("model.", '', RMSE_average_NORM.2$Model)
RMSE_average_NORM.2$color.code <-
  ifelse(
    RMSE_average_NORM.2$value > 0.9,
    "0",
    ifelse(
      RMSE_average_NORM.2$value > 0.7,
      "1",
      ifelse(RMSE_average_NORM.2$value < 0.3, "3", "2")
    )
  )

# RMSE join with name of curve

RMSE <- cbind(non_sample_cur, RMSE)
RMSE <- RMSE[, -dim(RMSE)[2]]

# Plot for the average RMSE per Model by currency
plot_3 <-
  ggplot(RMSE_average_NORM.2, aes(x = reorder(Model, as.numeric(Model)), y =
                                    value)) +
  geom_bar(stat = "identity", aes(fill = color.code)) +
  scale_fill_manual(
    values = c(
      "darkolivegreen4",
      "darkolivegreen3",
      "darkgoldenrod3",
      "coral2"
    ),
    labels = c(">0.9", ">0.7 & <= 0.9", ">=0.3 & <= 0.7", "<0.3")
  ) +
  facet_wrap( ~ variable,  ncol = 1, strip.position = "left") +
  xlab("Models") + ylab("Currency") + ggtitle("Normalized average RMSE") +
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
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    strip.text.y =  element_text(size = 10, color = "white"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank()
  )

plot_3


# Arrrange to create just one graph joinning the average normalized MAE and RMSE per model by currency
plot_23 <- ggarrange(
  plot_2,
  plot_3,
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 1
)

plot_23

jpeg(
  "plot_23.jpeg",
  width = 15,
  height = 8,
  units = 'in',
  res = 300
)
plot_23
dev.off()

# 3. Time comparison of the models---------------------------------------------------------

# Create data frame to join the computational time for each method
results.time.100 <- data.frame(row.names(index_model))

index_time <- paste0('time_elapsed.', index)
model_time <- mget(index_time[1])[[1]]
names(model_time)[1] <- "Time.elapsed"

for (i in 2:length(index)) {
  timex <- mget(index_time[i])[[1]]
  model_time <- c(model_time, timex)
}

model_time <- data.frame(model_time)
model_time$Model <- index_model
model_time$Model <- sub('results.', '', model_time$Model)

# Change name of the models
model_time <- left_join(model_time, Descriptionmodels)
results100$Model <- sub('results.', '', results100$Model)

# Rank model
results100$rank <-
  rank(-results100$RMSE.min,
       na.last = 'keep',
       ties.method = 'first')

model_time <- left_join(model_time, results100)
model_time <- mutate(model_time, `Time(hours)` = model_time / 3600)
model_time$typeh <-
  ifelse(model_time$`Time(hours)` > 12,
         "1",
         ifelse(model_time$`Time(hours)` < 7, "3", "2"))

# Plot of the computational time for each method measre in hours
plot_8 <-
  ggplot(model_time,
         aes(
           x = as.factor(Seq),
           y = `Time(hours)`,
           fill = as.factor(typeh)
         )) +
  geom_bar(stat = "identity") +
  ylim(0, 72) +
  geom_label(aes(label = round(`Time(hours)`, digits = 1)),
             vjust = -0.5,
             color = "white") +
  labs(x = "Model") +
  scale_fill_manual(values = c("1" = "coral2", "2" = "gray" , "3" = "chartreuse3"),
                    guide = FALSE) +
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
    axis.text.y = element_blank(),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
  )

plot_8

jpeg(
  "plot_8.jpeg",
  width = 4,
  height = 3,
  units = 'in',
  res = 300
)
plot_8
dev.off()


# Remove innecesary data sets to retrieved just the needed for the next step
rm(list = ls()[!ls() %in% c(
  'RMSE',
  'MAE',
  'model_comp',
  'RMSE_average_NORM',
  'MAE_average_NORM',
  'model_time',
  'data_mods'
)])

save.image("./Performance.RData")




