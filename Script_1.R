# Set the directory of the folder

# Set seed to ensure results are reproducible
set.seed(1234)

# Required packages
req.packages <-
  c("readxl", "dplyr", "reshape2", "Hmisc", "stringr", "igraph")
new.packages <-
  req.packages[!(req.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
invisible(lapply(req.packages, library, character.only = TRUE))

# Load data
load('./Data/data.RData')

names(data) <-
  c(
    'date_id',
    'date',
    'curve_name',
    'currency',
    'tenor_id',
    'tenor',
    'value',
    'variation',
    'type_of_variation',
    'dhs_parameter'
  )
curves <- unique(data$curve_name)

#Create a risk factor dataset
data_fr <-
  data %>% group_by(curve_name, currency, tenor) %>% summarise()

#Prove that have unique currencies, if there is just one currency for each risk factor it will return "OK"
if (dim(data %>% group_by(curve_name, currency) %>% summarise())[1] == dim(data %>%
                                                                           group_by(curve_name) %>% summarise())[1]) {
  print('OK')
} else {
  print('ERROR')
}

# 1. Create data set to define modellable and non-modellable sets ------------
setwd("./Data")
# Load modellability map given by the entity
mod_map <-
  read.csv('data_mod_20190904.csv', sep = ';', header = TRUE)
#Check if there are registers that are duplicated
mod_map_dup <- mod_map[duplicated(mod_map), ]
mod_map <- distinct(mod_map)
rm('mod_map_dup')

# Join data with tenor names
# Load data frame that contains the name of the period and its equivalence in days.
tenor_trans = read.csv('tenor_days.csv', sep = ';', header = TRUE)
names(tenor_trans) <- c('PERIODS', 'tenor')

# Join with the modellability map
mod_map <- left_join(mod_map, tenor_trans)

# Check risk factors that do not have a tenor equivalence
mod_map_without <- unique(mod_map[is.na(mod_map$tenor), 1:2])
mod_map$impute_tenor <- NA
mod_map$PERIODS <- as.character(mod_map$PERIODS)
mod_map[is.na(mod_map$tenor), 7] <- mod_map[is.na(mod_map$tenor), 2]

#There are some tenors with D included that will not have a correspondance in terms of terminology, so it needs to be removed
mod_map$impute_tenor <-
  sapply(mod_map$impute_tenor, function(x)
    gsub("D", "", x))

#Impute tenor D to tenor
mod_map[is.na(mod_map$tenor), match('tenor', names(mod_map))] <-
  mod_map[is.na(mod_map$tenor), match('impute_tenor', names(mod_map))]

# Check risk factors that do not have a tenor equivalence
mod_map_without <- unique(mod_map[is.na(mod_map$tenor), 1:2])

#Check the ones that have an empty tenor, if every risk factor has one it will return "OK"
if (nrow(mod_map_without) == 0) {
  mod_map_without <- unique(mod_map[is.na(mod_map$tenor), 1:2])
  print('OK')
} else {
  print('ERROR')
}

# Check levels of dataset, how many are "modellable" and how many "non-modellable"
mod_map %>% group_by(Modellable, mod_code) %>% summarise()

# Change name of columns and type so all are integers
mod_map$tenor <- as.integer(mod_map$tenor)
names(mod_map) <-
  c(
    'curve_name',
    'period',
    'currency',
    'modellable',
    'mod_code',
    'tenor',
    'impute_tenor'
  )


# Check that there are not duplicates, if not will return "OK"
mod_map[duplicated(mod_map[, 1:3]), ]

if (nrow(mod_map_without) == 0) {
  print('OK')
} else {
  print('ERROR')
}

#Check currencies in the modellability map.
map_curves <-
  mod_map %>% group_by(curve_name, currency) %>% summarise()
map_currency <- unique(map_curves$currency)
length(unique(map_curves$curve_name))
dim(
  mod_map %>% group_by(curve_name, tenor, modellable, mod_code, impute_tenor) %>%
    summarise()
)

# 2. Join risk factors with mod_map----------------------------------------
mod_map <- select(mod_map,-currency)
data_fr <- left_join(data_fr, mod_map)

#Futures which tenor is less than 4 years are treated as modellable. The rest follows the modellability map
data_fr[grepl("FUT", data_fr$curve_name) &
          data_fr$tenor < 1463, match('modellable', names(data_fr))] <-
  'YES'
data_fr[grepl("FUT", data_fr$curve_name) &
          data_fr$tenor < 1463, match('mod_code', names(data_fr))] <-
  1

# Risk factors that were not classify because there was not an equivalent risk factor in the modellability map are deemed to be "non-modellable"
data_fr_without <- data_fr[is.na(data_fr$modellable), ]
data_fr[is.na(data_fr$modellable), match('modellable', names(data_fr))] <-
  'NO'
data_fr[data_fr$modellable == 'NO', match('mod_code', names(data_fr))] <-
  0

#Check curve names
curve_names_w <- unique(data_fr_without$curve_name)

#Check that they all are with a modellable criterion, if all then, it will return "OK"
data_fr_without <- data_fr[is.na(data_fr$modellable), ]

if (nrow(data_fr_without) == 0) {
  print('OK')
} else {
  print('ERROR')
}



#Curves complete with complete tenors
curves_mod_nomod <-
  data_fr %>% group_by(curve_name, modellable) %>% summarise(count = n())
curves_mod_nomod_t <-
  dcast(curves_mod_nomod, curve_name ~ modellable, value.var = "count")

#Check total cases
colSums(curves_mod_nomod_t[, 2:3])
colSums(curves_mod_nomod_t[, 2:3], na.rm = TRUE)

#Remove unnecesary columns
data_fr <-
  select(data_fr, 'curve_name', 'currency', 'mod_code', 'tenor')
# 3. Join risk factors data with type and term---------------------------------------------------------------
# Load data of types of curves, this file has been manually created
type <- read_excel("types_curves.xlsx", sheet = "type")

data_fr <- left_join(data_fr, type)

#Check that the data set contains the type for all the risk factors
data_fr_without <- data_fr[is.na(data_fr$type), ]

#Eliminate FX curves
data_fr <- data_fr[data_fr$type != 'FX', ]

#Add term curves
data_fr$term <- 'lt'
data_fr[data_fr$tenor <= 732, match('term', names(data_fr))] <- 'st'


#Create the identificator "fr"
data_fr <-
  mutate(data_fr,
         fr = paste0(curve_name, '.t', tenor, '.c', currency, '.tt', term))


#Separate data set of series into modellability and non-modellability
mod <- data_fr[data_fr$mod_code == 1, ]
nomod <- data_fr[data_fr$mod_code == 0, ]

rm('data_fr_without', 'mod_map_without')

# 4. Create day variations of rates ---------------------------------------
data_var <- data

#Remove unnecesary variables
data_var <-
  select(data_var,
         'date_id',
         'date',
         'curve_name',
         'currency',
         'value',
         'tenor')

#Add term curves
data_var$term <- 'lt'
data_var[data_var$tenor <= 732, match('term', names(data_var))] <-
  'st'

#Order data
data_var <-
  data_var[with(data_var, order(curve_name, tenor, date)), ]

#Create the identifcator "fr"
data_var <-
  mutate(data_var,
         fr = paste0(curve_name, '.t', tenor, '.c', currency, '.tt', term))

#Create variations
data_var <- data_var %>%
  group_by(fr) %>%
  mutate(var_value = value - lag(value, default = value[1]))

#Check the start date for all the curves
min(data_var$date)
data_min_date <- data_var[data_var$date == 39085, ]
dim(data_min_date %>% group_by(curve_name, tenor) %>% summarise())[1]

#Remove the first day
data_var <- data_var[data_var$date != 39085, ]

#Remove unnecesary variables
data_var <-
  select(
    data_var,
    'date_id',
    'date',
    'curve_name',
    'currency',
    'tenor',
    'fr',
    'var_value',
    'term'
  )

# 5. Create dataset of "modellable" risk factors with their respective series-------------------------------------------------------
mod <- data.frame(mod)

data_var <- data.frame(data_var)

data_mod <- data_var[data_var$fr %in% mod$fr, ]

data_mod1 <- dcast(data_mod, date ~ fr, value.var = "var_value")

mod <- data.frame(lapply(mod, function(x)
  gsub('\\s+', '..', x)))
names(data_mod1) <- str_replace_all(names(data_mod1), " ", "..")

mod <- data.frame(lapply(mod, function(x)
  gsub(':', '.', x)))
names(data_mod1) <- str_replace_all(names(data_mod1), ':', '.')

mod <- data.frame(lapply(mod, function(x)
  gsub('-', '_', x)))
names(data_mod1) <- str_replace_all(names(data_mod1), '-', '_')

data_canscale <- data_mod1[, 2:dim(data_mod1)[2]]
data_mods <- data_mod1

# 6. Review correlation among "modellable" risk factors -------------------------------------------------------------

#Function to flatten the correlation matrix
# taken from http://www.sthda.com/english/wiki/correlation-matrix-formatting-and-visualization

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p = pmat[ut]
  )
}

# Create correlation matrix
res2 <- rcorr(as.matrix(data_canscale))

# Flatten results
cor <- flattenCorrMatrix(res2$r, res2$P)

# Retrieved the "modellable" factors that have a 1 in correlation
cormod1 <- cor[cor$cor == 1 | cor$cor == -1, ]


# 7. Create dataset for "non-modellable" risk factors with their respective series ----------------------------------------------------
nomod <- data.frame(nomod)

data_nomod <- data_var[data_var$fr %in% nomod$fr, ]

data_nomod1 <- dcast(data_nomod, date ~ fr, value.var = "var_value")

nomod <- data.frame(lapply(nomod, function(x)
  gsub('\\s+', '..', x)))
names(data_nomod1) <- str_replace_all(names(data_nomod1), " ", "..")

nomod <- data.frame(lapply(nomod, function(x)
  gsub(':', '.', x)))
names(data_nomod1) <- str_replace_all(names(data_nomod1), ':', '.')

nomod <- data.frame(lapply(nomod, function(x)
  gsub('-', '_', x)))
names(data_nomod1) <- str_replace_all(names(data_nomod1), '-', '_')

data_canscale2 <- data_nomod1[, 2:dim(data_nomod1)[2]]
data_nomods <- data_nomod1

# 8. Correlation matrix for "non-modelable" risk factors-------------------------------------------------------------
res <- rcorr(as.matrix(data_canscale2))
cornomod <- flattenCorrMatrix(res$r, res$P)

# Retrieved the "non-modellable" factors that have a 1 in correlation
cornomod1 <- cornomod[cornomod$cor == 1 | cornomod$cor == -1, ]
cornomod1_sum <-
  cornomod1 %>% group_by(row) %>% summarise(count = n())


# 9. Create a network of correlation 1 for the "non-modellable" risk factors series---------------------------------------
net <- graph.data.frame(cornomod1)
net <-
  graph_from_data_frame(cornomod1, directed = FALSE, vertices = NULL)

nomod_ver <- as_data_frame(net, what = 'vertices')

# Get components for the network
V(net)
gsize(net)
gorder(net)
edge_attr(net)
is_connected(net)

components(net)

#Decompose components
components <- decompose(net, mode = 'weak', min.vertices = 2)
sapply(components, diameter)
sum(sapply(components, gorder))
sapply(components, gorder)
sapply(components, gsize)
nomod_comp <-
  sapply(components, function(x)
    as_data_frame(x, what = 'vertices'))

# Get a serie of the components and pick just one
nomod_ver_sample <- paste(sapply(nomod_comp, function(x)
  sample(x, 1)))
nomod_ver_del <-
  nomod_ver[!(nomod_ver$name %in% nomod_ver_sample), ]

# Create data frame for "non-modellable" risk factors without the perfectly correlated series.
data_nomods <-
  data_nomods[, which((names(data_nomods) %in% nomod_ver_del) == FALSE)]



# 10. Create a network of correlation 1 for the "modellable" risk factor series ---------------------------------------
net <- graph.data.frame(cormod1)
net <-
  graph_from_data_frame(cormod1, directed = FALSE, vertices = NULL)
#Graph display of the network.
plot.igraph(net, vertex.label = NA)
mod_ver <- as_data_frame(net, what = 'vertices')

# Display of the ocmponents of the network
V(net)
gsize(net)
gorder(net)
is_connected(net)

components(net)


#Decompose components
components <- decompose(net, mode = 'weak', min.vertices = 2)
sapply(components, diameter)
sum(sapply(components, gorder))
sapply(components, gorder)
sapply(components, gsize)
mod_comp <-
  sapply(components, function(x)
    as_data_frame(x, what = 'vertices'))

#Get a serie to see the components and pick just one
mod_ver_sample <- paste(sapply(mod_comp, function(x)
  sample(x, 1)))
mod_ver_del <- mod_ver[!(mod_ver$name %in% mod_ver_sample), ]

# Create data frame with teh "modellable" risk factors without the perfectly correlated series.
data_mods <-
  data_mods[, which((names(data_mods) %in% mod_ver_del) == FALSE)]

# 11. New modellability maps without correlated values -------------------------------------------------------------------------
#Modellability map: "modellable" risk factors
mod <- data.frame(mod)
mod_n <- mod
mod_n <- mod_n[!(mod_n$fr %in% mod_ver_del), ]
mod_n_test <- mod[(mod$fr %in% mod_ver_del), ]

#Modellability map: "non-modellable" risk factors
nomod <- data.frame(nomod)
nomod_n <- nomod
nomod_n <- nomod_n[!(nomod_n$fr %in% nomod_ver_del), ]
nomod_n_test <- nomod[(nomod$fr %in% nomod_ver_del), ]


# Recover lists for expanding methodology over perfectly correlate --------
for (i in 1:length(mod_comp)) {
  names(mod_comp)[i] <-
    mod_ver_sample[which(mod_ver_sample %in% mod_comp[[i]])]
  mod_comp[[i]] <-
    mod_comp[[i]][-which(mod_comp[[i]] == names(mod_comp)[i])]
}
for (i in 1:length(nomod_comp)) {
  names(nomod_comp)[i] <-
    nomod_ver_sample[which(nomod_ver_sample %in% nomod_comp[[i]])]
  nomod_comp[[i]] <-
    nomod_comp[[i]][-which(nomod_comp[[i]] == names(nomod_comp)[i])]
}

save(list = c("nomod_comp", "mod_comp"), file = "components.Rdata")


rm(list = ls()[!ls() %in% c('nomod_n', 'mod_n', 'data_mods', 'data_nomods')])

save.image("C:/Users/Rafaela Becerra/Desktop/UC3M/TFM/TFM/datamods1.RData")