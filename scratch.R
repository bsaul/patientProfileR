#### Import Data ####
rt <- '150623'
lib <- 'J:\\PROTECT\\Operations\\Data checks\\Query Requests\\QU44 Subject Medication Profile Graphics\\'
outfile <- paste0("QU44_", rt, ".pdf")


dt <- read.csv(paste0(lib, 'sample_', rt, '.csv'), header = T, 
               stringsAsFactors = F, na.strings = ".")

dt <- read.csv(paste0(lib, 'med_history_', rt, '.csv'), header = T, 
               stringsAsFactors = F, na.strings = ".")
names(dt) <- tolower(names(dt))


save(dt, file = 'data/patientsamples.rdata')

#### TESTING GROUNDS ####

myMap <- aes_map(x = 'xmin', 
                 xend = 'xmax',
                 vline1 = 'xwdraw')

myBaseplot  <- make_baseplot(dt,
                         map = myMap,
                         axis.lim.x    = c(-7,(78*7)+15),
                         axis.breaks.x = c(0, 4*7, 12*7, 29*7, 52*7, 78*7),
                         axis.labels.x = c(0, 4, 12, 29, 52, 78),
                         axis.breaks.y = c(.2, .5, .75, .85),
                         axis.labels.y = c('R', 'P', 'ORCS', 'IVCS')) +
                theme(axis.ticks.y = element_blank())

## Create all plots by subject ##
allPlots <- dlply(dt, 'subjectid', function(x){
  plot_patient(myBaseplot, 
               x, 
               title = x[1, 'subjectid'],
               aes_map = myMap)
})

#### OUTPUT ####
save_single_plot <- function(SUBJECTID){
  ggsave(filename = paste0(lib, 'QU44_', SUBJECTID, '_', rt, '.pdf'),
         plot = allplots[[SUBJECTID]], 
         width = 3.5, height = 2, units = "in")
}

sapply(unique(dt$subjectid), save_single_plot,USE.NAMES = FALSE)


ml = do.call(marrangeGrob, c(allplots, list(nrow=4, ncol=2)))
ggsave(paste0(lib, outfile), ml)


