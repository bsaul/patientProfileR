
example_patient <- read.csv('data/example_data.csv', stringsAsFactors = FALSE, na.strings = ".")
save(example_patient, file = 'data/example_patient.rdata')

myMap <- aes_map(color.values = c('#a6cee3', '#1f78b4', 'black', '#b2df8a',
                                  '#33a02c', 'black', 'black', 'black', 'black'),
                 shape.values = c(1, 63, 8, 124, 23, 124, 2),
                 size.values  = c(0, 8, 7, 7, 3),
                 alpha.values = c(.2, 1),
                 fill.values  = c('white', 'lightgrey', '#e34a33', 
                                  'white', '#fee8c8', '#fdbb84'),
                 linetype.values = c('solid', 'dotted', 'dashed'),
                 label = 'text')


myBaseplot <- make_baseplot(example_patient,  
                            aes_map = myMap,
                            axis.lim.x    = c(-7,(78*7)+15),
                            axis.breaks.x = c(0, 4*7, 12*7, 29*7, 52*7, 78*7),
                            axis.labels.x = c(0, 4, 12, 29, 52, 78),
                            axis.breaks.y = c(.15, .35, .55, .65, .75, .85, .95),
                            axis.labels.y = c('R', 'P', 'ORCS', 'IVCS', '0-', 'PUCAI', '85-')) + 
  geom_hline(yintercept = .75, color = 'darkgrey') + 
  theme(axis.ticks.y = element_blank())


test <- plot_patient(myBaseplot, example_patient, 
                     title = 'An Example Plot', 
                     aes_map = myMap)
test


ggsave('example.png', test, width = 6, height = 4, units = 'in')




plot_patient(myBaseplot, s5, aes_map = myMap) 

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


