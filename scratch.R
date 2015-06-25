

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


