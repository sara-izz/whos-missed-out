#Function to plot points with error bars of a variable comparing each category by region
#To plot on subset, create subset of main survey design then call function
# NB: 'variable' must be factor; use update(my_design, factor) before using this function

plot.survey.grpdregion <- function(my_design, variable, var_levels, var_labels, p_title = "title", svy_fun = svymean, region = "GVTREGN", p_type = "point"){
  
  ftable(svyby(formula = as.formula(paste("~", variable)), by = as.formula(paste("~", region)), 
               design = my_design, FUN = svy_fun, na.rm = TRUE))
  
  
  t_variable <- as.data.frame( ftable(svyby(formula = as.formula(paste("~", variable)), by = as.formula(paste("~", region)),
                                              design = my_design,
                                              FUN = svy_fun, na.rm = TRUE))
  )


  t_variable <- dcast(t_variable, paste(paste(region, collapse = "+" ), "+ Var3 ~ Var2" ))
  t_variable$Var3 <- factor( t_variable$Var3, levels = var_levels,
                               labels = var_labels)
  colnames(t_variable)[1] <- "region"


  #Plot as points with error bars
  if(p_type == "point"){
    p <- ggplot(na.omit(t_variable), aes(x = svy_fun, xmin = svy_fun-SE, xmax = svy_fun+SE, y =region,
                                         colour = region)) +
      geom_point() + geom_segment( aes(x = svy_fun-SE, xend = svy_fun+SE, y = region,
                                       yend=region)) +
      #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(colour = "grey85")) +
      xlab("Prop in category") + guides(colour=FALSE, shape = FALSE) + ylab("Region")+
      ggtitle(p_title) + facet_grid( Var3~ .)

    print(p)
  }

   if(p_type == "bar" & (region == "grpd_region2" | region == "grpd_region3")){
     #Plot as bars
     b <- ggplot(na.omit(t_variable), aes(x = Var3, y = svy_fun, fill = region)) +
       #geom_freqpoly( aes(group = factor(aff_shared_noincben)))+
       geom_bar(stat="identity", position = position_dodge()) +  
       scale_fill_manual(values=c("#A7A8AA","#FF0000")) +
       guides(fill=guide_legend(title=NULL)) +
       theme( legend.position = "top", panel.background = element_rect(fill = "white") ) +
       xlab(paste(variable)) + ylab("Proportion") 
     
     print(b)
   }
  
  if(p_type == "hbar" & (region == "grpd_region2" | region == "grpd_region3")){
    #Plot as bars
    hb <- ggplot(na.omit(t_variable), aes(x = Var3, y = svy_fun, fill = region)) +
      #geom_freqpoly( aes(group = factor(aff_shared_noincben)))+
      geom_bar(stat="identity", position = position_dodge()) + 
      coord_flip() +
      scale_fill_manual(values=c("#A7A8AA","#FF0000")) +
      guides(fill=guide_legend(title=NULL)) +
      theme(axis.title.y = element_blank()) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white") ) +
      ylab("Proportion")
    
    print(hb)
  }
  #return dataframe
  #View(t_variable, variable)
  return(t_variable)
  
}