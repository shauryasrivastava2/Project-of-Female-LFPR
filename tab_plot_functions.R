# Tabulation and plotting functions ----
library(tidyverse)
library(kableExtra)

# Single categorical variable
tabOneVar <- function(dataframe, varname){
  var_no_string <- sym(varname)
  tab <- dataframe %>% 
    group_by(!!var_no_string) %>% 
    summarise(
      n = n(),
      Percent = round((n/nrow(dataframe))*100,1)
    )
  return(tab)
}


# Single Categorical variable
plotOneVar <- function(tableframe,xString){
  p <- ggplot(data= tableframe, aes(x=!!sym(names(tableframe)[[1]]), y=percent, label = percent)) + 
    geom_bar( width = 0.3, stat = "identity", fill = "steelblue") +
    labs(  y = "percent",
           x =  xString
           #subtitle = paste0("n = ",sum(tableframe[,2]))
           ) +
    geom_text(aes(label = percent),position=position_dodge(width=0.75), vjust=-0.25, size=2)+
    theme_bw() +
    scale_x_discrete(labels = wrap_format(10))+
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 9),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))+ 
    theme( legend.position = "bottom",
           strip.background = element_rect(colour="white"),
           panel.border = element_blank())+
    expand_limits(y = c(0, round(max(tableframe$percent)+0.04*max(tableframe$percent))))+
    NULL
  ggsave(paste0(figures,xString,".png"), p, 
         width = 5 , height = 3, device = "png")
  
  return(p)
  
}

plotOneVarOrder <- function(tableframe,xString){
  p <- ggplot(data= tableframe, aes(x=reorder(!!sym(names(tableframe)[[1]]),-percent), y=percent)) + 
    geom_bar( width = 0.3, stat = "identity", fill = "steelblue") +
    labs(  y = "percent",
           x =  xString
    ) +
    geom_text(aes(label = percent),position=position_dodge(width=0.75), vjust=-0.25, size=2)+
    theme_bw() +
    scale_x_discrete(labels = wrap_format(12))+
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 9),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))+ 
    theme( legend.position = "bottom",
           strip.background = element_rect(colour="white"),
           panel.border = element_blank()) +
    expand_limits(y = c(0, round(max(tableframe$percent)+0.04*max(tableframe$percent))))+
    NULL
  
  ggsave(paste0(figures,xString,".png"), p, 
         width = 5 , height = 3, device = "png")
  
 

  return(p)
  
}
# Two Categorical Variables
tabTwoVar <-  function(dataframe, varname, crossvar){
  tab <- dataframe %>%
    group_by (
      !!sym(crossvar)
    ) %>%
    mutate(
      countT= n()
    ) %>%
    group_by(
      !!sym(varname), add=TRUE
    ) %>%
    summarise(
      n = n(),
      percent = round(n/first(countT)*100,1)
    )
  return(tab)
}

# Plot two Categorical Variables

plotTwoVar <- function(tableframe,xString,fillString){
#   tableframe = tab_quantile
 #  axisString = "Quantile"
  # crossString = "Positions"
  p <- ggplot(tableframe, aes(x = !!sym(names(tableframe)[[2]]), y = Percent, fill = !!sym(names(tableframe)[[1]]))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.75) +
    scale_fill_manual(values = (cbp1))+
    labs(  y = "Percent",
           fill = fillString
          # subtitle = paste0("n = ",sum(tableframe[,3]))
          ) +
    geom_text(aes(label = Percent),position=position_dodge(width=0.75), vjust=-0.25, size=2)+
    theme_bw() +
    scale_x_discrete(labels = wrap_format(12))+
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 9),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))+ 
    theme( legend.position = "bottom",
           strip.background = element_rect(colour="white"),
           panel.border = element_blank())+
    expand_limits(y = c(0, round(max(tableframe$Percent)+0.04*max(tableframe$Percent))))
  
  ggsave(paste0(figures,fillString,"_",xString,".png"), p, 
         width = 5 , height = 3, device = "png")
  
  return(p)
}


# Bin Continuos variable
tabContBin <- function(dataframe,varname, bins){
  tab <- dataframe %>% 
    mutate(
      Bins   = cut_number(!!sym(varname), n = bins,na.rm=TRUE, dig.lab=6)
    ) %>% 
    group_by(Bins) %>% 
    summarise(
      n = n(),
      percent = round(n/nrow(dataframe)*100,1)
    )
  return(tab)
}

# Tabulate Multi Columns
tabMultiCol <- function(dataframe, startCol, EndCol,mainString){
  tab <- dataframe %>% 
    summarise_at(.vars = names(.)[startCol:EndCol],
                 .funs = c(mean="mean"))
  names(tab) <- str_replace(names(tab),"_mean","")
  
  names(tab) = label(dataframe[,names(tab)])
  new_tab <- as.data.frame(t(tab))
  
  new_tab <- tibble::rownames_to_column(new_tab, "Var") %>% 
    mutate(V1 = round(V1*100,1))
  names(new_tab) <- c(mainString,"percent")
  return(new_tab)
}

plotOneVarShort <- function(tableframe,xString){
  p <- ggplot(data= tableframe, aes(x=reorder(!!sym(names(tableframe)[[1]]),-percent), y=percent)) + 
    geom_bar( width = 0.3, stat = "identity", fill = "steelblue") +
    labs(  y = "percent",
           x =  xString
    ) +
    geom_text(aes(label = percent),position=position_dodge(width=0.75), vjust=-0.25, size=2)+
    theme_bw() + theme(axis.text.x = element_text(size = 6, angle = 40, hjust=1)) +
    theme(
          axis.title = element_text(size = 8),
          plot.subtitle = element_text(size = 7),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))+ 
    expand_limits(y = c(0, round(max(tableframe$percent)+0.04*max(tableframe$percent))))+ NULL
  
  
  ggsave(paste0(figures,xString,".png"), p, 
         width = 5 , height = 3, device = "png")
  

  return(p)
  
}


tabContBinTwoVar <- function(dataframe,varname,crossvar, bins){
  tab <- dataframe %>% 
    mutate(
      Bins   = cut_number(!!sym(varname), n = bins,na.rm=TRUE, dig.lab=6)
    ) %>% 
    group_by (
      !!sym(crossvar)
    ) %>%
    mutate(
      countT= n()
    ) %>%
    group_by(
      Bins, add=TRUE
    ) %>%
    summarise(
      n = n(),
      percent = round(n/first(countT)*100,1)
    )
  return(tab)
}

tabContBinThreeVar <- function(dataframe,varname,crossvar1,crossvar2, bins){
  tab <- dataframe %>% 
    mutate(
      Bins   = cut_number(!!sym(varname), n = bins,na.rm=TRUE, dig.lab=6)
    ) %>% 
    group_by (
      !!sym(crossvar1), !!sym(crossvar2)
    ) %>%
    mutate(
      countT= n()
    ) %>%
    group_by(
      Bins, add=TRUE
    ) %>%
    summarise(
      n = n(),
      percent = round(n/first(countT)*100,1)
    )
  return(tab)
}

#table_kable <- function(tableframe, caption) {
#  
#  knitr::kable(tableframe, caption = caption, booktabs = TRUE) %>%
#    kableExtra::kable_styling(font_size = 10,latex_options = "hold_position", full_width = FALSE) %>% 
#    row_spec(0, bold = T) %>% # format last row
#    column_spec(1, italic = T)
#}

tab_cross <- function( dataframe, crossvar, varname){
  tab <- dataframe %>%
    group_by (
      !!sym(crossvar)
    ) %>%
    mutate(
      countT= sum(comp_wt)
    ) %>%
    group_by(
      !!sym(varname), add=TRUE
    ) %>%
    summarise(
      n = n(),
      Percent = round(sum(comp_wt)/first(countT)*100,1)
    )
}


tab_crossXtreme <- function( dataframe,sector, crossvar, varname){
  tab <- dataframe %>%
    group_by (
      !!sym(sector),!!sym(crossvar)
    ) %>%
    mutate(
      n_T =  n(),
      countT= sum(comp_wt)
    ) %>%
    group_by(
      !!sym(varname), add=TRUE
    ) %>%
    summarise(
      n = n(),
      d= first(n_T),
      Percent = round(sum(comp_wt)/first(countT)*100,1)
    )
}

#tab_xtreme <- df_female_wob %>% 
#  group_by(area_actual, social_category) %>% 
#  mutate(
#    n_T =  n(),
#    countT =  sum(comp_wt)
#  ) %>% 
#  group_by(
#    work_for_pay, add = TRUE
#  ) %>% 
#  summarise(
#    n_n = n(),
#    n_d = first(n_T),
#    n =  sum(comp_wt),
#    d = first(countT),
#    Percent =  round(n*100/d,1)
#  )
#

plotthreeVar <- function(tableframe,xString,fillString, grid_string){
  #   tableframe = tab_quantile
  #  axisString = "Quantile"
  # crossString = "Positions"
  p <- ggplot(tableframe, aes(x = !!sym(names(tableframe)[[3]]), y = Percent, fill = !!sym(names(tableframe)[[1]]))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.75) +
    scale_fill_manual(values = (cbp1))+
    labs(  y = "Percent",
           x =  xString,
           fill = fillString
           # subtitle = paste0("n = ",sum(tableframe[,3]))
    ) +
    geom_text(aes(label = Percent),position=position_dodge(width=0.75), vjust=-0.25, size=2)+
    theme_bw() +
    scale_x_discrete(labels = wrap_format(12))+
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 8),
          plot.subtitle = element_text(size = 7),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))+ 
    expand_limits(y = c(0, round(max(tableframe$Percent)+0.04*max(tableframe$Percent))))+ facet_wrap(as.formula(paste("~", grid_string)),nrow = 5, strip.position = "left") +
    theme( legend.position = "bottom",
           strip.background = element_rect(colour="white"),
           panel.border = element_blank())
  ggsave(paste0(figures,fillString,"_",xString,"_",grid_string,".png"), p, 
         width = 5 , height = 3, device = "png")
  
  return(p)
}

barrier_plot <- function( type) {
  ggplot(long_barriers %>% filter(`Barrier Type` == type), aes( x= Barrier, y = Percent, fill = Sector)) + geom_bar(stat =  "identity", position = "dodge", width = 0.75) +
    scale_fill_manual(values = (cbp1))+
    labs(  y = "Percent",
           fill = "Sector",
           x= "",
           title = paste0("Type of Barrier: ",type)
           # subtitle = paste0("n = ",sum(tableframe[,3]))
    ) +
    geom_text(aes(label = Percent),position=position_dodge(width=0.75), vjust=-0.25, size=2)+
    theme_bw() +
    scale_x_discrete(labels = wrap_format(15))+
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 9),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))+ 
    theme( legend.position = "bottom",
           strip.background = element_rect(colour="white"),
           panel.border = element_blank())+
    expand_limits(y = c(0, round(max(long_barriers$Percent)+0.04*max(long_barriers$Percent)))) 
  
}


enabler_plot <- function( type) {
  ggplot(long_enablers %>% filter(`Enabler Type` == type), aes( x= Enabler, y = Percent, fill = Sector)) + geom_bar(stat =  "identity", position = "dodge", width = 0.75) +
    scale_fill_manual(values = (cbp1))+
    labs(  y = "Percent",
           fill = "Sector",
           x= "",
           title = paste0("Type of Enabler: ",type)
           # subtitle = paste0("n = ",sum(tableframe[,3]))
    ) +
    geom_text(aes(label = Percent),position=position_dodge(width=0.75), vjust=-0.25, size=2)+
    theme_bw() +
    scale_x_discrete(labels = wrap_format(15))+
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 9),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))+ 
    theme( legend.position = "bottom",
           strip.background = element_rect(colour="white"),
           panel.border = element_blank())+
    expand_limits(y = c(0, round(max(long_enablers$Percent)+0.04*max(long_enablers$Percent)))) 
  
}
