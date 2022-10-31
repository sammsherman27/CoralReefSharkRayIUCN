# This script generates the Boosted Regression Tree figures for the Coral Reef Sharks project

library(tidyverse)
library(patchwork)

# publication theme
publication_theme <- function(axis_title_size = 13,
                              axis_text_size = 11) {
  
  theme(text = element_text(family = 'Helvetica'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = 'grey20', size = 12),
        axis.line = element_line(colour = 'grey20'),
        axis.text = element_text(colour = 'grey20', size = axis_text_size),
        axis.title = element_text(colour = 'grey20', size = axis_title_size),
        axis.ticks = element_line(colour = 'grey20'),
        strip.background = element_blank())
  
}

# BRT bias plots -----------------------------------
# Test set biases
test_clean_shark <- read_csv('../Data/BRT_testset_pred_shark.csv')
test_clean_ray <- read_csv('../Data/BRT_testset_pred_ray.csv')

test_clean <- 
  rbind(test_clean_shark, test_clean_ray) %>% 
  mutate(SharkRay = paste(SharkRay, 's', sep = ''))

head(test_clean)

bias_plot <- 
  ggplot(test_clean %>% 
           # take the mean per round
           group_by(N_round, SharkRay) %>% 
           summarise(bias = mean(threat_bias)),
         aes(x = bias, fill = SharkRay)) +
  geom_histogram(bins = 40, colour = NA,
                 alpha = 0.7, position = 'identity') +
  scale_fill_manual(values = c('#e68a00', '#2d5986')) +
  labs(y = 'Density', 
       x = 'Average bias (observed - predicted)',
       fill = '') +
  theme(legend.position = c(0.2, 0.99),
        legend.direction = 'horizontal') +
  publication_theme()

bias_plot


# RMSE ---
rmse_clean_shark <- read_csv('../Data/BRT_rmse_shark.csv')
rmse_clean_ray <- read_csv('../Data/BRT_rmse_ray.csv')

rmse_clean <- 
  rbind(rmse_clean_shark, rmse_clean_ray) %>% 
  mutate(SharkRay = paste(SharkRay, 's', sep = ''))

head(rmse_clean)

rmse_plot <- 
  ggplot(rmse_clean, aes(x = rmse, fill = SharkRay)) +
  geom_histogram(bins = 40, colour = NA,
                 alpha = 0.7, position = 'identity') +
  scale_fill_manual(values = c('#e68a00', '#2d5986')) +
  labs(x = 'Root mean squared error',
       y = '') +
  theme(legend.position = 'none') +
  publication_theme()

rmse_plot  

# patch them together
bias_plots <- 
  bias_plot + rmse_plot + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')

bias_plots

ggsave('../Figures/BRT_biases.png', bias_plots,
       width = 12, height = 6, dpi = 400)

# Correlation heatmap ---------------------------------------------------
all_data <- 
  read_csv('../Data/Full_data.csv') %>% 
  mutate_at(vars('coastal_pop', 'gdp', 'protein', 'ShelfArea_km2',
                 'Chla', 'catch', 'effort', 'cpue', 'spue'), log1p)

cor_data <- 
  all_data %>% 
  # rename all the column names to clean it up for plotting
  dplyr::rename('Coastal population' = 'coastal_pop',
                'GDP' = 'gdp',
                'HDI' = 'HDI_2019',
                'Marine protein\nconsumption' = 'protein',
                'WGI' = 'wgi',
                'Shelf area' = 'ShelfArea_km2',
                '1º Productivity' = 'Chla',
                'SST' = 'sst_med',
                'Catch' = 'catch',
                'Fishing effort' = 'effort',
                'CPUE' = 'cpue',
                'SPUE' = 'spue')

names(cor_data)

# It's annoying using the default cor() function because of the mismatch NAs
# Create a dataframe to go through and loop every combination of variables
cormat <- tibble(var1 = rep(colnames(cor_data[4:15]), each = 12),
                 var2 = rep(colnames(cor_data[4:15]), 12),
                 cor_val = NA)

for(i in 1:nrow(cormat)) {
  
  # specify the column names that we want
  col1 <- cormat$var1[i]
  col2 <- cormat$var2[i]
  
  # make a dataframe that we want to loop through
  # because of the repeating values, we'll have to make two seperate dataframes
  # one for each variable
  df1 <- 
    cor_data %>% 
    dplyr::select(all_of(col1)) 
  
  # second dataframe
  df2 <- 
    cor_data %>% 
    dplyr::select(all_of(col2))
  
  # now let's join them together
  df <- 
    bind_cols(df1, df2) %>% 
    # frop the NAs
    drop_na()
  
  # infill the empty dataframe
  cor(df)[2] -> cormat$cor_val[i]
  
}

cormat

# let's convert this to a matrix so we can remove the upper half
cormat_mat <- 
  cormat %>% 
  pivot_wider(everything(), 
              names_from = var2,
              values_from = cor_val) %>% 
  # create a rowname and remove the column var1
  column_to_rownames(var = 'var1') %>% 
  as.data.frame() %>% 
  as.matrix()

head(cormat_mat)

# only keep the lower half
cormat_mat[upper.tri(cormat_mat)] <- NA
cormat_mat

# now we're going to convert it to a dataframe again

cor_plot <- 
  melt(cormat_mat, na.rm = TRUE) %>% 
  mutate(value = round(value, 2)) %>% 
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile(colour = 'white') +
  geom_text(aes(Var1, Var2, label = value),
            colour = 'black', size = 4) +
  scale_x_discrete(limits = rev) +
  scale_fill_gradient2(low = 'purple', high = 'red', mid = 'white',
                       midpoint = 0, limit = c(-1, 1), space = 'Lab') +
  labs(x = '', y = '') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

cor_plot

ggsave('../Figures/Correlation_plot.png')

# Relative influence of variables ----------------------------------------
ri_clean_shark <- read_csv('../Data/BRT_RelImp_shark.csv')
ri_clean_ray <- read_csv('../Data/BRT_RelImp_ray.csv')

ri_clean <- 
  rbind(ri_clean_ray, ri_clean_shark) %>% 
  mutate(SharkRay = paste(SharkRay, 's', sep = ''))

head(ri_clean)

# Let's find out what the relative importance of everything is
# OK so let's loop through and add new rows for variables that have an RI = 0

# we'll add a progress bar
pb <- 
  txtProgressBar(min = 1, max = length(unique(ri_clean$N_round)), style = 3)

ri_dfs <- 
  lapply(unique(ri_clean$N_round), function(round_no) {
    
    # for progress bar
    i <- as.numeric(round_no)
    
    # we'll need to do this seperately for sharks and rays
    # SHARKS
    subset_shark <- 
      ri_clean %>% 
      dplyr::filter(N_round == round_no,
                    SharkRay == 'Sharks')
    
    # now we'll create a new dataframe with the missing values
    shark_df <- 
      tibble(Feature = setdiff(unique(ri_clean$Feature), unique(subset_shark$Feature)),
             Gain = 0,
             Cover = 0,
             Frequency = 0,
             N_round = round_no,
             SharkRay = 'Sharks')
    
    #RAYS
    subset_rays <- 
      ri_clean %>% 
      dplyr::filter(N_round == round_no,
                    SharkRay == 'Rays')
    
    rays_df <- 
      tibble(Feature = setdiff(unique(ri_clean$Feature), unique(subset_rays$Feature)),
             Gain = 0,
             Cover = 0,
             Frequency = 0,
             N_round = round_no,
             SharkRay = 'Rays')
    
    # now we'll bind together the final dataframe to extract
    final_df <- 
      rbind(subset_shark, shark_df, subset_rays, rays_df)
    
    # set the progress bar
    setTxtProgressBar(pb, i)
    
    return(final_df)
    
  })

close(pb)

# now we'll bind all of these together 
ri_total <- 
  do.call(rbind, ri_dfs) %>% 
  group_by(SharkRay, Feature) %>% 
  summarise(sum_ri = sum(Gain),
            mean_ri = sum(Gain)/1000,
            # manually calculate the mean to account for the fact that some variables
            # have a VI = 0 for some rounds
            #mean_ri = mean(Gain),
            min_ri = min(Gain),
            max_ri = max(Gain),
            sd = sd(Gain),
            n = n(),
            ci = 1.96 * (sd/sqrt(n)),
            lower_95 = mean_ri - ci,
            upper_95 = mean_ri + ci) %>% 
  ungroup() %>% 
  mutate(Feature = case_when(Feature == 'spueray' ~ 'spue',
                             Feature == 'spueshark' ~ 'spue',
                             TRUE ~ Feature),
         Feature = factor(Feature, 
                          levels = c('coastal_pop',
                                     'ShelfArea_km2',
                                     'gdp', 
                                     'protein',
                                     'Chla',
                                     'wgi', 
                                     'sst_med',
                                     'HDI_2019',
                                     'catch',
                                     'effort',
                                     'cpue',
                                     'spue'))) %>% 
  mutate(Feature = dplyr::recode(Feature,
                                 'coastal_pop' = 'Coastal\npopulation',
                                 'ShelfArea_km2' = 'Shelf\narea',
                                 'gdp' = 'GDP',
                                 'protein' = 'Protein\nconsumption',
                                 'Chla' = 'Primary\nproduction',
                                 'wgi' = 'WGI',
                                 'sst_med' = 'Sea surface\ntemperature',
                                 'HDI_2019' = 'HDI',
                                 'catch' = 'Catch',
                                 'effort' = 'Fishing\neffort',
                                 'cpue' = 'CPUE',
                                 'spue' = 'SPUE')) %>% 
  # let's pull out the columns we care about
  dplyr::select(SharkRay, Feature, mean_ri, min_ri, max_ri, lower_95, upper_95) %>% 
  arrange(SharkRay, -mean_ri)
  

head(ri_total)

#write.csv(ri_total, '../Data/RelativeImportanceMeans.csv', row.names = FALSE)

# pdp plots
# make a dataframe to loop through with clean label names
pdp_clean_shark <- read_csv('../Data/BRT_pdp_shark.csv')
pdp_clean_ray <- read_csv('../Data/BRT_pdp_ray.csv')

pdp_df <- 
  rbind(pdp_clean_shark, pdp_clean_ray) %>% 
  mutate(label = case_when(variable == 'coastal_pop' ~ 'Coastal population',
                           variable == 'ShelfArea_km2' ~ 'Shelf area',
                           variable == 'gdp' ~ 'GDP',
                           variable == 'protein' ~ 'Protein consumption',
                           variable == 'Chla' ~ '1º Production',
                           variable == 'wgi' ~ 'WGI',
                           variable == 'sst_med' ~ 'Sea surface temperature',
                           variable == 'HDI_2019' ~ 'HDI',
                           variable == 'catch' ~ 'Catch',
                           variable == 'effort' ~ 'Fishing effort',
                           variable == 'cpue' ~ 'CPUE',
                           variable == 'spue' ~ 'SPUE'))

pdp_df

# we'll create a smaller dataframe outside the loop because adding it inside
# makes the computation of the label so stupidly slow
label_df <- 
  pdp_df %>% 
  distinct(variable, .keep_all = TRUE) %>% 
  # add a column for letter designation
  mutate(letter_label = case_when(variable == 'sst_med' ~ 'C', 
                                  variable == 'cpue' ~ 'D', 
                                  variable == 'ShelfArea_km2' ~ 'E', 
                                  variable == 'Chla' ~ 'F', 
                                  variable == 'coastal_pop' ~ 'G', 
                                  variable == 'gdp' ~ 'H', 
                                  variable == 'protein' ~ 'I', 
                                  variable == 'wgi' ~ 'J', 
                                  variable == 'effort' ~ 'K', 
                                  variable == 'catch' ~ 'L', 
                                  variable == 'spue' ~ 'M', 
                                  variable == 'HDI_2019' ~ 'N'))

pdp_plots <- 
  lapply(unique(pdp_df$variable), function(var) {
    
    # specify the fill colour based on the variable category 
    if (var %in% c('sst_med', 'ShelfArea_km2', 'Chla')) {
      plot_colour <- '#888844'
    } else if (var %in% c('HDI_2019', 'wgi', 'gdp')) {
      plot_colour <- '#bf4040'
    } else {
      plot_colour <- '#006666'
    }
    
    # make a dataframe for the plot
    plot_df <- 
      pdp_df %>% 
      dplyr::filter(variable == var) 
    
    # specify the label name
    var_df <- 
      label_df %>% 
      dplyr::filter(variable == var)
    
    var_label <- var_df$label
    var_letter <- var_df$letter_label
    
    # we also want a dataframe for the rug
    rug_data <- 
      all_data %>% 
      # most values are repeated for sharks and rays except for SPUE
      # which we are not plotting, so we can just filter the shark values
      dplyr::filter(SharkRay == 'Shark') %>% 
      # we need to centre and scale all variables
      #mutate_if(is.numeric, scale) %>% 
      # now let's pivot everything longer so it's easier to filter out in the function
      pivot_longer(c(-ISO3, -SharkRay), names_to = 'variable_name', 
                   values_to = 'value') %>% 
      # now we'll filter out the variable we care bout
      dplyr::filter(variable_name == var)
    
    # make the plot
    basic_plot <- 
      ggplot(plot_df, aes(x = value)) +
      # the fit line
      geom_smooth(aes(y = yhat, colour = SharkRay),
                  se = FALSE,
                  method = 'gam',
                  size = 2) +
      # add the rug
      geom_rug(data = rug_data,
               sides = 'b', alpha = 0.5, colour = 'grey50')
    
    # make a dataframe for the plot to figure out where to put the label
    plot_df <- 
      ggplot_build(basic_plot)$data[[1]]
    
    # set an expansion factor based on the variable 
    if (var == 'wgi') {
      expansion_factor <- 0.18
    } else if (var %in% c('gdp', 'effort', 'HDI_2019', 
                          'catch', 'spue', 'Chla')) {
      expansion_factor <- 0.13
    } else if (var == 'coastal_pop') {
      expansion_factor <- 0.11
    } else {
      expansion_factor <- 0.08
    }
    
    # specify the y axis location to put the label based on our expansion factor
    max_y <- 
      max(plot_df$y) + (max(plot_df$y) - min(plot_df$y)) * expansion_factor
    
    # specify the x axis location to put the label based on a certain distance after
    # the letter specification
    # we'll need to set an expansion factor based on variable
    if (var %in% c('Chla', 'protein')) {
      x_factor <- 0.05
    } else if (var == 'spue') {
      x_factor <- 0.1
    } else {
      x_factor <- 0.07
    }
    
    text_x <- 
      min(plot_df$x) + (max(plot_df$x) - min(plot_df$x)) * x_factor
    
    # now let's clean up the plot
    the_plot <- 
      basic_plot +
      # expand the plot
      scale_y_continuous(expand = expansion(mult = expansion_factor)) +
      annotate('text', x = min(plot_df$x), y = max_y, hjust = 0, 
               label = var_letter, size = 5, colour = 'black', fontface = 2) +
      # add the variable label
      annotate('text', x = text_x, y = max_y, hjust = 0, vjust = 0.65,
               label = var_label, size = 4, colour = plot_colour) +
      labs(x = '',
           y = '') +
      scale_colour_manual(values = c('#e68a00', '#2d5986')) +
      publication_theme() +
      theme(legend.position = 'none',
            plot.margin = unit(c(0, 0, 0, 0), 'cm'),
            axis.line = element_blank(),
            panel.border = element_rect(colour = 'grey20', fill = NA,
                                        size = 0.8))
    
    # now we're going to specify whether it's the bottom plot or not
    if (var %in% c('ShelfArea_km2', 'HDI_2019')) {
      
      # first we'll specify the mean and the sd
      mean_val <- mean(plot_df$x)
      sd_val <- sd(plot_df$x)
      
      final_plot <- 
        the_plot +
        scale_x_continuous(breaks = c(mean_val - sd_val, mean_val, mean_val + sd_val),
                           labels = c(-1, 0, 1)) +
        theme(axis.text.x = element_text(size = 9))
      
    } else if (var == 'protein') {
      
      # first we'll specify the mean and the sd
      mean_val <- mean(plot_df$x)
      sd_val <- sd(plot_df$x)
      
      final_plot <- 
        the_plot +
        scale_x_continuous(breaks = c(mean_val - sd_val, mean_val, mean_val + sd_val),
                           labels = c(-1, 0, 1)) +
        theme(axis.text.x = element_text(size = 9)) +
        labs(x = 'Standardized value of explanatory variable')
      
    } else {
      final_plot <- 
        the_plot +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())
    }
    
    return(final_plot)
    
  })

pdp_plots[[1]]
pdp_plots[[11]]

all_pdp <- 
  plot_spacer() +
  # SST, CPUE, shelf area
  {pdp_plots[[7]] + plot_spacer() + pdp_plots[[11]] + plot_spacer() +
      pdp_plots[[6]] + plot_layout(ncol = 1, heights = c(1, -0.145, 0.8, -0.145, 0.8))} +
  plot_spacer() +
  # 1º productivity, coastal population, GDP, protein
  {pdp_plots[[8]] + plot_spacer() +
      pdp_plots[[1]] + plot_spacer() + pdp_plots[[2]] + plot_spacer() +
      pdp_plots[[4]] + plot_layout(ncol = 1, heights = c(1, -0.2, 1, -0.2,
                                                         0.8, -0.2, 0.8))} +
  plot_spacer() +
  # WGI, effort, catch, spue, HDI
  {pdp_plots[[5]] + plot_spacer() + pdp_plots[[10]] + plot_spacer() + 
      pdp_plots[[9]] + plot_spacer() + pdp_plots[[12]] + plot_spacer() +
      pdp_plots[[3]] + plot_layout(ncol = 1, heights = c(1, -0.28, 1, -0.28,
                                                         1, -0.28, 1, -0.28, 1))} +
  plot_layout(ncol = 6, widths = c(0, 1, 0, 1, 0, 1))

all_pdp

ggsave('../Figures/BRT_relimp.pdf', all_pdp,
       height = 8, width = 10)

# Density plots ---------------------------------------------------------
dens_df <- 
  ri_clean %>% 
  # create a sorting factor for the type of variable
  mutate(var_cat = case_when(Feature %in% c('sst_med', 
                                            'Chla', 
                                            'ShelfArea_km2') ~ 'Ecological',
                             Feature %in% c('wgi',
                                            'gdp',
                                            'HDI_2019') ~ 'Management',
                             TRUE ~ 'Fishing pressure'),
         label = case_when(Feature == 'coastal_pop' ~ 'Coastal population',
                           Feature == 'ShelfArea_km2' ~ 'Shelf area',
                           Feature == 'gdp' ~ 'GDP',
                           Feature == 'protein' ~ 'Protein consumption',
                           Feature == 'Chla' ~ '1º Productivity',
                           Feature == 'wgi' ~ 'WGI',
                           Feature == 'sst_med' ~ 'Temperature',
                           Feature == 'HDI_2019' ~ 'HDI',
                           Feature == 'catch' ~ 'Catch',
                           Feature == 'effort' ~ 'Fishing effort',
                           Feature == 'cpue' ~ 'CPUE',
                           Feature == 'spue' ~ 'SPUE')) 

head(dens_df)

# Because Nick wants the plots to overlap, we can do this all in patchwork
# we'll just create individual plots and then decrease the distance between them all

# density plots for rays
ray_dens_plots <- 
  lapply(unique(dens_df$Feature), function(variable_name) {
    
    # specify the fill colour based on the variable category 
    if (variable_name %in% c('sst_med', 'ShelfArea_km2', 'Chla')) {
      plot_colour <- '#888844'
    } else if (variable_name %in% c('HDI_2019', 'wgi', 'gdp')) {
      plot_colour <- '#bf4040'
    } else {
      plot_colour <- '#006666'
    }
    
    the_data <- 
      dens_df %>% 
      dplyr::filter(SharkRay == 'Rays' & Feature == variable_name)
    
    plot_label <- the_data$label
    
    basic_plot <- 
      ggplot(the_data, aes(x = Gain)) +
      geom_density(fill = plot_colour, colour = 'white', size = 0.8, bw = 0.03) +
      geom_segment(aes(x = -0.5, xend = Inf, y = 0, yend = 0), 
                   colour = plot_colour, size = 0.8) +
      scale_x_continuous(limits = c(-0.5, 0.65),
                         breaks = seq(0, 0.5, 0.25),
                         labels = c(0, 0.25, 0.5)) +
      publication_theme(axis_text_size = 9,
                        axis_title_size = 11)
    
    # we'll make a dataframe of the plot so we can put the axis label
    # in the same position on each plot - they all have different y axes
    plot_df <- 
      ggplot_build(basic_plot)$data[[1]]
    
    # finalize the plot
    # specify an axis on the last plot
    if (variable_name == 'coastal_pop') {
      the_plot <- 
        basic_plot +
        annotate('text', x = -0.5, y = 0.15 * (max(plot_df$y)), hjust = 0, 
                 label = plot_label, colour = plot_colour, size = 3.5,
                 family = 'Helvetica') +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_text(vjust = 2),
              axis.text.y = element_blank(),
              axis.title = element_blank())
        
    } else {
      the_plot <- 
        basic_plot +
        annotate('text', x = -0.5, y = 0.15 * (max(plot_df$y)), hjust = 0, 
                 label = plot_label, colour = plot_colour, size = 3.5,
                 family = 'Helvetica')  +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())
    }
    
    return(the_plot)
    
  })

rays_dens_all <- 
  ray_dens_plots[[2]] + plot_spacer() + ray_dens_plots[[4]] + plot_spacer() +
  ray_dens_plots[[9]] + plot_spacer() + ray_dens_plots[[6]] + plot_spacer() +
  ray_dens_plots[[10]] + plot_spacer() + ray_dens_plots[[12]] + plot_spacer() +
  ray_dens_plots[[1]] + plot_spacer() + ray_dens_plots[[7]] + plot_spacer() +
  ray_dens_plots[[5]] + plot_spacer() + ray_dens_plots[[11]] + plot_spacer() +
  ray_dens_plots[[8]] + plot_spacer() + ray_dens_plots[[3]] + 
  plot_layout(ncol = 1, heights = c(1, -0.7, 1, -0.7, 1, -0.7,
                                    1, -0.7, 1, -0.7, 1, -0.7,
                                    1, -0.7, 1, -0.7, 1, -0.7,
                                    1, -0.7, 1, -0.7, 1))


#rays_dens_all
ggsave('../Figures/Relimp_dens_rays.pdf', rays_dens_all,
       height = 8, width = 4)

# density plots for sharks
shark_dens_plots <- 
  lapply(unique(dens_df$Feature), function(variable_name) {
    
    # specify the fill colour based on the variable category 
    if (variable_name %in% c('sst_med', 'ShelfArea_km2', 'Chla')) {
      plot_colour <- '#888844'
    } else if (variable_name %in% c('HDI_2019', 'wgi', 'gdp')) {
      plot_colour <- '#bf4040'
    } else {
      plot_colour <- '#006666'
    }
    
    # separate the variable that we care about from the dataframe
    the_data <- 
      dens_df %>% 
      dplyr::filter(SharkRay == 'Sharks' & Feature == variable_name)
    
    # specify the cleaned plot label
    plot_label <- the_data$label
    
    # make a basic plot to build on
    basic_plot <- 
      ggplot(the_data, aes(x = Gain)) +
      # density plot with a white outline
      geom_density(fill = plot_colour, colour = 'white', size = 0.8, bw = 0.03) +
      # horizontal line at y = 0
      geom_segment(aes(x = -0.5, xend = Inf, y = 0, yend = 0), 
                   colour = plot_colour, size = 0.8) +
      # adjust the x axis
      scale_x_continuous(limits = c(-0.5, 0.5),
                         breaks = seq(0, 0.5, 0.25),
                         labels = c(0, 0.25, 0.5)) +
      publication_theme(axis_text_size = 9,
                        axis_title_size = 11)
    
    # we'll make a dataframe of the plot so we can put the axis label
    # in the same position on each plot - they all have different y axes
    plot_df <- 
      ggplot_build(basic_plot)$data[[1]]
    
    # finalize the plot
    # specify an axis on the last plot
    if (variable_name == 'effort') {
      the_plot <- 
        basic_plot +
        annotate('text', x = -0.5, y = 0.15 * (max(plot_df$y)), hjust = 0, 
                 label = plot_label, colour = plot_colour, size = 3.5,
                 family = 'Helvetica') +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_text(vjust = 2),
              axis.text.y = element_blank(),
              axis.title = element_blank())
      
    } else {
      the_plot <- 
        basic_plot +
        annotate('text', x = -0.5, y = 0.15 * (max(plot_df$y)), hjust = 0, 
                 label = plot_label, colour = plot_colour, size = 3.5,
                 family = 'Helvetica')  +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())
    }
    
    return(the_plot)
    
  })

shark_dens_all <- 
  shark_dens_plots[[2]] + plot_spacer() + shark_dens_plots[[9]] + plot_spacer() +
  shark_dens_plots[[4]] + plot_spacer() + shark_dens_plots[[12]] + plot_spacer() +
  shark_dens_plots[[10]] + plot_spacer() + shark_dens_plots[[6]] + plot_spacer() +
  shark_dens_plots[[3]] + plot_spacer() + shark_dens_plots[[5]] + plot_spacer() +
  shark_dens_plots[[7]] + plot_spacer() + shark_dens_plots[[1]] + plot_spacer() +
  shark_dens_plots[[8]] + plot_spacer() + shark_dens_plots[[11]] + 
  plot_layout(ncol = 1, heights = c(1, -0.7, 1, -0.7, 1, -0.7,
                                    1, -0.7, 1, -0.7, 1, -0.7,
                                    1, -0.7, 1, -0.7, 1, -0.7,
                                    1, -0.7, 1, -0.7, 1))


#rays_dens_all
ggsave('../Figures/Relimp_dens_sharks.pdf', shark_dens_all,
       height = 8, width = 4)
