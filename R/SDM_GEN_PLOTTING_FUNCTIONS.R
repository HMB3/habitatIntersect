
######################################  PLOTTING FUNCTIONS FOR SDM ANALYSIS ---- #########################################
#########################################################################################################################


## Below are the functions used to project SDM models across geographic areas.


#' @title Project current maxent models across geographic space
#' @description This function takes the maxent models created by the 'fit_maxent_targ_bg_back_sel' function,
#' and projects the model across geographic space - currently just for Australia.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param maxent_path        Character string - The file path containing the existing maxent models
#' @param output_path        Character string - The file path where the polygon data will be saved
#' @param current_grids      Character string - Vector of current enviro conditions that you want to include
#' @param save_novel_poly    Character string - Save the novel areas as polygons?
#' @param create_mess        Logical - Create mess maps of the predictions (T/F)?
#' @param mess_layers        Logical - Save mess maps of the each layer as .png (T/F)?
#' @param poly_path          Character string - file path to feature polygon layer.
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @details It uses the rmaxent package https://github.com/johnbaums/rmaxent
#' @export single_bar_order_factor
## single_barchart_order_y_small 
single_bar_order_factor = function(df, title, xvar, yvar, ylab, xlab, ymin, ymax, axis_multiplier, col_palette,
                                   tsize, caption, xsize, ysize, ycol, mar,
                                   lab_size, bar_width, capt_size) {
  
  ## Set the max and min inside function
  ymax = max(df$yvar) + max(df$yvar * axis_multiplier)
  
  plot <- ggplot(df, aes(x = Response, y = Percentage, fill = Response)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_colour_brewer(palette = col_palette, na.value = "grey") +
    
    geom_text(aes(label = Percentage, hjust = + 0.5), 
              hjust = -0.5, 
              position = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold"),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  return(plot)
  
}




#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################
