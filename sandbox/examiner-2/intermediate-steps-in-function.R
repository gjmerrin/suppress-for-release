# LEFT panel
g_labels_left <- l$labels_long %>% 
  dplyr::filter( ! agg_level == "PROV" ) %>% 
  ggplot2::ggplot(
    aes(
      x = ""
      ,y = label_hsda
    )
  )+
  geom_tile(fill = NA)+
  geom_text(aes(label = value))+
  facet_grid(.~agg_level)+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  theme(
    axis.text.y = element_blank()
    ,panel.grid = element_blank()
  )
g_labels_left


# RIGHT panel
# colors from: http://colorbrewer2.org/#type=qualitative&scheme=Set2&n=3
censor_colors <- c(
   "0-none"            = NA                                 
  ,"1-small-cell"      = "#fc8d62" # red                     
  ,"2-recalc-triplet"  = "#66c2a5" # green                      
  ,"3-single-sup"      = "#8da0cb" # blue 
  ,"3-single-sup-draco"= "grey90" # grey                     
)

g_values_right <- l$values_long %>% 
  dplyr::mutate(
    agg_level         = factor(agg_level,        levels = c("HSDA","HA", "PROV")) 
    ,censor_activated = factor(censor_activated, levels = names(censor_colors) )
  ) %>% 
  ggplot2::ggplot(
    aes( x = sex, y = label_hsda )
  ) + 
  geom_tile(aes(fill = censor_activated))+
  geom_text(aes(label = value))+
  facet_grid(. ~ agg_level)+
  scale_fill_manual(values = censor_colors)+
  theme_minimal()+
  theme(
    axis.title = element_blank()
    ,axis.text.y = element_blank()
    ,legend.position = "none"
    , panel.grid = element_blank()
  )
g_values_right

main_title <- paste0(l$disease, " - ", l$year)

# COMBINE panels
grid::grid.newpage()

layout <- grid::grid.layout(
  nrow = 2
  ,ncol = 2
  ,widths  = grid::unit(x = c(.5,   .5),units =  c("null","null") ) 
  ,heights = grid::unit(x = c(.05, .95),units =  c("null","null") ) 
)

grid::pushViewport(grid::viewport(layout = layout))

grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col=1), just = "left")
print(g_labels_left, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
print(g_values_right, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))

grid::popViewport(0)


















