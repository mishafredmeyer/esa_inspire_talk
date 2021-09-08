library(ggplot2)
library(Cairo)
library(ggrepel)
library(ggpubr)
library(stringr)
library(gridExtra)

Cairo()
# Step 1: PPCP pie chart --------------------------------------------------

data <- data.frame(
  group = c("Antibiotic", "Fragrance", "Hormone",
            "Non-prescription drug", "Prescription drug"),
  value = c(39/126.2, 0.2/126.2, 18/126.2, 51/126.2, 18/126.2),
  label = c("39%", "0.2%", "18%", "51%", "18%")
)

bp1<- ggplot(data, aes(x="", y=value, fill=str_wrap(group, 16)))+
  geom_bar(width = 1, stat = "identity", color = "grey50") +
  xlab("") +
  ylab("") +
  ggtitle("PPCPs (2,515 abstracts)") +
  scale_fill_viridis_d(option = "viridis") +
  coord_polar("y", start=0) +
  # guides(fill = guide_legend(keyheight = 0.5,
  #                            default.unit = "inch")) +
  theme(#legend.key.size = unit(2, "cm"),
    axis.text.x=element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 24),
    legend.position = "none",
    legend.title = element_blank(),
    #text = element_text(size = 12),
    legend.text = element_text(size = 15,
                               margin = margin(2, 2, 2, 2, unit = "pt"))
  )
bp1

ggsave("PPCP_pie_chart.png", bp1, device = "png",
       height = 12, width = 18, units = "in")


# Step 2: STT pie chart ---------------------------------------------------
value = c((12+9+3)/134.1, (5+.9)/134.1, 37/134.1, (2+0.3)/134.1,
          (4+2+3)/134.1, (3+2)/134.1, 8/134.1, 19/134.1, 28/134.1)
data <- data.frame(
  group = c("Act carbon Adv oxidation Chlorination", 
            "Nano- & Sand filtration", 
            "Act sludge", "Anaer digester Anammox", 
            "Coag Flocc Sediment.",
            "Constr wetland Septic", "Membrane bioreactor",
            "Solids", "WTP"),
  value = c((12+9+3)/134.1, (5+.9)/134.1, 37/134.1, (2+0.3)/134.1,
            (4+2+3)/134.1, (3+2)/134.1, 8/134.1, 19/134.1, 28/134.1),
  label = as.character(paste0(round(value*100, 0), "%"))
)

bp2<- ggplot(data, aes(x="", y=value, fill=str_wrap(group, 15)))+
  geom_bar(width = 1, stat = "identity", color = "grey50") +
  xlab("") +
  ylab("") +
  ggtitle("STTs (2,558 abstracts)") +
  scale_fill_viridis_d(option = "inferno") +
  coord_polar("y", start=0) +
  # guides(fill = guide_legend(keyheight = 0.5,
  #                            default.unit = "inch")) +
  theme(#legend.key.size = unit(2, "cm"),
    axis.text.x=element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 24),
    legend.position = "none",
    legend.title = element_blank(),
    #text = element_text(size = 12),
    legend.text = element_text(size = 15,
                               margin = margin(2, 2, 2, 2, unit = "pt"))
  )
bp2

ggsave("STT_pie_chart.png", bp2, device = "png",
       height = 12, width = 18, units = "in")


# Step 3: Ecosystem pie chart ---------------------------------------------

value = c(29/138.3, 2/138.3, 0.3/138.3, 13/138.3,
          11/138.3, 62/138.3, 20/138.3)

data <- data.frame(
  group = c("Aquifer Groundwater", "Estuary Ocean", "Forest",
            "Lake", "Pond Lagoon Wetland", "River Stream", "Soil"),
  value = c(29/138.3, 2/138.3, 0.3/138.3, 13/138.3,
            11/138.3, 62/138.3, 20/138.3),
  label = as.character(paste0(round(value*100, 0), "%"))
)

bp3<- ggplot(data, aes(x="", y=value, fill=str_wrap(group, 10)))+
  geom_bar(width = 1, stat = "identity", color = "grey50") +
  xlab("") +
  ylab("") +
  ggtitle("Ecosystem Types (1,968 abstracts)") +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar("y", start=0) +
  # guides(fill = guide_legend(keyheight = 0.5,
  #                            default.unit = "inch")) +
  theme(#legend.key.size = unit(2, "cm"),
    axis.text.x=element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 24),
    legend.position = "none",
    legend.title = element_blank(),
    #text = element_text(size = 12),
    legend.text = element_text(size = 15,
                               margin = margin(2, 2, 2, 2, unit = "pt"))
  )
bp3

ggsave("ecosystem_pie_chart.png", bp3, device = "png",
       height = 12, width = 18, units = "in")


# Step 4: combine plots ---------------------------------------------------

ggarrange(bp1, bp2, bp3, ncol = 3, nrow = 1) %>%
  ggexport(filename = "combined_pie_chart_graphical abstracts.png")

grid.arrange(bp1, bp2,bp3)

#ggsave("combined_pie_chart_graphical_abstracts.png", combined_plots, device = "png")

gbp1 <- ggplotGrob(bp1)
gbp2 <- ggplotGrob(bp2)
gbp3 <- ggplotGrob(bp3)

gbp1$widths <- gbp3$widths
gbp2$widths <- gbp3$widths

grid_output <- grid.arrange(gbp1, gbp2, gbp3, nrow = 1, ncol = 3)

Cairo()
png("combined_pie_chart_graphical_abstracts.png", width = 1200, height = 800, 
    units = "px", type = "cairo") # Open a new pdf file
grid.arrange(gbp1, gbp2, gbp3, nrow = 1, ncol = 3)
dev.off()
