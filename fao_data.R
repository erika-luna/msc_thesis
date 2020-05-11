library(ggplot2)
library(ggpubr)

fao_2020 <- read.csv("FAOSTAT_data_4-16-2020.csv") 

fao_production <- fao_2020[which(fao_2020$Element == 'Production'), ]
fao_yield <- fao_2020[which(fao_2020$Element == 'Yield'), ]
fao_area <- fao_2020[which(fao_2020$Element == 'Area harvested'), ]

production <- fao_production$Item [order (fao_production$Value, decreasing = TRUE)]
yield <- fao_yield$Item [order (fao_yield$Value, decreasing = TRUE)]
area <- fao_area$Item [order (fao_area$Value, decreasing = TRUE)]


#production
p <- ggplot (data = subset (fao_production, Item %in% production [1 : 10]), 
        aes (Item, Value)) +
  geom_col (aes(reorder(Item[1:10], -Value), Value)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15))

#yield
y <- ggplot (data = subset (fao_yield, Item %in% yield [1 : 10]), 
        aes (Item, Value)) +
  geom_col (aes(reorder(Item[1:10], -Value), Value)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15))

#area
a <- ggplot (data = subset (fao_area, Item %in% area [1 : 10]), 
        aes (Item, Value)) +
  geom_col (aes(reorder(Item[1:10], -Value), Value)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15), legend.position = "none") 


grid.arrange(
  p, y, a, 
  nrow = 3,
  top = NULL,
  bottom = "Crop"
)



### From one data base ####
ggplot(fao_2020, aes(x = Item, y = Value)) + 
  geom_line(aes(color = Element)) + 
  facet_grid(Element ~ ., scales = "free_y") + 
  theme(legend.position = "none")

### Top 10 ###

tiff("test.tiff", units="in", width=16, height=10, res=600)
fao_2020 %>%
  group_by(Element) %>%
  top_n(n = 10, wt = Value) %>%
  ggplot(aes(x=Item, y=Value)) + 
  geom_col() + 
  scale_x_discrete(name="Crops") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(Element ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15)) 
dev.off()



#Other script

exports <- data.frame(Mexico_Exports_GrupoAlancelarioFrutasLegumbres)
colnames(exports) <- c("crop", "USD", "na", "season")

# based on variable values
exports_agr <- exports[which(exports$season=='perennial' |
                               exports$season=='annual'), ]

exp <- ggplot(exports_agr, aes(reorder(crop, -USD), USD, fill=season)) +
  geom_col() +
  scale_x_discrete(name="Crops") +
  scale_y_continuous(name="million USD") +
  #labs(title = "Main exported crops in Mexico 2018") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15))

exp + geom_text(aes(x = crop, 
                    y = USD, label = round(USD, 2), vjust=-0.25) )

fao_data <- data.frame(FAOSTAT_production_mx_2018)

production <- ggplot(fao_data, aes(reorder(Item[1:10], -Value), Value)) +
  geom_col() +
  scale_x_discrete(name="Crops") +
  scale_y_continuous(name="million USD") +
  #labs(title = "Main exported crops in Mexico 2018") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15))

crops <- fao_data$Item [order (fao_data$Value, decreasing = TRUE)]
ggplot (data = subset (fao_data, Item %in% crops [1 : 10]), 
        aes (Item, Value)) +
  geom_col(aes(reorder(Item[1:10], -Value), Value))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15))



