as=r


#Generamos el Índice "Ethnic" 100-%Pob. Índigena - %Pob. Afrodescendiente
GEOS$Ethnic= 100-GEOS$IND_141-GEOS$IND_128
summary(GEOS$Ethnic)

GEOS %>%
  ggplot( aes(x=Ethnic)) +
  geom_histogram(bins=10, fill='skyblue', color='#69b3a2')
