library(magrittr)
we <-
  kobo_trips %>%
  dplyr::ungroup() %>%
  dplyr::select(-Selenium_mu) %>%
  dplyr::mutate(dplyr::across(Zinc_mu:Vitamin_A_mu, ~ .x * 1000)) %>%
  tidyr::pivot_longer(-c(landing_id:weight), names_to = "nutrient", values_to = "gr") %>%
  dplyr::mutate(nutrient = tolower(nutrient),
                nutrient = stringr::str_replace(nutrient, "_mu", ""),
                nutrient = ifelse(nutrient == "omega_3", "omega3", nutrient),
                nutrient = ifelse(nutrient == "vitamin_a", "vitaminA", nutrient)) %>%
  dplyr::left_join(RDI_tab, by = "nutrient") %>%
  dplyr::mutate(npeople = gr / conv_factor) %>%
  dplyr::select(-gr, -conv_factor) %>%
  dplyr::group_by(landing_id) %>%
  dplyr::mutate(tot_people = sum(npeople, na.rm = T),
                people_perc = npeople / tot_people * 100) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(npeople, tot_people)) %>%
  tidyr::pivot_wider(names_from = "nutrient", values_from = "people_perc") %>%
  dplyr::filter(landing_period > "2019-01-01")

we_summ <-
  we %>%
  dplyr::group_by(landing_period, habitat, gear_type, vessel_type) %>%
  dplyr::summarise(dplyr::across(is.numeric, ~ median(.x))) %>%
  dplyr::ungroup()


# Assuming your data is stored in a variable called df
# Remove non-numeric columns for PCA

nut_vars <- we_summ %>% dplyr::select(zinc:vitaminA)

# Perform PCA
res.pca <- FactoMineR::PCA(nut_vars, scale.unit = TRUE, ncp = 4, graph = FALSE)

# Visualize eigenvalues/variances
factoextra::fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Biplot of PCA results
factoextra::fviz_pca_biplot(res.pca)


factoextra::fviz_pca_ind(res.pca,
             geom.ind = "point", # to keep the individuals as points
             col.ind = we_summ$gear_type, # color by gear_type
             palette = "jco", # color palette
             addEllipses = FALSE, # add concentration ellipses
             legend.title = "Gear Type",
             repel = FALSE)

df_pca <- dplyr::tibble(we_summ,
                        a1 = res.pca$ind$coord[ ,1],
                        a2 = res.pca$ind$coord[ ,2])
library(ggplot2)
df_pca %>%
ggplot(aes(a1, a2, color = habitat))+
  theme_minimal()+
  geom_point()

factoextra::fviz_nbclust(na.omit(we_summ)[ ,6:11], kmeans, method = "gap_stat")
k2 <- kmeans(na.omit(we_summ)[ ,6:11], centers = 5, nstart = 25)
factoextra::fviz_cluster(k2, data = na.omit(we_summ)[ ,6:11], geom = c("point"))

clusterdf <-
  dplyr::tibble(clusters = as.character(k2$cluster),
                           na.omit(we_summ))

clusterdf %>%
  dplyr::select(-weight) %>%
  tidyr::pivot_longer(-c(clusters:vessel_type)) %>%
  ggplot(aes(clusters, value, fill = name))+
  theme_minimal()+
  facet_wrap(.~name, ncol = 3, scales = "free")+
  geom_boxplot(size = 0.2, alpha = 0.75)


clusterdf %>%
  dplyr::select(-weight) %>%
  tidyr::pivot_longer(-c(clusters:vessel_type)) %>%
  dplyr::group_by(clusters, name) %>%
  dplyr::summarise(value = mean(value)) %>%
  ggplot(aes(clusters, value, fill = name))+
  theme_minimal()+
  geom_col()+
  scale_fill_viridis_d()+
  coord_flip()



#### discriminant factor

iris <-
  na.omit(we_summ) %>%
  #dplyr::mutate(gear_type = paste(habitat, gear_type, sep = "_")) %>%
  dplyr::select(habitat, zinc:vitaminA)

set.seed(123)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- iris[ind==1,]
testing <- iris[ind==2,]

linear <- MASS::lda(habitat~., training)
linear
p <- predict(linear, training)
df2 <- dplyr::tibble(p = p$x[,1], g = training$habitat)


df2 %>%
ggplot(aes(p, fill = g))+
  geom_histogram()+
  facet_grid(g~.)

p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$habitat)
tab

sum(diag(tab))/sum(tab)


as.data.frame(tab) %>%
  dplyr::mutate(Freq = ifelse(Freq == 0, NA_real_, Freq)) %>%
  ggplot(aes(Predicted, Actual))+
  geom_tile(aes(fill = Freq))+
  scale_fill_continuous(na.value = 'white', type = "viridis")+
  #scale_fill_viridis_c(na.value = 'white', alpha = 0.75)+
  coord_cartesian(expand = FALSE)+
  theme(panel.background = element_blank())
?scale_fill_continuous



##### methods: Nutrient content calculation #####

#1) Species Table. We got interested species information starting from
#fao interagency_code using fishbase API services. These species are filtered by specific regions code
# we used timor and indopacific
rfish_tab <- get_rfish_table(pars)

fao_groups <- get_fao_composition()

#2) Nutrients Table. Nutrients composition is retrieved by the final table from hicks and al.
nutrients_tab <-
  readr::read_csv(pars$nutrients$resource,
                  show_col_types = FALSE) %>%
  dplyr::rename(SpecCode = .data$spec_code) %>%
  dplyr::mutate(SpecCode = as.integer(.data$SpecCode)) %>%
  dplyr::select(.data$species, .data$SpecCode, tidyselect::contains("_mu")) %>%
#3) We merged Species Table and Nutrients Table by spec_code (FAO?)
  dplyr::right_join(rfish_tab, by = "SpecCode") %>%
  dplyr::select(.data$interagency_code, tidyselect::contains("_mu")) %>%
  dplyr::filter(!interagency_code %in% unique(fao_groups$interagency_code)) %>%
  dplyr::bind_rows(fao_groups)

#4) Some groups were not present in Nutrients Table and their nutritional values were
# obtained from alterantive sources (report di gianna)

if (isTRUE(convert)) {
  nutrients_tab <-
    nutrients_tab %>%
    dplyr::mutate(dplyr::across(
      c(.data$Zinc_mu, .data$Calcium_mu, .data$Iron_mu),
      ~ (.x / 1000) / 100
    )) %>%
    dplyr::mutate(dplyr::across(
      c(.data$Selenium_mu, .data$Vitamin_A_mu),
      ~ (.x / 1000000) / 100
    )) %>%
    dplyr::mutate(dplyr::across(
      c(.data$Omega_3_mu, .data$Protein_mu),
      ~ (.x / 1) / 100
    ))
}

#5) We summarise values by interagency code

if (isTRUE(summarise)) {
  nutrients_tab <-
    nutrients_tab %>%
    dplyr::group_by(.data$interagency_code) %>%
    dplyr::summarise_all(stats::median, na.rm = TRUE)
}

nutrients_tab


###

render_docs()

we <-
  timor.nutrients::kobo_trips %>%
  dplyr::filter(weight > 0, n_fishers >0, trip_duration > 0) %>%
  dplyr::select(trip_duration, n_fishers, habitat, gear_type, weight, Selenium_mu:Vitamin_A_mu) %>%
  na.omit() %>%
  rename_nutrients_mu() %>%
  tidyr::pivot_longer(-c(trip_duration, n_fishers, habitat, gear_type, weight),
                      names_to = "nutrient", values_to = "concentration_g") %>%
  dplyr::mutate(concentration_g_stand = (concentration_g / n_fishers) / trip_duration) %>%
  dplyr::mutate(concentration_g = concentration_g / weight) %>%
  dplyr::select(-weight) %>%
  dplyr::select(-c(trip_duration, n_fishers, concentration_g)) %>%
  dplyr::group_by(habitat, nutrient) %>%
  dplyr::summarise(concentration_g_stand = median(concentration_g_stand)) %>%
  dplyr::ungroup()


library(treemapify)

we %>%
  dplyr::left_join(timor.nutrients::RDI_tab, by = "nutrient") %>%
  dplyr::mutate(concentration_g_stand = concentration_g_stand * 1000,
                nutrient = stringr::str_to_title(nutrient),
                nutrient = dplyr::case_when(nutrient %in% c("Selenium", "Vitamina") ~ paste(nutrient, "(μg)"),
                                            nutrient %in% c("Calcium", "Iron", "Zinc") ~ paste(nutrient, "(mg)"),
                                            TRUE ~ paste(nutrient, "(g)")),
                nutrient = ifelse(nutrient == "Vitamina (μg)", "Vitamin-A (μg)", nutrient),
                nutrient = ifelse(nutrient == "Omega3 (g)", "Omega-3 (g)", nutrient)) %>%
  dplyr::mutate(concentration_g_stand = (concentration_g_stand / conv_factor)) %>%
  dplyr::filter(!nutrient == "Selenium (μg)") %>%
  ggplot(aes(area = concentration_g_stand,
             fill = nutrient,
             label = habitat,
             subgroup = nutrient), alpha = 0.5) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre",
                             grow = T,
                             alpha = 0.5,
                             colour = "black",
                             fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)+
  scale_fill_viridis_d()



###
we %>%
  dplyr::left_join(timor.nutrients::RDI_tab, by = "nutrient") %>%
  dplyr::mutate(concentration_g_stand = concentration_g_stand * 1000,
                nutrient = stringr::str_to_title(nutrient),
                nutrient = dplyr::case_when(nutrient %in% c("Selenium", "Vitamina") ~ paste(nutrient, "(μg)"),
                                            nutrient %in% c("Calcium", "Iron", "Zinc") ~ paste(nutrient, "(mg)"),
                                            TRUE ~ paste(nutrient, "(g)")),
                nutrient = ifelse(nutrient == "Vitamina (μg)", "Vitamin-A (μg)", nutrient),
                nutrient = ifelse(nutrient == "Omega3 (g)", "Omega-3 (g)", nutrient)) %>%
  dplyr::mutate(concentration_g_stand = (concentration_g_stand / conv_factor)) %>%
  dplyr::filter(!nutrient == "Selenium (μg)")

