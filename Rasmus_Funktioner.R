
#Grupperer en kategorisk variabel ved brug af en træ-model.
#.data - Datasættet
#.feature - Den kategoriske variabel, som skal grupperes
#.target - Den variabel, som den kategoriske variabel skal prædiktere
#predName - Navnet på kolonnen, som indeholder de prædikterede værdier for .target baseret på modellen
#featureName - Navnet på kolonnen med den grupperede version af den kategoriske variabel - OBS: Den nye variabel kan ikke have samme navn som den gamle
TreeModelGrouping <- function(.data, .feature, .target,
                              predName = "Pred", featureName = NULL, maxdepth = 2, cp = 0){
  #Default er, at erstatte den ugrupperede variabel
  if(is.null(featureName)){
    featureName <- .feature
  }
  
  #Opretter ML task
  Task <- .data %>% 
    dplyr::select(.feature, .target) %>% 
    as_task_regr(target = .target)
  
  #Opretter en træ-model, træner den og genererer prædiktioner
  TreeClassifier <- lrn("regr.rpart", maxdepth = maxdepth, cp = cp)
  TreeClassifier$train(Task)
  Pred <- TreeClassifier$predict(Task)
  
  #Sæt den grupperede faktor på output-datasættet
  if(.feature == featureName){
    .data <- .data %>% select(-.feature)
  }
  .data %>% 
    cbind(data.frame(
      .pred = pred$response,
      .featureName = pred$response %>% factor(labels = 1:length(unique(.)))
    ) %>%
      setNames(c(predName, featureName))
    ) %>% 
    return()
  
}

#Ensure that variables are of the correct type
freMPL1 <- freMPL1 %>% 
  mutate(SocioCateg = SocioCateg %>%
           as.character() %>% 
           map_chr(.f = substr, start = 4, stop = 999) %>% 
           map_dbl(.f = as.numeric) %>% 
           factor(levels = 1:100),
         HasKmLimit = factor(HasKmLimit),
         RiskVar = factor(RiskVar),
         VehAge = fct_relevel(VehAge, "10+", after = 8))

freMPL1 %>% str()

# colnames(freMPL1 %>% select(-ClaimInd, -ClaimAmount)) %>% 
#   map(.f = function(feature){
#     
#     if(is.factor(freMPL1[[feature]])){
#       print(feature)
#       freMPL1 %>% 
#         group_by(!!rlang::sym(feature)) %>% 
#         summarise(Frek = mean(ClaimInd)) %>% 
#         ggplot(aes(x = !!rlang::sym(feature), y = Frek)) +
#         geom_point() %>% return()
#     }
#     
#   })

#Hvis man kører hele dokumentet, skriver følgende linje, at man har gjort det. Bare for at gøre opmærksom.
print("Sourced Rasmus_Funktioner.")
