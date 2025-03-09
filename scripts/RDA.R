

## INPUT FROM SHINY APP=========================================================
n_permutation <- rda_perm_num

## DEFINE PREDICTORS and COMMUNITY
predictors <- env_proxies
community <- as.matrix (env_molecules)

# RUN RDA ======================================================================
myrda <- rda(community ~ ., data = predictors)


## RDA MODEL TEST ==============================================================
##(1) GLOBAL MODEL -------------------------------------------------------------
# 1a. (N° PERMUTATION)__________________________________________________________
perm <- paste("\nN° permutation: ", as.character(n_permutation))

# 1b. (GLOBAL SIGNIFICANCE)
Montecarlo_rda <- anova.cca(myrda, permutations = n_permutation)

# 1c. (R SQUARED)
rsq <- RsquareAdj(myrda)
r2_and_r2adj <- paste("\nR-Squared:", rsq[1], "\nR-Squared adjusted:", rsq[2])

##(2) RDA AXIS SIGNIFICANCE ----------------------------------------------------
Montecarlo_rda_axis <- anova.cca(myrda, by = "axis",permutations = n_permutation)
axis_significance <- Montecarlo_rda_axis[1:4]

##(3) TERMS SIGNIFICANCE -------------------------------------------------------
Montecarlo_rda_terms <- anova.cca(myrda, by = "terms", permutations = n_permutation) # Test which terms are significant
terms_significance <- Montecarlo_rda_terms[1:4]


## COLUMN 4 --------------------------------------------------------------------
# 4. (Multicollinearity) if >2 should be indicative of Multicollinearity
Multicollinearity <- sqrt(vif.cca(myrda))
mcol <- data.frame(Multicollinearity)

## OUTPUTS======================================================================
# GLOBAL
output_global <- list(perm,
                      Montecarlo_rda[1:4],
                      r2_and_r2adj)
list(output_global,
     axis_significance,
     terms_significance,
     mcol)

