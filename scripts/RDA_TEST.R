

## INPUT FROM SHINY APP=========================================================
env_proxies <- rda_predictor_env
env_molecules <- rda_community_env
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

##(3) TERMS SIGNIFICANCE -------------------------------------------------------
Montecarlo_rda_terms <- anova.cca(myrda, by = "terms", permutations = n_permutation)

##(4) MULTICOLLIBNEARITY -------------------------------------------------------
Multicollinearity <- sqrt(vif.cca(myrda))


## OUTPUTS======================================================================
# GLOBAL
output_global <- list(perm,
                      Montecarlo_rda[1:4],
                      r2_and_r2adj)
## AXIS
axis_significance <- Montecarlo_rda_axis[1:4]

## TERMS
terms_significance <- Montecarlo_rda_terms[1:4]

## MULTICOLLINEARITY
mcol <- data.frame(Multicollinearity)

## RETURN LIST__________________________________________________________________
list(output_global,
     axis_significance,
     terms_significance,
     mcol)
