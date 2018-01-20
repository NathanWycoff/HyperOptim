#!/usr/bin/Rscript
#  FLXMRglmnet_no_cv.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.06.2018

#A version of the built in that doesn't do CV
require(flexmix)
require(glmnet)

FLXMRglmnet_no_cv <- function (formula = . ~ ., family = c("gaussian", "binomial",
    "poisson"), adaptive = TRUE, select = TRUE, offset = NULL,
    ...)
{
    family <- match.arg(family)
    z <- FLXMRglm(formula = formula, family = family)
    z@preproc.x <- function(x) {
        if (!isTRUE(all.equal(x[, 1], rep(1, nrow(x)), check.attributes = FALSE)))
            stop("The model needs to include an intercept in the first column.")
        x
    }
    z@fit <- function(x, y, w) {
        if (all(!select)) {
            coef <- if (family == "gaussian") 
                lm.wfit(x, y, w = w)$coef
            else if (family == "binomial") 
                glm.fit(x, y, family = binomial(), weights = w)$coef
            else if (family == "poisson") 
                glm.fit(x, y, family = poisson(), weights = w)$coef
        }
        else {
            if (adaptive) {
                coef <- if (family == "gaussian") 
                  lm.wfit(x, y, w = w)$coef[-1]
                else if (family == "binomial") 
                  glm.fit(x, y, family = binomial(), weights = w)$coef[-1]
                else if (family == "poisson") 
                  glm.fit(x, y, family = poisson(), weights = w)$coef[-1]
                penalty <- mean(w)/abs(coef)
            }
            else penalty <- rep(1, ncol(x) - 1)
            if (any(!select)) {
                select <- which(!select)
                penalty[select] <- 0
            }
            m <- glmnet::glmnet(x[, -1, drop = FALSE], y, 
                family = family, weights = w, penalty.factor = penalty, 
                ...)
            coef <- as.vector(coef(m, s = "lambda.min"))
        }
        df <- sum(coef != 0)
        sigma <- if (family == "gaussian") 
            sqrt(sum(w * (y - x %*% coef)^2/mean(w))/(nrow(x) - 
                df))
        else NULL
        z@defineComponent(list(coef = coef, sigma = sigma, df = df + 
            ifelse(family == "gaussian", 1, 0)))
    }
    z
}

