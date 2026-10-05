## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----data, message=FALSE, warning=FALSE---------------------------------------
library(dbscan)
library(tidyverse)

x <- iris[, 1:4]
db <- x %>% dbscan(eps = .42, minPts = 5)

## ----tidy---------------------------------------------------------------------
tidy(db)

## ----plot, fig.alt="DBSCAN clusters in the iris data"-------------------------
augment(db, x) %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = .cluster, shape = noise)) +
  scale_shape_manual(values = c(19, 4))

