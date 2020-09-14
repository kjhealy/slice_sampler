### --------------------------------------------------
### Slice sampler example/animation
### --------------------------------------------------


### --------------------------------------------------
### Libraries
### --------------------------------------------------
library(tidyverse)
library(gganimate)
library(transformr)

theme_set(theme_minimal())
### --------------------------------------------------------------------


### --------------------------------------------------
### Slice sampling code adapted from Scott Lynch's example
### --------------------------------------------------

## 3 component normal mixture distribution
fx <- function(x) {
    return(0.4 * dnorm(x, 0, 0.2) + 0.3 * dnorm(x, 0.3, 0.1) + 0.3 * dnorm(x, 0.8, 0.1))
}

## Iterations
maxiter <- 1000

xspan <- seq(-0.5, 1.5, by = 0.01)
w <- 0.1
rtally <- 0

## Prep a tibble for the sampler
sim <- tribble(
  ~ind, ~x, ~q, ~seg1, ~seg2, ~seg3, ~rtally, ~cands,
  seq_along(1:maxiter), 0, 0, # ind, x, q
  tibble(seg1_x0 = 0, seg1_y0 = 0, seg1_x1 = 0, seg1_y1 = 0), # seg1
  tibble(seg2_x0 = 0, seg2_y0 = 0, seg2_x1 = 0, seg2_y1 = 0), # seg2
  tibble(seg3_x0 = 0, seg3_y0 = 0, seg3_x1 = 0, seg3_y1 = 0), # seg3
  0, # rtally
  tibble(cands_i = 0) # cand
) %>% unnest(cols = c(ind))


## Draw maxiter samples, storing information to draw the lines and so on as nested tibbles.
## We're a bit redundant in storing info (e.g. many zeros etc) but this could be
## cleaned up more later
for (i in 2:maxiter) {

  ## First segment
  tmp1 <- list()
  tmp1$seg1_x0 <- sim$x[i - 1]
  tmp1$seg1_y0 <- 0
  tmp1$seg1_x1 <- sim$x[i - 1]
  tmp1$seg1_y1 <- fx(sim$x[i - 1])

  sim$seg1[[i]] <- as_tibble(tmp1)

  ## q or y
  sim$q[i] <- runif(1, 0, fx(sim$x[i - 1]))

  ## lower
  under <- 0
  lower <- sim$x[i - 1]
  while (under == 0) {
    lower <- lower - w
    if (fx(lower) < sim$q[i]) {
      under <- 1
    }

    tmp2 <- NULL
    tmp2$seg2_x0 <- c(tmp2$seg2_x0, sim$x[i - 1])
    tmp2$seg2_y0 <- c(tmp2$seg2_y0, sim$q[i])
    tmp2$seg2_x1 <- c(tmp2$seg2_x1, lower)
    tmp2$seg2_y1 <- c(tmp2$seg2_y1, sim$q[i])
  }
  sim$seg2[[i]] <- as_tibble(tmp2)

  ## upper
  under <- 0
  upper <- sim$x[i - 1]
  while (under == 0) {
    upper <- upper + w
    if (fx(upper) < sim$q[i]) {
      under <- 1
    }

    tmp3 <- NULL
    tmp3$seg3_x0 <- c(tmp3$seg3_x0, sim$x[i - 1])
    tmp3$seg3_y0 <- c(tmp3$seg3_y0, sim$q[i])
    tmp3$seg3_x1 <- c(tmp3$seg3_x1, upper)
    tmp3$seg3_y1 <- c(tmp3$seg3_y1, sim$q[i])
  }
  sim$seg3[[i]] <- as_tibble(tmp3)

  ## candidate/reject points
  reject <- 1
  cands <- NULL
  while (reject == 1) {
    sim$rtally[i] <- sim$rtally[i] + 1
    cand <- runif(1, lower, upper)
    if(fx(cand) > sim$q[i]) {
      reject <- 0
    }
    cands <- c(cands, cand)
  }

  cands <- tibble(cands_i = cands)
  sim$cands[[i]] <- cands
  sim$x[i] <- cand
}

###--------------------------------------------------
### Make some animations
###--------------------------------------------------

## Fully unnest the sim tibble
sim_exp <- sim %>%
  unnest(cols = c(seg1:seg3)) %>%
  unnest(cols = c(cands))


## Animation of the trace of sampled values
trace_panel <- sim %>%
  ggplot(mapping = aes(x = ind, y = x)) +
  geom_point(color = "red", size = 3) +
  geom_line() +
  transition_reveal(ind) +
  scale_x_continuous(limits = c(1, maxiter)) +
  scale_y_continuous(limits = c(-0.5, 1.5)) +
  labs(x = "Iteration", y = "Value") +
  theme(axis.title = element_text(size = rel(6)),
        title = element_text(size = rel(6)))

animate(trace_panel, fps = 25, duration = 60,
        width = 1080, height = 320,
        renderer = ffmpeg_renderer(format = "mov"))


## Animation of the search/draws
base_layer <- tribble(
  ~xspan, ~fxspan,
  xspan, fx(xspan)
) %>%
  unnest(cols = c(xspan, fxspan)) %>%
  ggplot(mapping = aes(x = xspan, y = fxspan)) +
  geom_path(size = 1.2) +
  scale_x_continuous(limits = c(-0.5, 1.5)) +
  labs(x = "x", y = "f(x)") +
  theme(axis.title = element_text(size = rel(6)))

dist <- base_layer +
  geom_point(data = sim_exp, mapping = aes(x = x, y = q), pch = 1, color = "blue", size = 1.3) +
  geom_point(data = sim_exp, mapping = aes(x = x, y = cands_i),  pch = 8, color = "red", size = 1.3) +
  geom_segment(data = sim_exp, mapping = aes(x = seg1_x0, y = seg1_y0, xend = seg1_x1, yend = seg1_y1), size = 1.15, color = "gray50") +
  geom_segment(data = sim_exp, mapping = aes(x = seg2_x0, y = seg2_y0, xend = seg2_x1, yend = seg2_y1), size = 1.15, color = "#f37735") +
  geom_segment(data = sim_exp, mapping = aes(x = seg3_x0, y = seg3_y0, xend = seg3_x1, yend = seg3_y1), size = 1, color = "#f37735") +
  scale_y_continuous(limits = c(0, 1.5)) +
  labs(x = "x", y = "f(x)", title = "Iteration {frame_time}") +
  transition_time(ind) +
  theme(axis.title = element_text(size = rel(6)),
        title = element_text(size = rel(6)))

animate(dist, fps = 25, duration = 60,
        width = 1080,
        height = 480 , renderer = ffmpeg_renderer(format = "mov"))


## Animation of the accumulated draws within the distribution
out <- base_layer +
  geom_point(data = sim, mapping = aes(x = x, y = q, group = ind), pch = 1, size = 2) +
  transition_reveal(ind) +
  labs(x = "x", y = "y") +
  theme(axis.title = element_text(size = rel(6)),
        title = element_text(size = rel(6)))

animate(out, fps = 25, duration = 60,
        width = 1080,
        height = 480, renderer = ffmpeg_renderer(format = "mov"))

