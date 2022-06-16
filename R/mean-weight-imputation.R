library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# TODO: make into a function with Area as argument

#dat <- readr::read_csv("data/generated/all-commercial-mean-weight.csv")
#w3cd <- dplyr::filter(dat, area == "3CD")

w3cd <- readr::read_csv(here::here("report/MeanWeightTable_3CD.csv"))
w3cd <- w3cd[,1:2]
colnames(w3cd) <- c("year", "mean_weight")

w3cd <- dplyr::filter(w3cd, year < 2019, !is.na(mean_weight)) # not many samples

diff(w3cd$year)
# TODO missing some years!!

stan_dat <- list(N = length(w3cd$mean_weight), y = log(w3cd$mean_weight), rho_sd = 2, start_observer_effect = min(which(w3cd$year >= 1996)))

fit <- stan(
  "R/ar1ss.stan",
  data = stan_dat,
  chains = 4,
  iter = 2000,
  seed = 10292,
  control = list(adapt_delta = 0.9999999, max_treedepth = 12)
)
fit

p <- extract(fit)
matplot(t(p$y_true), type = "l", col = "#00000010", lty = 1)
yrs <- seq_along(w3cd$mean_weight)
points(yrs, log(w3cd$mean_weight), col = "red")

rho1 <- p$rho1
rho2 <- p$rho2
sigma_p <- p$sigma_p
sigma_o1 <- p$sigma_o1
sigma_o2 <- p$sigma_o2
alpha <- p$alpha
obs_effect <- p$observer_effect

SAMPS <- 1000
y_true <- t(p$y_true)[, 1:SAMPS]
y_obs <- matrix(ncol = ncol(y_true), nrow = nrow(y_true))
for (i in 1:SAMPS) {
  for (.t in 1:stan_dat$N) {
    if (.t < stan_dat$start_observer_effect) {
      y_obs[.t, i] <- rnorm(1, y_true[.t, i], p$sigma_o1[i])
    } else {
      y_obs[.t, i] <- rnorm(1, y_true[.t, i], p$sigma_o2[i])
    }
  }
}

matplot(y_obs, type = "l", col = "#00000010", lty = 1)
yrs <- seq_along(w3cd$mean_weight)
points(yrs, log(w3cd$mean_weight), col = "red")

calc_post <- function(y0, rho2, sigma_p, sigma_o2, alpha, obs_effect, N) {
  y_true <- numeric(length = N)
  for (i in 1:N) {
    if (i == 1) {
      y_true[i] <- y0
    } else {
      y_true[i] <- obs_effect + alpha + rho2 * y_true[i - 1] + rnorm(1, 0, sigma_p)
    }
  }
  rnorm(length(y_true), y_true, sigma_o2)
}

x <- matrix(nrow = 10L, ncol = SAMPS)

for (i in 1:SAMPS) {
  x[, i] <- calc_post(
    y0 = p$y_true[i, ncol(p$y_true)],
    rho2 = p$rho2[i],
    # rho = 1,
    sigma_p = p$sigma_p[i],
    sigma_o2 = p$sigma_o2[i],
    alpha = p$alpha[i],
    obs_effect = p$observer_effect[i],
    N = 10
  )
}
matplot(x, type = "l", col = "#00000080", lty = 1)

all <- rbind(y_obs, x)

library(dplyr)
library(ggplot2)
pp <- reshape2::melt(all) %>%
  rename(year = Var1, iter = Var2)

w3cd$numeric_year <- 1:nrow(w3cd)
# ggplot(pp, aes(year, value, group = iter)) +
#   geom_line(alpha = 0.1) +
#   geom_point(data = w3cd,
#     mapping = aes(numeric_year, mean_weight),
#     inherit.aes = FALSE, colour = "red")

pp %>%
  group_by(year) %>%
  summarise(
    lwr = quantile(value, probs = 0.025),
    lwr2 = quantile(value, probs = 0.25),
    upr2 = quantile(value, probs = 0.75),
    upr = quantile(value, probs = 0.975),
    med = quantile(value, probs = 0.5)
  ) %>%
  ggplot(aes(year, y = exp(med), ymin = exp(lwr), ymax = exp(upr))) +
  geom_line(alpha = 0.1) +
  geom_line(data = subset(pp, iter %in% 1:10), mapping = aes(x = year, y = exp(value), group = iter), alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(alpha = 0.2) +
  geom_ribbon(aes(ymin = exp(lwr2), ymax = exp(upr2)), alpha = 0.2) +
  geom_point(
    data = w3cd,
    mapping = aes(numeric_year, mean_weight),
    inherit.aes = FALSE, colour = "red"
  ) +
  geom_line(
    data = w3cd,
    mapping = aes(numeric_year, mean_weight),
    inherit.aes = FALSE, colour = "red", lwd = 0.2, alpha = 0.9
  )

fake <- data.frame(year = w3cd$numeric_year, value = log(w3cd$mean_weight), iter = NA, real_data = TRUE)
post <- subset(pp, iter %in% 1:8) %>% mutate(real_data = FALSE)

# fake$iter <- 4
# post$iter[post$iter == 4] <- 9

# filter(bind_rows(fake, post), year <= stan_dat$N) %>%
filter(bind_rows(fake, post)) %>%
  ggplot() +
  geom_line(aes(x = year, y = exp(value), colour = real_data), inherit.aes = FALSE) +
  facet_wrap(~iter) +
  scale_colour_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  geom_vline(xintercept = stan_dat$start_observer_effect, lty = 2) +
  geom_vline(xintercept = stan_dat$N, lty = 2)

# RF: now get values to use in models
# Want 2018-2020 ... for 3CD have to skip 2017!
obsyr <- w3cd$year
obsnyr <- length(obsyr)
projyr_ind <- (obsnyr+2):(obsnyr+4)
projyr <- (obsyr[obsnyr]+2):(obsyr[obsnyr]+4)

post_3yproj <- post %>%
  mutate(mean_weight=exp(value)) %>%
  select(-real_data, -value) %>%
  dplyr::filter(year %in% projyr_ind) %>%
  reshape2::dcast(year~iter) %>%
  mutate(year=projyr)

write.csv(post_3yproj,here::here("data/generated/imputed_mw_2018-2020_3CD.csv"))



