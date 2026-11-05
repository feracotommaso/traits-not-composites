# Figure 1: hierarchical model for extraversion (path diagram)

library(ggplot2)
library(ggforce)

#### nodes ####
fac = data.frame(
  lab = c("Warmth", "Gregariousness", "Assertiveness", "Activity",
          "Excitement seeking", "Positive emotions"),
  x = 4.9, y = 6:1)
a = 1.02; b = 0.32         # facet ellipses (half-axes)
xE = 1.6; yE = 3.5; rE = 0.45
xS = 6.6; rS = 0.2
xY = 8.7; yY = 4
xU = 8.1; yU = 5.7; rU = 0.3

# E -> facets: from the circle of E to the ellipse boundary of each facet
dx = xE - fac$x; dy = yE - fac$y; d = sqrt(dx^2 + dy^2)
t = 1 / sqrt((dx / d / a)^2 + (dy / d / b)^2)
arrE = data.frame(x = xE - rE * dx / d, y = yE - rE * dy / d,
                  xend = fac$x + t * dx / d, yend = fac$y + t * dy / d)

arr = arrow(length = unit(1.6, "mm"), type = "closed")

#### plot ####
p = ggplot() +
  # paths
  geom_segment(data = arrE, aes(x, y, xend = xend, yend = yend), arrow = arr, linewidth = .4) +
  geom_segment(aes(x = xS - rS, y = fac$y, xend = fac$x + a, yend = fac$y), arrow = arr, linewidth = .4) +
  geom_segment(aes(x = .35, y = yE, xend = xE - rE, yend = yE), arrow = arr, linewidth = .4) +
  geom_segment(aes(x = xS + rS, y = yY, xend = xY - .65, yend = yY), arrow = arr, linewidth = .4) +
  geom_path(aes(x = c(xE, xE, xY, xY), y = c(yE - rE, .3, .3, yY - .35)), arrow = arr, linewidth = .4) +
  geom_segment(aes(x = xU - .22, y = yU - .2, xend = xS + .15, yend = yY + .15),
               arrow = arr, linewidth = .4, linetype = "dashed") +
  geom_segment(aes(x = xU + .15, y = yU - .26, xend = xY, yend = yY + .35),
               arrow = arr, linewidth = .4, linetype = "dashed") +
  # nodes
  geom_ellipse(aes(x0 = fac$x, y0 = fac$y, a = a, b = b, angle = 0), fill = "white") +
  geom_circle(aes(x0 = xE, y0 = yE, r = rE), fill = "grey90") +
  geom_circle(aes(x0 = xS, y0 = fac$y, r = rS), fill = "white") +
  geom_circle(aes(x0 = xU, y0 = yU, r = rU), fill = "white", linetype = "dashed") +
  geom_rect(aes(xmin = -.75, xmax = .35, ymin = yE - .35, ymax = yE + .35), fill = "white", colour = "black") +
  geom_rect(aes(xmin = xY - .65, xmax = xY + .65, ymin = yY - .35, ymax = yY + .35), fill = "white", colour = "black") +
  # labels
  geom_text(aes(x = fac$x, y = fac$y, label = fac$lab), size = 3) +
  geom_text(aes(x = xS, y = fac$y, label = "S"), size = 3, fontface = "italic") +
  geom_text(aes(x = xE, y = yE, label = "E"), size = 4, fontface = "italic") +
  geom_text(aes(x = xU, y = yU, label = "U"), size = 3.5, fontface = "italic") +
  geom_text(aes(x = -.2, y = yE, label = "Reward\nsensitivity"), size = 3, lineheight = .9) +
  geom_text(aes(x = xY, y = yY, label = "Emergent\nleadership"), size = 3, lineheight = .9) +
  coord_fixed() +
  theme_void()

ggsave("figure1.png", p, width = 6.5, height = 4.3, dpi = 300, bg = "white")
