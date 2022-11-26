library(data.table)
library(ggplot2)
data(geopotential)

geopotential <- copy(geopotential)[date == date[1]]
geopotential[, gh.z := Anomaly(gh), by = .(lat)]
geopotential[, c("u", "v") := GeostrophicWind(gh.z, lon, lat)]

(g <- ggplot(geopotential, aes(lon, lat)) +
    geom_contour2(aes(z = gh.z), xwrap = c(0, 360)) +
    geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), L = 60,
                    xwrap = c(0, 360)))

# The circular parameter is particularly important for polar coordinates
g + coord_polar()

# If u and v are not converted into degrees/second, the resulting
# streamlines have problems, specially near the pole.
ggplot(geopotential, aes(lon, lat)) +
    geom_contour(aes(z = gh.z)) +
    geom_streamline(aes(dx = u, dy = v), L = 50)

# The step variable can be mapped to size or alpha to
# get cute "drops". It's important to note that ..dx.. (the calculated variable)
# is NOT the same as dx (from the data).
ggplot(geopotential, aes(lon, lat)) +
    geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v), alpha = ..step..,
                        color = sqrt(..dx..^2 + ..dy..^2), size = ..step..),
                        L = 40, xwrap = c(0, 360), res = 2, arrow = NULL,
                        lineend = "round") +
    scale_size(range = c(0, 0.6))

# Using topographic information to simulate "rivers" from slope
topo <- GetTopography(295, -55+360, -30, -42, res = 1/20)  # needs internet!
topo[, c("dx", "dy") := Derivate(h ~ lon + lat)]
topo[h <= 0, c("dx", "dy") := 0]

# See how in this example the integration step is too coarse in the
# western montanous region where the slope is much higher than in the
# flatlands of La Pampa at in the east.
ggplot(topo, aes(lon, lat)) +
    geom_relief(aes(z = h), interpolate = TRUE, data = topo[h >= 0]) +
    geom_contour(aes(z = h), breaks = 0, color = "black") +
    geom_streamline(aes(dx = -dx, dy = -dy), L = 10, skip = 3, arrow = NULL,
                    color = "#4658BD") +
    coord_quickmap()
