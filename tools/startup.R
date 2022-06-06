# fonts and palettes

font_add_google("Oswald", "oswald")
showtext_auto()

my_pink <<- c("#42BFDD", "#BBE6E4", "#F0F6F6", lighten("#FF66B3", 0.4), "#FF66B3")
d10 <<- c("#788FCE", "#BD8184", "#E6956F", "#F2CC8F", "#A6BA96", "#C5E8E3", "#F4F1DE", "#CDC3D4", "#A88AD2", "#60627C")

# geom_rrect
# taken from hrbrmstr/statebins
geom_rrect <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRrect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRrect <- ggplot2::ggproto("GeomRrect", ggplot2::Geom,

                              default_aes = ggplot2::aes(
                                colour = NA, fill = "grey35", size = 0.5, linetype = 1, alpha = NA
                              ),

                              required_aes = c("xmin", "xmax", "ymin", "ymax"),

                              draw_panel = function(self, data, panel_params, coord,
                                                    radius = grid::unit(6, "pt")) {

                                coords <- coord$transform(data, panel_params)

                                lapply(1:length(coords$xmin), function(i) {

                                  grid::roundrectGrob(
                                    coords$xmin[i], coords$ymax[i],
                                    width = (coords$xmax[i] - coords$xmin[i]),
                                    height = (coords$ymax[i] - coords$ymin)[i],
                                    r = radius,
                                    default.units = "native",
                                    just = c("left", "top"),
                                    gp = grid::gpar(
                                      col = coords$colour[i],
                                      fill = alpha(coords$fill[i], coords$alpha[i]),
                                      lwd = coords$size[i] * .pt,
                                      lty = coords$linetype[i],
                                      lineend = "butt"
                                    )
                                  )

                                }) -> gl

                                grobs <- do.call(grid::gList, gl)

                                ggname("geom_rrect", grid::grobTree(children = grobs))

                              },

                              draw_key = ggplot2::draw_key_polygon

)


ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
