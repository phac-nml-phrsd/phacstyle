
#' Apply the PHAC style to a ggplot object.
#'
#' @param g ggplot object.
#' @param size.base Integer. Base size for text in plot. Default: \code{size.base=12}
#' @param palette String. Color palette name used in the \code{rcartocolor} package.
#' @param color.type String. \code{'d'} for discrete (default), \code{'c'} for continuous
#' @param font.family String. Name of family fonts used. Default: \code{'sans'}.
#'
#' @return A ggplot object with the update style.
#' @export
#'
#' @import ggplot2
#' @importFrom rcartocolor scale_color_carto_d scale_color_carto_c scale_fill_carto_d scale_fill_carto_c
#'
#' @examples
#'
#' # Create the base plot
#' data(Orange)
#' g = ggplot(data = Orange,
#'            aes(x=age, y=circumference, color=factor(Tree))) +
#'   geom_line(size=1) +
#'   labs(
#'     title   = 'Orange growth',
#'     caption = 'Data source: from the `datasets` package',
#'     color   = 'Tree ID'
#'   )
#'
#' # Standard format from ggplot2:
#' plot(g)
#'
#' # Improved style:
#' plot(phac_style(g))
#' plot(phac_style(g + facet_wrap(~Tree)))
#'
#'
phac_style <- function(g,
                       size.base = 12,
                       palette = 'Safe',
                       color.type = 'd',
                       font.family = 'sans') {

  font.size.title       = size.base + 8
  font.size.subtitle    = size.base + 2
  font.size.axis.title  = size.base + 1
  font.size.axis.text   = size.base

  # --- Greys
  grey.dark      = 'grey50'
  grey.light     = 'grey80'
  grey.vlight    = 'grey95'

  # Type of x variable:
  a = g$layers[[1]]$computed_mapping$x
  xvarname = rlang::quo_get_expr(a)
  xtype = class(g$data[[ xvarname ]])

  th = theme(

    text = element_text(family = font.family), # names(pdfFonts())

    # --- Axis
    axis.line.x       = element_line(color = grey.dark),
    axis.line.y       = element_line(color = grey.dark),
    axis.ticks        = element_blank(),
    # axis.ticks        = element_line(color = grey.dark),
    # axis.ticks.length = unit(.3, "lines"),
    axis.title.x      = element_text(size = font.size.axis.title,
                                     margin = margin(t=12),
                                     face= 'bold'),
    axis.title.y      = element_text(size = font.size.axis.title,
                                     margin = margin(r=12),
                                     face = 'bold'),
    axis.text         = element_text(size = font.size.axis.text),

    # --- Grid
    panel.background = element_rect(fill = FALSE, linetype = 0),
    panel.grid.major = element_line(colour = 'grey95'),
    panel.grid.minor = element_line(colour = 'grey97'),

    # --- Legend
    legend.title  = element_text(size = size.base, face = 'bold'),
    legend.background = element_rect(fill = FALSE),
    legend.box.background = element_rect(fill = FALSE, linetype = 0),
    legend.key = element_rect(fill = FALSE),
    legend.text   = element_text(color = grey.dark),

    # --- Title
    plot.title    = element_text(size = font.size.title, face = "bold",
                                 margin = margin(t = 5, b=12)),
    plot.subtitle = element_text(size = font.size.subtitle, color = "grey40",
                                 margin = margin(b=12)),
    plot.title.position = "plot",

    # --- Caption
    plot.caption  = element_text(size = 9, face = 'italic',
                                 margin = margin(t = 15),
                                 color = 'grey50'),
    plot.caption.position = 'plot'
  )

  # --- For bar plots
  if(class(g$layers[[1]]$geom)[1] == 'GeomBar'){
    th = th +
      theme(panel.grid.major.y = element_blank())
  }

  # --- For faceted plots

  # If it's a faceted plot, make the axis text
  # slightly smaller as it will likely be busier:
  if(class(g$facet)[1] != 'FacetNull'){
    th = th +
      theme(
        # --- Panels
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(face = 'bold'),
        strip.background = element_rect(fill  = grey.vlight,
                                        color = FALSE),
        panel.border = element_rect(linetype = 'solid',
                                    fill = FALSE, color = grey.vlight),
        panel.grid.minor = element_blank(),

        # --- Axis
        axis.text.x = element_text(size = font.size.axis.text - 3),
        axis.text.y = element_text(size = font.size.axis.text - 2)
      )
    # If the x-axis text is expected to be long, like a date,
    # introduce an angle to keep a font size large enough:
    if(xtype == 'Date'){
      th = th +
        theme(
          axis.text.x = element_text(size = font.size.axis.text - 3,
                                     angle = 30, hjust = 1))
    }
  }

  # --- Apply the theme
  gg = g + th

  # --- Apply the color palettes
  if(color.type == 'd'){
    gg = gg +
      rcartocolor::scale_color_carto_d(palette = palette) +
      rcartocolor::scale_fill_carto_d(palette = palette)
  }
  if(color.type == 'c'){
    gg = gg +
      rcartocolor::scale_color_carto_c(palette = palette) +
      rcartocolor::scale_fill_carto_c(palette = palette)
  }

  return(gg)
}
