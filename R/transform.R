# transforming layers and grobs -------------------------------------------

#' Transform input grobs as one
#' @param ... grobs to transform
#' @param grob_transform a function that transforms a grob
#' @return a grob containing transformed versions of all input grobs
#' @noRd
transform_grobs = function(..., grob_transform) {
  grob_transform(grobTree(...))
}

#' Transform groblists together. Each groblist represents the grobs for a layer,
#' and is lists of grobs, one for each panel.
#' @param groblists a list of groblists. Each groblist is a list of grobs.
#' @param grob_transform a function that takes a grob and returns a transformed grob
#' @return a single groblist
#' @noRd
transform_groblists = function(groblists, grob_transform) {
  groblist = .mapply(transform_grobs, groblists, list(grob_transform = grob_transform))
  if (length(groblists) > 0) {
    names(groblist) = names(groblists[[1]])
  }
  groblist
}

#' transform layers
#' @param groblists a list of ggplot2::Layers. Should be hidden layers (as
#' returned by hidden_layer()).
#' @param grob_transform a function taking a grob and returning a transformed grob
#' @param check a function to call to check if this transformation is valid (and
#' throw a warning if necessary)
#' @return a ggplot2::Layer
#' @noRd
transform_layers = function(layers, grob_transform, check = function() NULL) {
  force(layers)
  force(grob_transform)
  force(check)

  # skip over hidden layers when transforming (they should already be incorporated
  # into a TransformedLayer) and elements that aren't layers (e.g. coords, scales, etc)
  layers = flatten_layer_list(layers)
  to_transform =
    vapply(layers, inherits, what = "LayerInstance", logical(1)) &
    !vapply(layers, inherits, what = "HiddenLayer", logical(1))
  layers[to_transform] = lapply(layers[to_transform], hide_layer)
  layers_to_transform = layers[to_transform]

  transformed_layer = ggproto("TransformedLayer", geom_blank(inherit.aes = FALSE),
    draw_geom = function(self, data, layout) {
      check()
      groblists = lapply(layers_to_transform, function(l) {
        groblist = l$ggblend__draw_geom_(layout)
        # do not transform within layers
        lapply(groblist, groupGrob)
      })
      transform_groblists(groblists, grob_transform)
    }
  )

  c(layers, list(transformed_layer))
}

#' Make a layer that will transform its contents when created. If the layer does not
#' have a partition aesthetic, transform all its grobs at once. If it does,
#' first generate grobs for each partition, then transform the groups.
#' @param layer a ggplot2::Layer
#' @param grob_transform a function taking a grob and returning a transformed grob
#' @noRd
transform_layer = function(layer, grob_transform, check = function() NULL) {
  force(layer)
  force(grob_transform)
  force(check)

  ggproto(NULL, layer,
    draw_geom = function(self, data, layout) {
      check()
      if (is.null(data$partition)) {
        # absent a partition aes, we apply the transform to all grobs in the layer
        groblist = ggproto_parent(layer, self)$draw_geom(data, layout)
        lapply(groblist, grob_transform)
      } else {
        # with a partition aes, we apply the transform over partitions

        # draw the geom for each partition separately
        # groblists will be a list of groblists, where each groblist is a list
        # of grobs for a single layer
        groblists = lapply(split(data, data$partition), function(d) {
          groblist = ggproto_parent(layer, self)$draw_geom(d, layout)
          # make layers their own group so that the transform is only
          # applied between layers, not within layers
          lapply(groblist, groupGrob)
        })

        transform_groblists(groblists, grob_transform)
      }
    }
  )
}

#' Make a hidden layer that does not draw itself by default. We will draw the
#' layer ourselves when applying transforms
#' @param a ggplot2::Layer to hide
#' @return a ggplot2::Layer whose `$draw_geom()` method does not draw, but
#' saves data for later drawing. Use `$ggblend__draw_geom_()` later to draw.
#' @noRd
hide_layer = function(layer) {
  force(layer)
  store = environment()

  ggproto("HiddenLayer", layer,
    # keep ggplot from drawing this layer normally, we will draw it later
    draw_geom = function(self, data, layout) {
      store$last_grobs = ggproto_parent(layer, self)$draw_geom(data, layout)

      # draw nothing here
      rep(list(zeroGrob()), nrow(layout$layout))
    },
    # function to actually draw the geom, using the grobs saved from the last
    # call to $draw_geom()
    ggblend__draw_geom_ = function(self, layout) {
      store$last_grobs
    }
  )
}
