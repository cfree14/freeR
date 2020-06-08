
#' Convert a XYZ-layer data frame to a raster brick
#'
#' Converts a XYZ-layer data frame to a raster brick.
#'
#' @param df A dataframe with XYZ info for many layers
#' @param x_col Name of the column with the x info
#' @param y_col Name of the column with the y info
#' @param z_col Name of the column with the z info
#' @param layer_col Name of the column with the layer info
#' @return A raster brick
#' @examples
#' nlayers <- 10
#' df <- expand.grid(layer=1:nlayers, x=1:10, y=1:10) %>%
#'   arrange(layer) %>%
#'   mutate(z=runif(n=n()))
#' df_brick <- df2brick(df=df, x_col="x", y_col="y", z_col="z", layer_col="layer")
#' raster::plot(df_brick)
#' @export
df2brick <- function(df, x_col, y_col, z_col, layer_col){

  # Format data
  df1 <- df %>%
    dplyr::select(layer_col, x_col, y_col, z_col) %>%
    setNames(c("layer_col", "x_col", "y_col", "z_col"))

  # Layer names
  layers <- df1 %>% dplyr::pull(layer_col) %>% unique()

  # Loop through layers and build raster stack
  for(i in 1:length(layers)){

    # Convert layer data frame to raster
    ras <- df1 %>%
      dplyr::filter(layer_col == layers[i]) %>%
      dplyr::select(-layer_col) %>%
      raster::rasterFromXYZ()

    # Add to raster stack
    if(i==1){rstack <- ras}else{rstack <- raster::stack(rstack, ras)}

  }

  # Convert stack to brick
  rbrick <- raster::brick(rstack)
  names(rbrick) <- layers

  # Return raster brick
  return(rbrick)

}
