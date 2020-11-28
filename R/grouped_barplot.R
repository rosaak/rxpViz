#' Grouped Segment Plot
#'
#' @description Make a grouped segmnet plot
#' @param my_data a dataframe that contains \code{class_name}, \code{Gp1_colname} and \code{Gp2_colname}.
#' @param class_colname class column name
#' @param Gp1_colname group1 column name
#' @param Gp2_colname group2 column name
#' @param order_by ordered by gp1 or gp2 column name
#' @param point_size point size default 3
#' @param point_gp1_col gp1 color of point; default "white"
#' @param point_gp2_col gp2 color of point; default "white"
#' @param seg_size segment size
#' @param seg_gp1_col gp1 color of segment; default "black"
#' @param seg_gp2_col gp2 color of segment; default "orange"
#' @param ylim_gp1 ylim for gp1
#' @param ylim_gp2 ylim for gp2
#'
#' @import tidyverse
#' @import ggplot2
#' @import ggpubr
#' @import gridExtra
#' @import grid
#' @importFrom stats reorder
#' @importFrom dplyr select
#'
#' @return a grouped segment plot
#' @export
#'
#' @examples
#'
#' ## load example data
#'
#' file_loc <- system.file('extdata', 'example_data.tsv', package = 'rxpViz', mustWork = TRUE)
#' dfX <- read.table(file_loc, sep="\t", stringsAsFactors = FALSE, header = 1)
#'
#' ## plot the grouped data
#' rxpv.group_segment_plot(my_data=dfX, class_colname="gene_id",
#'                       Gp1_colname="WT",
#'                       Gp2_colname="KO",
#'                       order_by="WT",
#'                       point_size=2,
#'                       point_gp1_col="white",
#'                       point_gp2_col="white",
#'                       seg_size=5,
#'                       seg_gp1_col="black",
#'                       seg_gp2_col="orange"
#'                       )
#'
rxpv.group_segment_plot = function(my_data,
                                 class_colname,
                                 Gp1_colname,
                                 Gp2_colname,
                                 order_by,
                                 point_size=3,
                                 point_gp1_col="white",
                                 point_gp2_col="white",
                                 seg_size=1,
                                 seg_gp1_col="black",
                                 seg_gp2_col="orange",
                                 ylim_gp1=NULL, ylim_gp2=NULL){

  assertthat::assert_that(assertthat::has_name(my_data, class_colname))
  assertthat::assert_that(assertthat::has_name(my_data, Gp1_colname))
  assertthat::assert_that(assertthat::has_name(my_data, Gp2_colname))

  Gp1_title = Gp1_colname
  Gp2_title = Gp2_colname
  suppressMessages({
    attach(my_data)
    Gp1 = get(Gp1_colname)
    Gp2 = get(Gp2_colname)
    order_by = get(order_by)
  })
  if( is.null(ylim_gp1) || is.null(ylim_gp2)){
    ylim_gp1 = my_data %>% dplyr::select(-class_colname) %>% max
    ylim_gp2 = ylim_gp1
  }

  g.mid=ggplot(my_data,aes(x=1,y=reorder(get(class_colname), order_by))) + geom_text(aes(label=get(class_colname)))+
    ggtitle("")+
    ylab(NULL)+
    scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
    theme_minimal() +
    theme(axis.title=element_blank(),
          panel.grid=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_text(color=NA),
          axis.ticks.x=element_line(color=NA),
          plot.margin = unit(c(1,-1,1,-1), "mm"))

  g1 = ggplot(data = my_data, aes(x = reorder(get(class_colname), order_by), y = Gp1)) +
    geom_segment(aes(x=reorder(get(class_colname), order_by), xend=get(class_colname), y=0, yend=Gp1), size = seg_size,lineend = "round", color=seg_gp1_col)+
    geom_point(aes(x=get(class_colname), y=Gp1),stat ="identity", color=point_gp1_col, size=point_size)  + ggtitle(Gp1_title)  + theme_pubclean() +
    theme(text = element_text(size=10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,-1,1,0), "mm")) +
    theme(plot.title = element_text(hjust=0.5)) +
    coord_flip() + ylim(ylim_gp1,0) # + scale_y_reverse()


  g2 = ggplot(data = my_data, aes(x = reorder(get(class_colname), order_by), y = Gp2)) + xlab(NULL)+
    geom_segment(aes(x=reorder(get(class_colname), order_by), xend=get(class_colname), y=0, yend=Gp2), size =seg_size,lineend = "round", color=seg_gp2_col) +
    geom_point(aes(x=get(class_colname), y=Gp2), stat ="identity", color=point_gp2_col, size=point_size) + ggtitle(Gp2_title) + theme_pubclean() +
    theme(text = element_text(size=10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,0,1,-1), "mm")) +
    theme(plot.title = element_text(hjust=0.5)) +
    coord_flip() + ylim(0,ylim_gp2)

  gg1 = ggplot_gtable(ggplot_build(g1))
  gg2 = ggplot_gtable(ggplot_build(g2))
  gg.mid = ggplot_gtable(ggplot_build(g.mid))
  detach(my_data)
  grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/10,2/10,4/10))

}



