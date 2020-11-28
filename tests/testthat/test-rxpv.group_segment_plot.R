test_that("rxpv.group_segment_plot runs correctly", {
  require(rxpViz)
  file_loc <- system.file('extdata', 'example_data.tsv', package = 'rxpViz', mustWork = TRUE)
  dfX <- read.table(file_loc, sep="\t", stringsAsFactors = FALSE, header = 1)
  p = rxpv.group_segment_plot(my_data=dfX, class_colname="gene_id",
                                    Gp1_colname="WT",
                                    Gp2_colname="KO",
                                    order_by="WT",
                                    point_size=2,
                                    point_gp1_col="white",
                                    point_gp2_col="white",
                                    seg_size=5,
                                    seg_gp1_col="black",
                                    seg_gp2_col="black",
                                    ylim_gp1=NULL,
                                    ylim_gp2=NULL)
  expect_success(expect_type(p, 'list'))


})

