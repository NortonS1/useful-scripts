#######################################################################################
#######################################################################################
########################VORTEX CLUSTER FEQUENCY VIEWER#################################


vortex_cluster_frequency <-
  function(file_name,
           st1 = NULL,
           st2 = NULL,
           st3 = NULL) {
    # Read in data, reformat and aassign variables
    df <- read.csv(file_name)
    sample_ids <- df[, c(1, 3)]
    tissue_id <- df[, 3]
    cluster_id <- df[, 1]
    tissue_id <- as.data.frame(tissue_id)
    cluster_id <- as.data.frame(cluster_id)
    unique_tissues <<- (unique.data.frame(tissue_id))
    unique_clusters <<- (unique.data.frame(cluster_id))
    tissue_names <<- matrix()
    tissue_counts <<- matrix()
    cluster_names <<- matrix()
    
    # Count number of cells per tissue per cluster
    
    # r <<- 1
    for (cluster_name in unique_clusters$cluster_id) {
      for (tissue_name in unique_tissues$tissue_id) {
        tissue_count <- 0
        # r = r + 1
        for (i in 1:length(sample_ids$ClusterID)) {
          if (sample_ids[i, 1] == cluster_name &&
              sample_ids[i, 2] == tissue_name) {
            tissue_count = tissue_count + 1
          }
        }
        # print (r)
        tissue_names <<- append(tissue_names, tissue_name)
        cluster_names <<- append(cluster_names, cluster_name)
        tissue_counts <<- append(tissue_counts, tissue_count)
      }
    }
    result <<- cbind(tissue_names, cluster_names, tissue_counts)
    write.csv(result, file = "cluster_counts_individual.csv")
    
    # Reformat counted data into stacked columns
    print("Count finished")
    k = 1
    result <<- as.data.frame(result)
    column_names <<- as.character(unique_clusters$cluster_id)
    row_names <<- as.character(unique_tissues$tissue_id)
    stacked_data <<- matrix()
    print("stacked_data predefined")
    for (cluster_name in unique_clusters$cluster_id) {
      chunk_id <<- paste("Cluster", cluster_name, sep = "_")
      assign(chunk_id, matrix(ncol = 3))
      temp <<-
        as.data.frame(get(paste("Cluster", cluster_name, sep = "_"))[cluster_name])
      temp <<-
        as.data.frame(result[which(result[, 2] == cluster_name), "tissue_counts"])
      stacked_data <<- cbind2(stacked_data, temp)
      print(k)
      k = k + 1
    }
    
    stacked_data <<- stacked_data[, -1]
    colnames(stacked_data) <<- c(column_names)
    row.names(stacked_data) <<- c(row_names)
    write.csv(stacked_data, file = "cluster_counts_stacked.csv")
    print("Data stacked")
    # Separate data by tissue key word and re-numerise data-table
    
    rows <<- row.names(stacked_data)
    
    if (!is.null(st1)) {
      sample_type_1_rows <<-  grep(st1, rows)
    }
    if (!is.null(st2)) {
      sample_type_2_rows <<-  grep(st2, rows)
    }
    if (!is.null(st3)) {
      sample_type_3_rows <<-  grep(st3, rows)
    }
    
    print("Samples split")
    ################################################################################
    # PLOTTING PART
    plot_cluster_freq <<- function(which_rows, plot_title) {
      spec_sample_data <<- stacked_data[c(which_rows), ]
      indx <<- sapply(spec_sample_data, is.factor)
      spec_sample_data[indx] <<-
        lapply(spec_sample_data[indx], function(x)
          as.numeric(as.character(x)))
      print("data indexed")
      
      # Assign data dimensions
      
      j_length = length(spec_sample_data[1, ])
      i_length = length(spec_sample_data[, 1])
      
      # Generate Frequency data frame for specific tissue
      
      spec_sample_freq <<- data.frame()
      for (i in 1:i_length) {
        rowsum <- sum(spec_sample_data[i, ])
        for (j in 1:j_length) {
          frequency <- spec_sample_data[i, j] / rowsum
          spec_sample_freq[i, j] <<- frequency
        }
      }
      print("frequency calculated")
      # reassign column names/row names
      
      spec_row_names <<- rownames(spec_sample_data)
      colnames(spec_sample_freq) <<- c(column_names)
      rownames(spec_sample_freq) <<- c(spec_row_names)
      print("names assigned")
      
      # Transpose data frame and plot
      
      
      bar_width = as.integer(100/j_length)
      #if bar_width is greater than 10, bar_width = 10
      trans_spec_samples_freq <<- t(spec_sample_freq)
      barplot(
        as.matrix(trans_spec_samples_freq),
        width = c(bar_width), xlim = c(0,100),
        col = rainbow(26),
        cex.names = 0.5,
        las = 2,
        main = plot_title
      )
    }
    print("Begin plotting")
    if (!is.null(st1)) {
      plot_cluster_freq(sample_type_1_rows, st1)
      print("Sample type 1 plotted")
    }
    if (!is.null(st2)) {
      plot_cluster_freq(sample_type_2_rows, st2)
      print("Sample type 2 plotted")
    }
    if (!is.null(st3)) {
      plot_cluster_freq(sample_type_3_rows, st3)
      print("Sample type 3 plotted")
    }
    print("Plotting complete!")
  }







