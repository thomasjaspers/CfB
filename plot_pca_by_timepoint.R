
library(data.table)

# Input
coords <- fread("results/pca/pca_coordinates.tsv")
variance <- fread("results/pca/pca_variance_explained.tsv")

# Sample metadata
metadata <- fread("sample_metadata.tsv")

# Match metadata to PCA samples
coords[, Sample_number := as.integer(sub("Sample_", "", Sample))]

idx <- match(coords$Sample_number, metadata$Sample)

if (anyNA(idx)) {
    stop("Could not match all PCA samples to metadata.")
}

metadata <- metadata[idx]

coords[, Timepoint := factor(
    metadata$Timepoint,
    levels = c("0h", "6h", "1d", "2d", "3d", "4d", "7d", "8d")
)]

coords[, Replicate := factor(metadata$Replicate)]

# Variance explained
pc1 <- variance[
    Principal_Component == "PC1",
    Variance_Explained_Percent
]

pc2 <- variance[
    Principal_Component == "PC2",
    Variance_Explained_Percent
]

# Timepoint colours
time_colors <- c(
    "0h" = "#0072B2",
    "6h" = "#E69F00",
    "1d" = "#009E73",
    "2d" = "#CC79A7",
    "3d" = "#D55E00",
    "4d" = "#56B4E9",
    "7d" = "#F0E442",
    "8d" = "#000000"
)

# Different shapes for replicate series
rep_shapes <- c("1" = 21, "2" = 22, "3" = 24)

plot_pca <- function() {

    par(
        mar = c(5, 5, 4, 12),
        xpd = NA
    )

    plot(
        coords$PC1,
        coords$PC2,
        type = "n",
        xlab = sprintf("PC1 (%.2f%% variance explained)", pc1),
        ylab = sprintf("PC2 (%.2f%% variance explained)", pc2),
        main = "PCA of K562 drug-treatment time course",
        cex.lab = 1.2,
        cex.main = 1.4
    )

    grid(col = "grey85", lty = "dotted")

    points(
        coords$PC1,
        coords$PC2,
        pch = rep_shapes[as.character(coords$Replicate)],
        bg = time_colors[as.character(coords$Timepoint)],
        col = "black",
        cex = 1.5
    )

    text(
        coords$PC1,
        coords$PC2,
        labels = paste0("S", coords$Sample_number),
        pos = 3,
        offset = 0.6,
        cex = 0.85
    )

    abline(h = 0, v = 0, lty = 2, col = "grey70")

    legend(
        "topright",
        inset = c(-0.43, 0),
        legend = names(time_colors),
        fill = time_colors,
        title = "Treatment",
        bty = "n",
        cex = 0.85
    )

    legend(
        "bottomright",
        inset = c(-0.43, 0.1),
        legend = paste("Replicate", names(rep_shapes)),
        pch = rep_shapes,
        title = "Technical replicate",
        bty = "n",
        cex = 0.85
    )
}

# PNG
png(
    "results/pca/pca_by_timepoint.png",
    width = 3000,
    height = 2200,
    res = 300
)

plot_pca()
dev.off()

# PDF
pdf(
    "results/pca/pca_by_timepoint.pdf",
    width = 12,
    height = 9
)

plot_pca()
dev.off()

cat("Annotated PCA completed.\n")
cat(sprintf("PC1: %.2f%%\n", pc1))
cat(sprintf("PC2: %.2f%%\n", pc2))
cat(sprintf("Combined: %.2f%%\n", pc1 + pc2))

