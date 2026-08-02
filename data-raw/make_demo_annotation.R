# ------------------------------------------------------------------------
# Build inst/extdata/demo_annotated.rds
#
# The Shiny demo corpus ships already tokenised, lemmatised and POS tagged, so
# clicking "Load demo corpus" goes straight to the analysis without waiting for
# udpipe. This script produces that file from the udpipe annotation of the
# bundled 1000-document sample (inst/extdata/sample_collection.csv).
#
# The annotation itself was produced with the English GUM treebank:
#   coll   <- read_collection("inst/extdata/sample_collection.csv")
#   model  <- ts_download_model("english", treebank = "GUM")
#   tokens <- preprocess_texts(coll, model = model)
#
# Document ids are renumbered doc_1 ... doc_1000, following the order of the
# rows in sample_collection.csv, so the annotation lines up with
# read_collection(sample_collection.csv, sequential_ids = TRUE).
#
# Run from the package root:
#   Rscript data-raw/make_demo_annotation.R [path/to/themescope_preprocessed.csv]
# ------------------------------------------------------------------------

args    <- commandArgs(trailingOnly = TRUE)
csv_in  <- if (length(args) >= 1) args[1] else "../themescope_preprocessed.csv"
rds_out <- "inst/extdata/demo_annotated.rds"

stopifnot(file.exists(csv_in))

sample_csv <- "inst/extdata/sample_collection.csv"
sample     <- utils::read.csv(sample_csv, stringsAsFactors = FALSE)

annot <- utils::read.csv(csv_in, stringsAsFactors = FALSE, na.strings = c("NA", ""))

# Renumber: original id -> doc_N (N = row number in sample_collection.csv)
lookup <- stats::setNames(paste0("doc_", seq_len(nrow(sample))), sample$doc_id)
missing <- setdiff(unique(annot$doc_id), names(lookup))
if (length(missing) > 0) {
  stop("Annotation contains ", length(missing),
       " documents that are not in sample_collection.csv.")
}
annot$doc_id <- unname(lookup[annot$doc_id])

# Keep the annotation in document order, then in reading order inside a document.
annot <- annot[order(as.integer(sub("^doc_", "", annot$doc_id)),
                     annot$paragraph_id, annot$sentence_id,
                     seq_len(nrow(annot))), , drop = FALSE]
rownames(annot) <- NULL

saveRDS(annot, rds_out, compress = "xz", version = 3)

message("Wrote ", rds_out, ": ", nrow(annot), " tokens, ",
        length(unique(annot$doc_id)), " documents, ",
        round(file.size(rds_out) / 1024^2, 2), " MB")
