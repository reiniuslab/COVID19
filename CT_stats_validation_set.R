setwd("~/Papers/2020_SARS_CoV2/")

library(data.table)
library(caret)

format.conf.matrix <- function(x, digits = 2){
  out <- c(round(x$overall[c("Accuracy","AccuracyLower","AccuracyUpper","AccuracyNull")]*100,digits),signif(x$overall["AccuracyPValue"],digits+1),round(x$byClass[c("Sensitivity","Specificity")]*100,digits))
  return(out)
}

# load data
ct.val <- fread("validation_set/Benchmark_KS_Reinius_Lab.csv")
colnames(ct.val) <- tolower(colnames(ct.val))

# format as binary
ct.val.bin <- ct.val[,lapply(.SD, function(x) x<50 ), .SDcols = -1]
ct.val.bin[, amb := t1.clinic != t2.clinic]

# construct confusion matrices
cm.val.ls <- list(
  "N1vT1&T2" = ct.val.bin[, confusionMatrix(factor(n1.clinic),reference=factor(t1.clinic&t2.clinic),positive="TRUE")],
  "N1vT1&T2_noamb" = ct.val.bin[amb == F, confusionMatrix(factor(n1.clinic),reference=factor(t1.clinic&t2.clinic),positive="TRUE")],
  "N1vT1|T2" = ct.val.bin[, confusionMatrix(factor(n1.clinic),reference=factor(t1.clinic|t2.clinic),positive="TRUE")],
  "T1vT2" = ct.val.bin[, confusionMatrix(factor(t1.clinic),reference=factor(t2.clinic),positive="TRUE")],
  "T2vT1" = ct.val.bin[, confusionMatrix(factor(t2.clinic),reference=factor(t1.clinic),positive="TRUE")],
  "N1vT1" = ct.val.bin[, confusionMatrix(factor(n1.clinic),reference=factor(t1.clinic),positive="TRUE")],
  "N1vT2" = ct.val.bin[, confusionMatrix(factor(n1.clinic),reference=factor(t2.clinic),positive="TRUE")]
)

# write output
## full stats
sink("validation_set/confusion_matrix_validation_set.txt")
  invisible(sapply(names(cm.val.ls),function(x){
    cat("=====================\n")
    cat(paste0(x,"\n"))
    cat("=====================\n\n")
    print(cm.val.ls[[x]])
    cat(paste0("Exact Accuracy P = ", cm.val.ls[[x]]$overall["AccuracyPValue"],"\n\n\n"))
  }))
sink()

## matrix
write.table(t(sapply(cm.val.ls,format.conf.matrix)),"validation_set/confusion_matrix_stats_validation_set.tsv",quote = F,sep = "\t",col.names = NA)

# plot
library(ggplot2)
library(cowplot)
p.cm.n1vt1t2.noamb <-
  ggplot(ct.val.bin[amb==F], aes(x=t1.clinic&t2.clinic,y=n1.clinic)) +
    geom_jitter(width= 0.3,height = 0.3) +
    labs(x="T1 & T2 Cobas", y="N1 hid-RT-qPCR") +
    theme_minimal()

p.cm.n1vt1ort2 <- 
  ggplot(ct.val.bin, aes(x=t1.clinic|t2.clinic,y=n1.clinic)) +
    geom_jitter(width= 0.3,height = 0.3) +
    labs(x="T1 or T2 Cobas", y="N1 hid-RT-qPCR") +
    theme_minimal()

p.cm.t1vt2 <- 
  ggplot(ct.val.bin, aes(x=t1.clinic, y=t2.clinic)) +
  geom_jitter(width= 0.3,height = 0.3) +
  labs(x="T1 Cobas", y="T2 Cobas") +
  theme_minimal()

ggsave2("plots/validationset_confusionmatrix.pdf",height = 4,width = 12,
  plot_grid(nrow = 1,
    p.cm.n1vt1t2.noamb,p.cm.n1vt1ort2,p.cm.t1vt2
  )
)
