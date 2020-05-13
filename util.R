require("reshape2")
require("ggplot2")

plottopics <- function(example_ids ){
    lda_posterior <- posterior(lda)
    top5termsPerTopicProb <- lda::top.topic.words(lda_posterior$terms, 5, by.score = T)
    topicProportionExamples <- lda_posterior$topics[example_ids, ]
    colnames(topicProportionExamples) <- apply(top5termsPerTopicProb, 2, paste, collapse = " ")

    vizDataFrame <- melt(data = cbind(data.frame(topicProportionExamples), document = docvars(DTM.2[example_ids])$docname), 
                     variable.name = "topic", 
                     id.vars = "document")

    ggplot(data = vizDataFrame, aes(x = topic, y = value, fill = document), ylab = "proportion") +
      geom_bar(stat = "identity", position = "stack") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +  
      coord_flip() + facet_wrap(~document, ncol = length(example_ids))
}