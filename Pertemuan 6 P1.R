library(ggplot2)
library(treemap)
library(networkD3)

# Data ####
midwest
table(midwest$state)

# Treemap ####
state_population <- aggregate(x = midwest$poptotal, by = list(midwest$state), FUN = sum)
treemap(state_population, index="Group.1", vSize="x", type="index")

# Treemap with labels ####
state_population$label <- paste(state_population$Group.1, state_population$x, sep = "\n")
state_population

treemap(state_population, index="label", vSize="x", type="index")
state_population$label

# Treemap with subgroups ####
state_county_pop <- aggregate(x = midwest$poptotal,
                               by = list(midwest$state, midwest$county, midwest$inmetro),
                               FUN = sum)
state_county_pop

treemap(midwest, index=c("state", "county"), vSize="poptotal", type="index")

treemap(state_county_pop, index=c("Group.1", "Group.2"), vSize="x", type="index")
treemap(state_county_pop, index=c("Group.1", "Group.2"), vSize="x", type="index",
        border.lwds = 6)
treemap(state_county_pop, index=c("Group.1", "Group.2"), vSize="x", type="index",
        border.lwds = c(6, 1))

treemap(state_county_pop, index=c("Group.1", "Group.2", "Group.3"), vSize="x", type="index")

# Treemap with subgroups & labels ####
state_county_pop$label <- paste(state_county_pop$Group.2,
                                state_county_pop$x,
                                 sep = "\n")
state_county_pop

treemap(state_county_pop, index=c("Group.1", "label"), vSize="x", type="index")
treemap(state_county_pop, index=c("Group.1", "label"), vSize="x", type="index",
        bg.labels = 0)

# Sankey Diagram ####
links <- aggregate(x = midwest$poptotal,
                   by = list(midwest$state, midwest$inmetro),
                   FUN = sum)
links
colnames(links) <- c("source", "target", "value")

nodes <- data.frame(
    name = unique(
        c(as.character(links$source),
          as.character(links$target))
    )
)
nodes

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
links

p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name",
                   sinksRight = FALSE)
p

javascript_string <- 
    'function(el, x){
    d3.select(el).selectAll(".node text")
      .text(d => d.name + " (" + d.value + ")");
  }'

htmlwidgets::onRender(x = p, jsCode = javascript_string)
