# annual-report.R

library(readr)
pubs <- read_csv("FME Paper Tracking - Submitted and Published Manuscripts.csv",
                 skip = 6,
                 col_names = c("Mentor", "Process", "Last_update", "Lead_COMPASS_author",
                               "Authors", "COMPASS_lead", "T1", "T2", "T3",
                               "Synthesis", "TEMPEST", "EXCHANGE", "Submit_date",
                               "PY", "ESS_DIVE_pub_date", "Comms",
                               "Title", "Journal", "DOI_for_crossref", "URL",
                               "Highlight", "Notes", "X1", "X2", "X3", "X4"))

pubs_compass <- subset(pubs, COMPASS_lead %in% c("Y", "y", "Yes", "yes") &
                           !is.na(DOI_for_crossref))

message("Getting crossref data...")
library(rcrossref)
x <- cr_works(pubs_compass$DOI_for_crossref)

message("Extracting author information...")
author_lists <- x$data$author
author_lists <- lapply(author_lists, function(al) {
    al[c("family", "sequence")]
})
names(author_lists) <- x$data$doi

authors <- dplyr::bind_rows(author_lists, .id = "doi")
authors$family <- utf8::as_utf8(authors$family)

find_person <- function(name) {
    for(i in x$data$author) {
        if(name %in% i$family) {
            w <- which("affiliation.name" == names(i))
            if("affiliation.name" %in% names(i)) {
                print(i[w,])
                print(i[w, "affiliation.name"])
                stop("Found")
            } else {
                print(i[w, ])
                warning("Found but no affiliation column")
            }
        }
    }
    stop("NOT FOUND")
}

# Clean up family names
message("Cleaning names...")
cleans <- read_csv("name_cleanup.csv", col_types = "ccc")

# Construct author-coauthor pairs
library(dplyr)
library(tidyr)
authors %>%
    left_join(cleans, by = c("family" = "Name")) %>%
    rename(author = Name_clean) %>%
    # Make authors pairings
    group_by(doi) %>%
    mutate(first_author = author[1]) %>%
    ungroup() %>%
    select(-family, -sequence) ->
    authors

# Count how many times people are on papers together
message("Counting coauthorships...")
authors %>%
    rowwise() %>%
    mutate(author_A = if_else(first_author < author, first_author, author),
           author_B = if_else(first_author < author, author, first_author)) %>%
    group_by(author_A, author_B) %>%
    summarise(weight = n(), .groups = "drop") %>%
    filter(author_A != author_B) ->
    author_pairings

# Create colors based on institutions
message("Making color scheme...")
read_csv("institutions.csv", col_types = "ccc") %>%
    arrange(Name_clean) %>%
    replace_na(list(Institution = "")) %>%
    mutate(Inst_n = as.numeric(as.factor(Institution))) ->
    institutions
library(viridis)
cols <- turbo(length(unique(institutions$Institution)))
names(cols) <- unique(institutions$Institution)
cols[""] <- "#D3D3D3" # light gray
institutions$color <- cols[institutions$Inst_n]

# Create the graph
message("Creating graph...")
library(igraph)
g <- graph_from_data_frame(author_pairings, directed = FALSE)
# ...and assign attributes
vnames <- V(g)$name
V(g)$color <- institutions$color[match(vnames, institutions$Name_clean)]
V(g)$size <- 5
V(g)$label.degree = -pi/2
V(g)$label.dist = 0.5
E(g)$width <- 2 ^ (E(g)$weight - 1)

# Have names only for people with significant number of connections
deg <- degree(g)
lows <- names(deg)[deg < 5]
V(g)$size[match(lows, V(g)$name)] <- 2
V(g)$name[match(lows, V(g)$name)] <- ""

# Everything
png("test.png", width = 480 * 4, height = 480 * 2)
plot(g, layout = layout_with_fr)
dev.off()

g_compass <- delete_vertices(g, which("" == V(g)$name))
g_compass <- delete_vertices(g_compass, which("Feron" == V(g_compass)$name))
g_compass <- delete_vertices(g_compass, which("Enguehard" == V(g_compass)$name))
g_compass <- delete_vertices(g_compass, which("Forbes" == V(g_compass)$name))
plot(g_compass, layout = layout_with_fr)

test_layout <- layout_in_circle(g, order = order(V(g_compass)$color))
test_layout <- test_layout[1:27,]
plot(test_layout)
V(g_compass)$label.dist = 0.75
plot(g_compass, layout = test_layout)


# https://stackoverflow.com/questions/45720062/grouping-igraph-vertices-in-a-weighted-network-by-color-subgroup-in-r
GroupByVertex01 = function(Groups, spacing = 5) {
    Position <- (order(Groups) + spacing * Groups)
    Angle <- Position * 2 * pi / max(Position)
    matrix(c(cos(Angle), sin(Angle)), ncol = 2)
}


Groups <- as.numeric(as.factor(V(g_compass)$color))
GBV1 <- GroupByVertex01(Groups)
plot(g_compass, layout = GBV1)
