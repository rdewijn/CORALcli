#' Define a function to create a vector of colors based on selected kinases
#'
#' @param df coral tree dataframe
#' @param sel selected kinases
#' @param bg.col background color
#' @param sel.col selected color
#'
#' @return vector with color values
color.by.selected <- function(df,sel,bg.col,sel.col)
{

 # set background color
 color.vector = rep(bg.col,nrow(df))

 # recolor selected kinases
 if (length(sel) > 0)
 {
  color.vector[which(df$id.coral %in% sel)] = sel.col
 }

 # return color vector
 return (color.vector)
}

#' Define a function creates color vector from group
#'
#' @param df coral tree df
#' @param recolordf df to be recolored
#' @param colors colors
#' @param bg.col background color
#' @param categories kinase group
#'
#' @return list of colors
color.by.group <- function(df,recolordf,colors,bg.col="#D3D3D3",categories=NULL)
{
 # set background color
 color.vector = rep(bg.col,nrow(df))

 # keep track of group labels
 group.vector = rep("none",nrow(df))

 # get group names
 group.names = names(table(recolordf[,2]))

 # used user supplied groups
 if (is.null(categories) == FALSE)
 {
  group.names = categories
 }

 # Determine the number of groups
 numgroups = length(group.names)

 # set palette
 if (numgroups > length(colors))
 {
  colors = colorRampPalette(colors)(numgroups)
 }
 pal = colors[1:numgroups]

 groupcolormapping = c()
 for (i in 1:numgroups)
 {
  # get group and color
  group.name  = group.names[i]
  group.color = pal[i]

  # record the group/color mapping
  groupcolormapping = c(groupcolormapping,group.color)
  names(groupcolormapping)[length(groupcolormapping)] = group.name

  # get kinases from this group
  kinsase.from.this.group = recolordf[which(recolordf[,2]==group.name),1]

  # update vector of colors
  color.vector[which(df$id.coral %in% kinsase.from.this.group)] = group.color
  group.vector[which(df$id.coral %in% kinsase.from.this.group)] = group.name
 }

 return( list(color.vector,group.vector,groupcolormapping))
}


#' Define a function creates color vector from values
#'
#' @param df coral tree df
#' @param recolordf df to be recolored
#' @param colors colors
#' @param heatrange range
#' @param bg.col background color
#'
#' @return vector with color
color.by.value <- function(df ,recolordf ,colors  ,heatrange , bg.col="#D3D3D3")
{
 # set background color
 color.vector = rep(bg.col,nrow(df))

 # kep track of group labels
 value.vector = rep(NA,nrow(df))

 # convert to numeric
 recolordf[,2] = as.numeric(recolordf[,2])

 # convert to numeric
 recolordf$color = map2color(recolordf[,2],pal = colors, limits = heatrange)

 # find indices to recolor
 dflookup = match(recolordf[,1],df[,1])

 # update colors and values
 color.vector[dflookup] = recolordf$color
 value.vector[dflookup] = recolordf[,2]

 return (list(color.vector, value.vector))
}

#' Define a function creates radius vector from values
#'
#' @param df coral tree df
#' @param resizedf df to be resized
#' @param sizerange range of sizes
#' @param controlledrange default = FALSE
#' @param minvalue default = 0
#' @param maxvalue default = 5
#' @param showall default = "show"
#'
#' @return
resizes.by.value <- function(df, resizedf, sizerange, controlledrange = FALSE, minvalue=0, maxvalue = 5,showall="show")
{
 # set values for non supplied kinases
 radius.vector = rep(0,nrow(df))
 if (showall == "show"){radius.vector = rep(sizerange[1],nrow(df))}

 # keep track of group labels
 value.vector = rep(NA,nrow(df))

 # convert to numeric
 resizedf[,2] = as.numeric(resizedf[,2])

 if (controlledrange == FALSE)
 {
  # (1) get range
  rangesize = sizerange[2] - sizerange[1]

  minvalue = min(resizedf[,2])
  maxvalue = max(resizedf[,2])
 }

 # if controlledrange == TRUE
 if (controlledrange == TRUE)
 {
  # truncate values beyond the range
  resizedf[,2][which(resizedf[,2] < minvalue)] = minvalue
  resizedf[,2][which(resizedf[,2] > maxvalue)] = maxvalue

  # (1) get range
  rangesize = sizerange[2] - sizerange[1]
 }

 # (2) shift values such that they start at zero
 radii = resizedf[,2] - minvalue

 # (3) scale so max = 1
 radii[which(radii !=0)] = radii[which(radii !=0)] / maxvalue

 # (3) multiply to fit range
 radii = radii * rangesize

 # (4) increase all values to be within range
 radii = radii+ sizerange[1]

 resizedf$radii = radii

 # find indices to resize
 dflookup = match(resizedf[,1],df[,1])

 # update colors and values
 radius.vector[dflookup] = resizedf$radii
 value.vector[dflookup] = resizedf[,2]

 return (list(radius.vector, value.vector))
}

#' Define a function the converts input identifiers to coral IDs
#'
#' @param df coral tree df
#' @param recolordf df to be recolored
#' @param inputtype input id type
#'
#' @return recolordf with ids converted to coral.id
convertID <- function(df,recolordf,inputtype)
{
 if (inputtype == "coralID")
 {
  inputtype = "id.coral"
 }
 if (inputtype != "id.coral")
 {
  inputtype = paste("id.",inputtype,sep="")
 }

 if (length(which(recolordf[,1] %in% df[,inputtype])) == 0)
 {
  return(data.frame())
 }


 # filter input df for those found in table
 recolordf = recolordf[which(recolordf[,1] %in% df[,inputtype]),]
 # Make a new recolordf accounting for the fact that one other ID could correspond
 # to multiple coral IDs
 newids = c()
 newvalues = c()
 for (i in 1:nrow(recolordf))
 {
  otherid  = recolordf[i,1]
  quantval = recolordf[i,2]
  coralids = as.character(df[which(as.character(df[,inputtype]) == otherid),1])
  for (coralid in coralids)
  {
   newids = c(newids,coralid)
   newvalues = c(newvalues,quantval)
  }
 }
 recolordfconverted = data.frame(ids=newids,values=newvalues,stringsAsFactors = F)

 return(recolordfconverted)
}



#' Define a function that makes boxes and labels for group type legends
#'
#' @param x
#' @param y
#' @param color
#' @param width
#' @param height
#' @param label
#' @param fontsize
#' @param elementtype
#' @param fontfamily
#'
#' @return legend element
build.group.legend.elements <- function(x=99.208,y,color,width=6.584,height=6.584,label="group",fontsize=5,elementtype="Branch",fontfamily="'AvenirNext-Bold'")
{
 # build the square
 square = paste("<rect x=\"", x,"\"",
                " y=\"", y, "\"",
                " fill=\"", color, "\"",
                " width=\"", width, "\"",
                " height=\"", height,"\"/>",
                sep="")

 # build the circle
 circle = paste("<circle cx=\"", x + width/2,"\"",
                " cy=\"", y+ width/2 , "\"",
                " fill=\"", color, "\"",
                " r=\"", width/2, "\"/>",
                sep="")


 # build the text
 textx = 110.8889
 texty = y + 4.5
 text = paste("<text x=\"", textx,"\"",
              " y=\"", texty, "\"",
              " font-weight=\"700\" ",
              " font-size=\"", fontsize, "\"",
              " font-family=\"", fontfamily,"\">",
              label,"</text>",
              sep="")

 if (elementtype == "Branch")
 {
  return(c(square,text))
 }

 if (elementtype == "Node")
 {
  return(c(circle,text))
 }

}

#' Define a function that builds a legend for group color
#'
#' @param yoffset
#' @param groupslabels
#' @param groupcolors
#' @param elementtype
#' @param fontfamily
#'
#' @return group legend
build.group.legend  <- function(yoffset=0,groupslabels,groupcolors,elementtype = "Branch",fontfamily="'AvenirNext-Bold'")
{
 # write the header
 header = paste("<text x=\"98.8075\"",
                " y=\"", yoffset + 8.8451, "\"",
                " font-family=\"", fontfamily, "\" ",
                " font-weight=\"700\" ",
                " font-size=\"9px\">", elementtype," Color</text>",
                sep="")

 # write the grey line
 greylineheight = 14 * length(groupslabels) + 14
 greyline       = paste("<rect x=\"", 89.807,"\"",
                        " y=\"", yoffset, "\"",
                        " fill=\"", "#D3D3D3", "\"",
                        " width=\"", 2.333, "\"",
                        " height=\"", greylineheight,"\"/>",
                        sep="")

 # add all of the labels
 legendstuff = c()
 yoffset = yoffset + 19
 for (i in 1:length(groupslabels))
 {
  legendstuff = c(legendstuff,
                  build.group.legend.elements(
                   x=99.208,
                   y=yoffset,
                   color = groupcolors[i],
                   width=6.584,
                   height=6.584,
                   label=groupslabels[i],
                   fontsize=5,
                   fontfamily=fontfamily,
                   elementtype)
  )
  yoffset = yoffset + 14
 }

 return (list(c(header,greyline,legendstuff),yoffset))
}


#' Define a function that draws a rect
#'
#' @param x
#' @param y
#' @param fill
#' @param width
#' @param height
#'
#' @return rectangle
drawrect <- function(x,y,fill,width=6.584,height=11.27)
{
 rectline = paste("<rect x=\"",x,"\"",
                  " y=\"",y,"\"",
                  " fill=\"",fill,"\"",
                  " width=\"",width,"\"",
                  " height=\"",height, "\"/>",
                  sep = "")

 return (rectline)
}


#' Define a function that builds a legend for values
#'
#' @param yoffset
#' @param minval
#' @param maxval
#' @param palette
#' @param elementtype
#' @param fontfamily
#' @param subtitle
#'
#' @return legend for values
build.value.legend  <- function(yoffset=0,minval,maxval, palette,elementtype = "Branch",fontfamily="'AvenirNext-Bold'",subtitle="test")
{
 # write the header
 header = paste("<text x=\"98.8075\"",
                " y=\"", yoffset + 8.8451, "\"",
                " font-family=\"", fontfamily, "\" ",
                " font-weight=\"700\" ",
                " font-size=\"9px\">", elementtype," Color</text>",
                sep="")

 subtitle.height = 0
 if (subtitle != ""){subtitle.height = 8.8451}

 # write the grey line
 greylineheight = 41.58 + subtitle.height
 greyline       = paste("<rect x=\"", 89.807,"\"",
                        " y=\"", yoffset, "\"",
                        " fill=\"", "#D3D3D3", "\"",
                        " width=\"", 2.333, "\"",
                        " height=\"", greylineheight,"\"/>",
                        sep="")

 # add the subtitle
 if (subtitle != ""){

  yoffset = yoffset + 8.8451

  subtitleline = paste("<text x=\"98.8075\"",
                       " y=\"", yoffset + 8.8451*1.5, "\"",
                       " font-family=\"", fontfamily, "\" ",
                       " font-weight=\"700\" ",
                       " font-size=\"7px\">", subtitle,"</text>",
                       sep="")
 }

 # add the gradient
 heatrange = seq(minval,maxval,length.out = 11)
 legcols = map2color(x=heatrange,pal=palette,limits=NULL)

 # Draw the rectangle
 rects = c()
 for (i in 1:11)
 {
  rects = c(rects, drawrect (x=92.632 + (6.576 * i), y=yoffset + 26.51 ,fill=legcols[i],width=6.584,height=11.27))
 }

 text.min = paste("<text x=\"", 98.8075,"\"",
                  " y=\"", yoffset + 23.1251, "\"",
                  " font-size=\"", "5px", "\"",
                  " font-weight=\"700\" ",
                  " font-family=\"", fontfamily, "\">",
                  minval,"</text>",
                  sep="")

 text.mid = paste("<text x=\"", 133.8944,"\"",
                  " y=\"", yoffset + 23.1251, "\"",
                  " font-size=\"", "5px", "\"",
                  " font-weight=\"700\" ",
                  " font-family=\"", fontfamily, "\">",
                  mean(c(minval , maxval)),"</text>",
                  sep="")

 text.max = paste("<text x=\"", 166.7776,"\"",
                  " y=\"", yoffset + 23.1251, "\"",
                  " font-size=\"", "5px", "\"",
                  " font-weight=\"700\" ",
                  " font-family=\"", fontfamily,"\">",
                  maxval,"</text>",
                  sep="")

 # assemble output
 output = c(header, greyline, rects, text.min, text.mid, text.max)
 if (subtitle != "")
 {
  output = c(header, subtitleline, greyline, rects, text.min, text.mid, text.max)
 }

 yoffset = yoffset + 41.58
 return(list(output,yoffset))
}





#' Define a function that builds a legend for values
#'
#' @param yoffset
#' @param minval
#' @param maxval
#' @param minsize
#' @param maxsize
#' @param fontfamily
#' @param subtitle
#'
#' @return nodesize legend
build.nodesize.legend  <- function(yoffset=0,minval,maxval,minsize ,maxsize,fontfamily="'AvenirNext-Bold'",subtitle="test")
{
 extrayoff = 0

 if (maxsize > 6)
 {
  extrayoff = maxsize - 6
 }

 # write the header
 header = paste("<text x=\"98.8075\"",
                " y=\"", yoffset + 8.8451, "\"",
                " font-weight=\"700\" ",
                " font-family=\"", fontfamily, "\" ",
                " font-size=\"9px\">","Node Size</text>",
                sep="")

 subtitle.height = 0
 if (subtitle != ""){subtitle.height = 8.8451}

 # write the grey line
 greylineheight = 41.58 + 2 * extrayoff + subtitle.height
 greyline       = paste("<rect x=\"", 89.807,"\"",
                        " y=\"", yoffset, "\"",
                        " fill=\"", "#D3D3D3", "\"",
                        " width=\"", 2.333, "\"",
                        " height=\"", greylineheight,"\"/>",
                        sep="")

 # add the subtitle
 if (subtitle != ""){

  yoffset = yoffset + 8.8451

  subtitleline = paste("<text x=\"98.8075\"",
                       " y=\"", yoffset + 8.8451*1.5, "\"",
                       " font-family=\"", fontfamily, "\" ",
                       " font-weight=\"700\" ",
                       " font-size=\"7px\">", subtitle,"</text>",
                       sep="")
 }

 # Make circles
 circles = c()

 xs = c(100.266,109.45,120.846,134.454,150.273,168.303)

 sizes = seq(minsize,maxsize,length.out = length(xs))

 for (i in 1:length(xs))
 {
  circle = paste("<circle cx=\"", xs[i] ,"\"",
                 " cy=\"", yoffset + 33.932 + extrayoff, "\"",
                 " fill=\"", "#D3D3D3", "\"",
                 " stroke=\"white\"",
                 " r=\"", sizes[i], "\"/>",
                 sep="")
  circles = c(circles,circle)
 }

 # add text labels
 text.min = paste("<text x=\"",  min(xs),"\"",
                  " y=\"", yoffset + 23.1251, "\"",
                  " font-size=\"", "5px", "\"",
                  " text-anchor=\"middle\"",
                  " font-weight=\"700\" ",
                  " font-family=\"", fontfamily, "\">",
                  minval,"</text>",
                  sep="")
 text.max = paste("<text x=\"",  max(xs),"\"",
                  " y=\"", yoffset + 23.1251, "\"",
                  " font-size=\"", "5px", "\"",
                  " text-anchor=\"middle\"",
                  " font-weight=\"700\" ",
                  " font-family=\"", fontfamily, "\">",
                  maxval,"</text>",
                  sep="")

 # asssemble output
 output = c(header, greyline, circles, text.min, text.max)
 if (subtitle != "")
 {
  output = c(header, greyline,subtitleline,  circles, text.min, text.max)
 }
 yoffset = yoffset + 41.58 + extrayoff
 return(list(output,yoffset))
}

#' Define a function that maps numbers to colors
#'
#' @param x
#' @param pal
#' @param limits
#'
#' @return vector of colors
map2color <- function(x=NULL,pal=colorRampPalette(c("deepskyblue2","black","gold"))(100),limits=NULL)
{

 # set the limits to the min and max
 if(is.null(limits))
 {
  # get just the finite values
  finitevalues = x[which(is.finite(x)==TRUE)]

  # set the limits
  limits = range(x)
 }

 # get the new colors
 newcolors = pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]

 # return the new colors
 return(newcolors)
}


#' Define a function that writes the group names
#'
#' @param l
#' @param font
#' @param groupcolor
#'
#' @return grouplabels
build.group.labels <- function(l,font,groupcolor)
{

 # change color
 colortag = paste(" fill=\"",groupcolor,"\" font-family",sep="")
 l = gsub(pattern = "font-family",colortag,l)

 # use correct font
 grouplabel = gsub(pattern = "'Roboto-Bold'",font,l)


 # make bold
 grouplabel = gsub(pattern = "letter-spacing","font-weight=\"700\" letter-spacing",grouplabel)




 return(grouplabel)
}

#' Define a function that make a branch
#'
#' @param l
#'
#' @return branch
build.branch <- function(l)
{
 branch = paste("<path id=\"b_x5F_",l["id.coral"],
                "\" fill=\"",l["branch.col"],
                "\" d=\"",l["branch.coords"],"\"/>",sep = "")
 return(branch)
}

#' Define a function that make a label
#'
#' @param l
#' @param labelselect
#'
#' @return label
build.text <- function(l,labelselect)
{

 # choose label type
 label = ""
 if (labelselect == "Default"){label = l["text.label"]}
 if (labelselect == "coralID"){label = l["id.coral"]}
 if (labelselect == "uniprot"){label = l["id.uniprot"]}
 if (labelselect == "ensembl"){label = l["id.ensembl"]}
 if (labelselect == "entrez"){label = l["id.entrez"]}
 if (labelselect == "HGNC"){label = l["id.HGNC"]}

 label = paste("<a xlink:href=\"http://www.uniprot.org/uniprot/",l["id.uniprot"],"\" target=\"_blank\">",
               "<text id=\"t_x5F_",l["id.coral"],"\" ",
               "x=\"",  l["text.x"],"\" ",
               "y=\"", trimws(l["text.y"]),"\" ",
               "font-weight=\"700\" ",
               " font-size=\"",l["text.size"],"px\" ",
               "fill=\"",l["text.col"],"\" ",
               "font-family=\"", l["text.font"], "\" ",
               "letter-spacing=","\".035\"",

               # # mouse over effects
               # " \nonmouseover=\"evt.target.setAttribute('font-size', '10');\"",
               # " \nonmouseout=\"evt.target.setAttribute('font-size','",origfontsize,"');\"",

               ">",label,"</text>","</a>",sep = "")
 return(label)
}


#' Define a function that makes a node
#'
#' @param l
#'
#' @return node
build.node <- function(l)
{
 if (l["node.col"] == "none")
 {
  return()
 }

 circle = paste("<circle id=\"n_x5F_",l["id.coral"],"\" ",
                "cx=\"",l["node.x"],
                "\" cy=\"",gsub(" ","",l["node.y"]),
                "\" r=\"",l["node.radius"],
                "\" opacity=\"",l["node.opacity"],
                "\" stroke=\"",l["node.strokecol"],
                "\" stroke-width=\"",l["node.strokewidth"],
                "\" fill=\"",l["node.col"],"\"/>",sep="")
 return(circle)
}

#' Define a function that writes an kinase tree svg file
#'
#' @param svginfo
#' @param destination
#' @param font
#' @param labelselect
#' @param groupcolor
#'
#' @return tree lines
#' @export
writekinasetree <- function(svginfo,destination,font,labelselect,groupcolor)
{
 outputlines = c()

 # add header
 outputlines = c(outputlines,svginfo$header)

 # add title
 outputlines = c(outputlines,"<g id=\"TITLE\">")
 outputlines = c(outputlines,paste("<text x=\"425\" y=\"10\" text-anchor=\"middle\" font-weight=\"700\" font-family=\"",font,"\"  font-size=\"15px\">",svginfo$title,"</text>",sep=""))
 outputlines = c(outputlines,"</g>")

 # add legend
 outputlines = c(outputlines,"<g id=\"LEGEND\">")
 outputlines = c(outputlines,svginfo$legend)
 outputlines = c(outputlines,"</g>")

 # reorder branches by branch order
 branchorderesDF = svginfo$dataframe[svginfo$dataframe$branchorder,]

 # add branches
 outputlines = c(outputlines,"<g id=\"BRANCHES\">")
 outputlines = c(outputlines,unlist(     apply(branchorderesDF,1, build.branch)       ))
 outputlines = c(outputlines,"</g>")

 # add circles
 outputlines = c(outputlines,"<g id=\"CIRCLES\">")

 # reorder by node order
 nodeorderesDF = svginfo$dataframe[svginfo$dataframe$nodeorder,]

 # reorder circles by size
 nodeorderesDF = nodeorderesDF[order(nodeorderesDF$node.radius,decreasing = TRUE),]
 nodeorderesDF = nodeorderesDF[order(nodeorderesDF$node.selected,decreasing = FALSE),]

 outputlines = c(outputlines,unlist(apply(nodeorderesDF,1, build.node )))
 outputlines = c(outputlines,"</g>")

 # add labels
 outputlines = c(outputlines,"<g id=\"LABELS\">")
 outputlines = c(outputlines,unlist(apply(svginfo$dataframe,1, build.text,labelselect=labelselect)))
 outputlines = c(outputlines,"</g>")

 # add tail
 outputlines = c(outputlines,"<g id=\"GROUPS\">")
 outputlines = c(outputlines,unlist(lapply(svginfo$groups, build.group.labels, font=font,groupcolor=groupcolor)))
 outputlines = c(outputlines,"</g>")
 outputlines = c(outputlines,"</svg>")

 Encoding(outputlines) <- "UTF-16"

 writeLines(outputlines,destination)
 # return(outputlines)
}

#' Make empty coral tree
#'
#' @return empty coral tree structure
#' @import readr
make_empty_tree <- function() {
 require(readr)
 orig_svginfo <- readRDS(system.file("extdata", "kintree.RDS", package = "CORALcli"))
 orig_svginfo$dataframe <- data.frame(read_tsv(system.file("extdata", "coral_dataframe.tsv", package = "CORALcli")))
 NAs = which(is.na(orig_svginfo$dataframe$kinase.subfamily))
 orig_svginfo$dataframe$kinase.subfamily[NAs] = ""

 # remove NAs from HGNCs
 NAs = which(is.na(orig_svginfo$dataframe$id.HGNC))
 orig_svginfo$dataframe$id.HGNC[NAs] = ""
 # add correct header
 # orig_svginfo$header = "<svg viewBox=\"50 -10 800 640\"  preserveAspectRatio=\"xMidYMid meet\"\n
 orig_svginfo$header = "<svg width=\"940\" height=\"940\"\n

xmlns=\"http://www.w3.org/2000/svg\"\n
xmlns:xlink=\"http://www.w3.org/1999/xlink\" >\n"

 # <defs>\n    <style type=\"text/css\">\n
 # @import url('https://fonts.googleapis.com/css?family=Roboto:700');\n
 # text {font-family: \"Roboto-Bold\";\n  }\n  </style>\n    </defs>"

 # initialize title
 orig_svginfo$title = ""

 # initialize legend
 orig_svginfo$legend = c()

 # add node opacity
 orig_svginfo$dataframe$node.opacity = 1

 # add node order
 orig_svginfo$dataframe$node.selected = -1

 # make svginfo (leaving the original intact)
 svginfo = orig_svginfo

 # assign node and branch orders
 svginfo$dataframe$nodeorder = 1:nrow(svginfo$dataframe)
 svginfo$dataframe$branchorder = 1:nrow(svginfo$dataframe)

 return(svginfo)
}

#' Compile coral tree
#'
#' @param df
#' @param min_col
#' @param max_col
#'
#' @return list of coral tree and legend
#' @import dplyr
#' @export
make_tree_data <- function(df, min_col, max_col) {
 require(dplyr)
 empty_tree <- make_empty_tree()
 tempdf <- empty_tree$dataframe
 tempdf$text.font <- 'Helvetica'
 legend <- c()
 yoffset <- 79.125

 kinaseData <- df
 # Threshold final score
 kinaseData <- kinaseData %>% filter(`Median Final score` > 1.2)
 kinaseNodeSize <- kinaseData %>% select(`Kinase Uniprot ID`, `Median Final score`) %>% as.data.frame(.)
 kinaseNodeColor <- kinaseData %>% select(`Kinase Uniprot ID`, `Median Kinase Statistic`) %>% as.data.frame(.)

 # ---------------- BRANCH & NODE COLOR ---------------- #
 recolordf <- kinaseNodeColor


 colnames(recolordf) = c("kinase","userinfo")

 recolordf <- convertID(tempdf, recolordf, inputtype = "uniprot")
 if (nrow(recolordf) > 0) {
  branchcolpalette <- colorRampPalette(c("#1B8ED1","#e5e5e5","#FA6958"))(11)
  bg.col <- "#e5e5e5"
 }

 newcolors_and_colormapping <- color.by.value(df = tempdf, recolordf = recolordf, colors = branchcolpalette, heatrange = c(min_col, max_col), bg.col = bg.col)
 tempdf$branch.col <- newcolors_and_colormapping[[1]]
 tempdf$branch.val <- newcolors_and_colormapping[[2]]
 tempdf$node.col <- newcolors_and_colormapping[[1]]
 tempdf$node.val <- newcolors_and_colormapping[[2]]

 tempdf$branchorder <- order(abs(tempdf$branch.val), decreasing = FALSE, na.last = FALSE)

 # add legend info
 lines_and_offset <- build.value.legend(yoffset = yoffset, minval = min_col, maxval = max_col, palette = branchcolpalette, elementtype = "Branch", fontfamily = "Helvetica", subtitle = "Kinase Statistic")
 lines <- lines_and_offset[[1]]
 yoffset <- lines_and_offset[[2]] + 14
 legend <- c(legend, lines)

 # ---------------- NODE SIZE ---------------- #
 resizedf <- kinaseNodeSize
 colnames(resizedf) = c("kinase","userinfo")
 resizedf <- convertID(tempdf, resizedf, inputtype = "uniprot")

 min_node_size <- 1
 max_med_final_score <- max(kinaseNodeSize$`Median Final score`)
 max_node_size <- ceiling(max_med_final_score / 0.5) * 0.5

 minvalforlegend <- min_node_size
 maxvalforlegend <- max_node_size

 if (nrow(resizedf) > 0) {
  radii_and_mapping <- resizes.by.value(
   df = tempdf, resizedf = resizedf, sizerange = c(3, 9),
   controlledrange = TRUE, minvalue = min_node_size, maxvalue = max_node_size, showall = "hide"
  )
 }

 tempdf$node.radius <- radii_and_mapping[[1]]
 tempdf$node.val.radius <- radii_and_mapping[[2]]

 lines_and_offset <- build.nodesize.legend(yoffset = yoffset, minval = minvalforlegend, maxval = maxvalforlegend, minsize = 3, maxsize = 9, fontfamily = "Helvetica", subtitle = "Kinase Score")
 lines <- lines_and_offset[[1]]
 yoffset <- lines_and_offset[[2]] + 14
 legend <- c(legend, lines)

 # ---------------- ADDITIONAL ---------------- #

 selkinases <- kinaseNodeColor$`Kinase Uniprot ID`
 selkinasescoral <- ""

 if (length(selkinases) > 0) {
  kinasestoconvert <- data.frame(kin1 = selkinases, kin2 = selkinases)
  selkinasesconverted <- convertID(tempdf, kinasestoconvert, inputtype = "uniprot")
  if (nrow(selkinasesconverted) > 0) {
   selkinasescoral <- selkinasesconverted[, 1]
  }
 }
 # set background color and font size
 tempdf$text.col <- "#D3D3D3"
 tempdf$text.size <- 0
 # set selected font color and size
 tempdf$text.col[which(tempdf$id.coral %in% selkinasescoral)] <- "#000000"
 tempdf$text.size[which(tempdf$id.coral %in% selkinasescoral)] <- 7

 tempdf$node.strokecol <- tempdf$node.col


 return(list(tempdf, legend))
}

#' Make coral tree SVG
#'
#' @param df
#' @param min_col
#' @param max_col
#' @param png
#'
#' @return tree
#' @import magick rsvg stringr
#' @export
plot_tree <- function(df, comparison, tree_dir, min_col, max_col, png=FALSE) {
 require(magick)
 require(rsvg)
 require(stringr)

 new_tree <- make_empty_tree()
 dfandlegend <- make_tree_data(df, min_col = min_col, max_col = max_col)
 new_tree$dataframe <- dfandlegend[[1]]
 new_tree$legend <- dfandlegend[[2]]

 tree_file <- paste0(comparison, "_CORAL.svg")
 svg_file <- paste0(tree_dir, "/",tree_file)

 if (!dir.exists(dirname(svg_file))) {
  dir.create(dirname(svg_file), showWarnings = F)
 }
 tree <- writekinasetree(new_tree, destination = svg_file, font = "Helvetica", labelselect = "Default", groupcolor = "#000000")
 # if (png == TRUE) {
 #  pngDPI <- 300
 #  img <- magick::image_read_svg(svg_file, width = 4000, height = 4000)
 #  magick::image_write(img, png_file, format = "png", flatten = FALSE, density = pngDPI)
 # }
}

