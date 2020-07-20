# Dependencies

library("wesanderson")
library("tibble")
library("dplyr")






wes_14colors = c(  wes_palette(name = "Darjeeling1"),
                  wes_palette(name = "GrandBudapest2"),
                  wes_palette(name = "FantasticFox1"))







#Cool Palette
somecol= c(




  "#0000FF", #Blue
  "#000066", #Darkblue
  "#000033", #SuperDarkBlue


  "#FFCCCC", #SkinPink
  "#FF3399", #Pink
  "#FF00FF", #VibrantPink
  "#FF33CC", #DarkPink




  "#FFCC00", #Yellow
  "#FFFF99", #SuperLightYellow
  "#FFFF00", #PaleYellow
  "#FF9900", #LightOrange
  "#FF6600", #OrangeYellow
  "#FF3300", #IntenseOrange

  "#0099FF", #Cyan
  "#33CCFF", #lightCyan
  "#6699FF", #FadedCyan
  "#3366CC", #Darkcyan



  "#330000", #darkBrown
  "#993300", #Brown
  "#663300", #LightBrown
  "#996633", #Palebrown


  "#CC0000", #Darkred
  "#FF0000", #Red
  "#FF3333", #Lightred
  "#FF6666", #PaleRed
  "#CC0033", #Coral




  "#003300", #VeryDarkgreen
  "#006600", #DarkGreen
  "#336633", #SmokedGreen
  "#669966", #LightSmokedgreen
  "#00FF00", #Greem
  "#66FF66", #Lightgreen
  "#99FF33", #Lightyellowgreen
  "#CCFF00", #yeallowgreen


  "#003366", #Darkpetrolgreen
  "#006699", #Petrolgreen
  "#00CCCC", #Lightpetrolgreen
  "#33FFCC", #SuperlihtpetrolGreen

  "#9900FF", #LightPurple
  "#6600FF", #Purple
  "#9966FF", #FadedPurple
  "#3300CC", #Violet
  "#330099", #DarkViolet

  "#000000", #Black
  "#333333", #Grey
  "#CCCCCC" #LightGrey


)



#LEGACY PALETTE
 c( "#0988e3",
           "#d109e3",
           "#1be309",
           "#e36409",
           "#093fe3",
           "#e3091b",
           "#e3d109",
           "#767676",
           "#000000",
           "#e30988",
           "#88e309",
           "#033050",
           "#8bccfa",
           "#767676",
           "#f7b80a",
           "#0af7e0",
           "#0a21f7",
           "#808080",
           "#21f70a",
           "#e00af7",
           "#0a98f7",
           "#0af769",
           "#690af7",
           "#652a03",
           "#fcc49e",
           "#808080")


# my_colors = colorRampPalette(brewer.pal(9,"Set1"))(57)
#
# my_colors = brewer.pal(9, "Paired")
#
# my_colors = cbind(my_colors, c())
#
# my_colors = rainbow(60)


# USE THIS
my_colors = colorRampPalette(somecol)(60)



# my_colors = colorRampPalette(cbind(wes_palette(name="Darjeeling"),
#                                    wes_palette(name="Moonrise3"),
#                                    wes_palette(name="Cavalcanti")))(60)
#

# RANDOMIZATION
my_colors = sample(my_colors)



gino_rain = sample(rainbow(20))



# FUNCTION RANDOMIZING COLORS
randomize_my_colors = function(){

  my_colors = sample(my_colors)
}


##--------------- POLLO_20 :: Color Paolette -----------------------




pollo_20 <- c(
  "#FF0000"
  ,"#00A08A"
  ,"#F2AD00"
  ,"#F98400"
  ,"#5BBCD6"
  ,"#5bbf13" # green dettofatto
  ,"#ff03f2" # pink
  ,"#6b43e0"  # everybody violet
  ,"#0374ff" # intense blue
  ,"#4d1919" #coral bella storia
  ,"#690038" # shitty bordeaux
  ,"#03ff79" # shy green
  ,"#695006"  #real brown
  ,"#696506" #what is this?
  ,"#cf6b00" # candy brown
  ,"#007c82" # petroleum
  ,"#00a61c" # ireland
  ,"#000000" # black

  ####3
  )



# The secret:
# YOUR color-filled table

pollos_colors <- tribble(

  ~color_code, ~description     ,~q1_priority  ,~q6_cluster  ,~q6_priority, ~q5_cluster  ,~q5_priority ,~q4_cluster  ,~q4_priority  ,~q2_cluster  ,~q2_priority  ,~q3_cluster  ,~q3_priority

  ,"#FF0000"   ,"Red vero"            ,1            ,"Q1"        ,1            ,"Q1"        ,1             ,"Q1"        ,1             ,"Q1_Warm"         ,1      ,"Q1_Warm"        ,1
  ,"#00A08A"   ,"Green Tiffany"       ,2            ,"Q3"        ,1            ,"Q3"        ,1             ,"Q3"        ,1             ,"Q2_Cold"         ,1      ,"Q2_Cold"        ,1
  ,"#F2AD00"   ,"Yellow DettoFatto"   ,3            ,"Q4"        ,1            ,"Q4"        ,1             ,"Q4"        ,1             ,"Q1_Warm"         ,3      ,"Q1_Warm"        ,3
  ,"#F98400"   ,"Orange Rugby Varese" ,4            ,"Q4"        ,2            ,"Q4"        ,2             ,"Q4"        ,2             ,"Q1_Warm"         ,2      ,"Q1_Warm"        ,2
  ,"#5BBCD6"   ,"Light Blue Cielo"    ,5            ,"Q2"        ,1            ,"Q2"        ,1             ,"Q2"        ,1             ,"Q2_Cold"         ,2      ,"Q2_Cold"        ,2
  ,"#5bbf13"   ,"Green DettoFatto"    ,6            ,"Q3"        ,2            ,"Q3"        ,2             ,"Q3"        ,2             ,"Q2_Cold"         ,3      ,"Q2_Cold"        ,3
  ,"#ff03f2"   ,"Pink"                ,7            ,"Q1"        ,2            ,"Q1"        ,2             ,"Q1"        ,2             ,"Q1_Warm"         ,4      ,"Q1_Warm"        ,4
  ,"#6b43e0"   ,"Violet EveryBody "   ,8            ,"Q2"        ,2            ,"Q2"        ,2             ,"Q2"        ,2             ,"Q2_Cold"         ,5      ,"Q2_Cold"        ,5
  ,"#0374ff"   ,"Intense blue"        ,9            ,"Q2"        ,3            ,"Q2"        ,3             ,"Q2"        ,3             ,"Q2_Cold"         ,4      ,"Q2_Cold"        ,4
  ,"#d93830"   ,"Coral bella storia"  ,10           ,"Q1"        ,3            ,"Q1"        ,3             ,"Q1"        ,3             ,"Q1_Warm"         ,6      ,"Q1_Warm"        ,5
  ,"#800000"   ,"shitty bordeaux"     ,11           ,"Q1"        ,4            ,"Q1"        ,4             ,"Q1"        ,4             ,"Q1_Warm"         ,5      ,"Q3_Neutral"     ,1
  ,"#03ff79"   ,"shy green"           ,12           ,"Q6"        ,2            ,"Q5"        ,4             ,"Q3"        ,4             ,"Q2_Cold"         ,6      ,"Q2_Cold"        ,8
  ,"#695006"   ,"real brown"          ,13           ,"Q5"        ,1            ,"Q5"        ,1             ,"Q4"        ,4             ,"Q1_Warm"         ,7      ,"Q3_Neutral"     ,2
  ,"#696506"   ,"what is this?"       ,14           ,"Q5"        ,2            ,"Q5"        ,2             ,"Q3"        ,6             ,"Q1_Warm"         ,8      ,"Q3_Neutral"     ,3
  ,"#cf6b00"   ,"candy brown"         ,15           ,"Q6"        ,1            ,"Q5"        ,3             ,"Q4"        ,3             ,"Q1_Warm"         ,9      ,"Q3_Neutral"     ,4
  ,"#007c82"   ,"petroleum"           ,16           ,"Q3"        ,3            ,"Q3"        ,3             ,"Q3"        ,5             ,"Q2_Cold"         ,7      ,"Q2_Cold"        ,7
  ,"#00b003"   ,"ireland"             ,17           ,"Q3"        ,4            ,"Q3"        ,4             ,"Q3"        ,3             ,"Q2_Cold"         ,8      ,"Q2_Cold"        ,6
  ,"#000000"   ,"black"               ,18           ,"Q4"        ,3            ,"Q4"        ,3             ,"Q2"        ,5             ,"Q1_Warm"         ,9      ,"Q3_Neutral"     ,5
  ,"#7fffff"   ,"azzurrissimo"        ,19           ,"Q2"        ,4            ,"Q2"        ,4             ,"Q2"        ,4             ,"Q2_Cold"         ,9      ,"Q2_Cold"        ,9

  )

#View(pollos_colors)



#____ FUNCTION ____
# To retrieve correct colors among# selected ones
#
i_want_pollos_colors <- function(     howmany_colors      = NULL
                                    , howmany_categories  = NULL
                                    , colors_per_category = NULL
){

  #++++++++++++++++++++++++++++++
  # TEST
  # howmany_colors      = 17
  # howmany_categories  = 4
  # colors_per_category = 4
  #++++++++++++++++++++++++++++++


  # set lenght of output dataset
  if(is.null(howmany_colors)) { ncolors = length(pollos_colors[[1]])
  }else{ ncolors = howmany_colors }



  # Check input consistency
  if(!is.null(howmany_categories)){


    if(is.null(colors_per_category)){
      stop("colors_per_category must contain a valid number!")
    }

  }

  #TODO

  # Check colors availability
  if(ncolors > length(pollos_colors[[1]] )){
    stop(paste0("Too many colors! Only ",length(pollos_colors[[1]] )," available!"))
  }

  #TODO


  ## Colors builder ...........



  if(is.null(howmany_categories)){
    return(pollos_colors[order(pollos_colors$q1_priority),]$color_code[1:ncolors])
  }

  ##  One category

  if(howmany_categories == 1){
    return(pollos_colors[order(pollos_colors$q1_priority),]$color_code[1:ncolors])
  }


  ## More categories

  # subset of colors needed from category
  pollos_subset = pollos_colors %>%
           # from "how_many_Categories" selects column,
           # from "colors_per_category" selects number of results
    filter( !!as.symbol(paste0("q",howmany_categories,"_priority")) %in% c(1:colors_per_category)) %>%
    as.data.frame()


  # order by cluster_names, then priority
  ordered_pollo_subset = pollos_subset[order(pollos_subset[[paste0("q",howmany_categories,"_cluster") ]],
                                             pollos_subset[[paste0("q",howmany_categories,"_priority")]]  ),]


  # return colors
  return(ordered_pollo_subset$color_code)


}





##---------------: REGIONAL COLOR PALETTE :-----------------------



colorize_regions <- function(region_id = "ed57"){


  #ED57 color palette
  if(region_id == "ed57"){

    c(
      "arg"     =  "#000000",
      "aus"     =  "#48D1CC",
      "aut"     =  "#ae8000",
      "bel"     =  "#800000",
      "bgr"     =  "#003366",
      "blt"     =  "#bf4040",
      "bra"     =  "#ffd633", #Yellow           # <
      "can"     =  "#6600cc", #Violet           # <
      "chl"     =  "#ffece6",
      "chn"     =  "#ff531a", #Red              # ==
      "cor"     =  "#adebad",
      "cro"     =  "#808080",
      "dnk"     =  "#ff9933",
      "egy"     =  "#0044cc", #Purple           # <
      "esp"     =  "#ffd6cc",
      "fin"     =  "#00cccc",
      "fra"     =  "#cc0000",
      "gbr"     =  "#FFFFFF",
      "golf57"  =  "#33d6ff", #Light blue      # ==
      "grc"     =  "#00ffcc",
      "hun"     =  "#9999ff",
      "idn"     =  "#996633",
      "irl"     =  "#ff4dff",
      "ita"     =  "#FFFF00",
      "jpn"     =  "#006600", #Petrolgreen      # <
      "meme"    =  "#b32d00",
      "mex"     =  "#ccff33", #Bluette          # <
      "mys"     =  "#145252",
      "nde"     =  "#00d900", #Green            # ==
      "nld"     =  "#C309BD",
      "noan"    =  "#ffff99",
      "noap"    =  "#ecf2f9",
      "nor"     =  "#ff3399",
      "oeu"     =  "#ffb3ff",
      "osea"    =  "#008fb3", #Pink             # <
      "pol"     =  "#d6f5d6",
      "prt"     =  "#003300",
      "rcam"    =  "#4d1919", #Coral            # <
      "rcz"     =  "#00FFFF",
      "rfa"     =  "#DEB887",
      "ris"     =  "#000080", #Darkblue         # <
      "rjan57"  =  "#bf00ff",
      "rom"     =  "#FF00FF",
      "rsaf"    =  "#ff8000", #Orange           # ==
      "rsam"    =  "#0000FF", #DarkPink         # <
      "rsas"    =  "#CCD6DD", #darkBrown        # <
      "rsl"     =  "#00FF00",
      "rus"     =  "#66757F", #Chocolate        # ==
      "slo"     =  "#ff3091",
      "sui"     =  "#61A62F",
      "swe"     =  "#CB1942",
      "tha"     =  "#EFFF14",
      "tur"     =  "#4B0082", #DarkViolet       # <
      "ukr"     =  "#c198ff", #Lightviolet      # <
      "usa"     =  "#FFCC00", #Gold             # ==
      "vnm"     =  "#3377ff", #LightGreen       # <
      "zaf"     =  "#b3ccff"  #DarkSlateGray
    )



  }


}

