#import libraries
library(shiny) ; library(tidyverse) ; library(shinyjs) ; library(scales) ; library(DT)

#load example dataset for user
data <- read.csv("example_data.csv")
data <- data[,-1]

#basic wrangling
data$week <- as.factor(data$week)
data$week_start_date <- as.POSIXct(data$week_start_date)
str(data)

#generate data titles for graphs and use (New Names)
nn_gen <- function(df){
        
        #get a list of the names of the dataframe and reformat
        nn <- lapply(
                sapply(strsplit(names(df), split = "_"), Hmisc::capitalize), 
                paste, collapse = " ")
        
        #assign names and original names to temp variable
        vals <- names(df) ; names(vals) <- nn
        
        #assign names of the list as reference values (like python dict)
        vals <- append(vals, nn) ; names(vals)[12:22] <- vals[1:11]
        
        return(vals)
        
        
}

#create dict
name_dict <- nn_gen(data)

#Set theme for graphs
col_pal <- RColorBrewer::brewer.pal("Set3", n = 11)
names(col_pal) <- names(data)

#generate a theme for the graph
def_theme <- function() {

        #define the background color
        offwhite <- "#FBFBFB"

        #define the font used
        fontText <- 'Avenir Next Condensed'

        #use minimal theme as base
        theme_minimal(base_size=9, base_family = fontText ) +
                theme(plot.title=element_text(size = 14, vjust = 1.25, hjust = 0.5)) + #plot title size & position
                theme(axis.text.x=element_text(size = 11)) + #x axis text size & position
                theme(axis.text.y=element_text(size = 11)) + #y axis text size & position
                theme(axis.title.x=element_text(size = 12)) +  #x axis title size & position
                theme(axis.title.y=element_text(size = 12, vjust = 1.25)) + #y axis title size & position
                theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + #change the plot margins
                theme(plot.subtitle = element_text(size = 12, vjust = 1.25)) + #subtitle options
                theme(axis.line = element_blank(),  #blank out axis line
                      plot.background = element_rect(fill = offwhite, color = offwhite), #plot background as afore mentioned color
                      panel.background = element_rect(fill = offwhite, color = offwhite), #panel background as afore mentioned color
                      panel.border = element_blank() #panel border blanked out
                )
}

#set theme
theme_set(def_theme())

#plotting weeks function
plot_weeks <- function(df, conv, rate, mediums, pal, weekRange, name_dict = name_dict, ylim1, ylim2, pTitle = "", bestfit,
                       line = TRUE, points = TRUE, bfCol){

        #if applicable, change null inputs to NA for ease of use
        if(is.null(ylim1)){

                ylim1 <- NA

        }

        if(is.null(ylim2)){

                ylim2 <- NA

                }

        #if NAs exist, set ylim flag to NA else set ylims as variable
        if(is.na(ylim1) || is.na(ylim2)){

                ylim <- NA

        } else {

                ylim <- c(ylim2, ylim1)

        }

        #create weekrange variable (x:y) out of args
        weekRange <- c(weekRange[1]:weekRange[2])

        #ensure correct data type
        df$week <- as.integer(as.character(df$week))

        #shave off the unwanted mediums and weeks
        df <- df[df$channel %in% mediums & df$week %in% weekRange, ]

        #create rate variable from the wanted variables
        df$rate <- df[,conv] / df [,rate]

        #make variables for titles
        X <- "Week" ; aY <- name_dict[conv]

        #unique titles for revenue (grammar)
        if(rate == "revenue"){

                bY <- paste(name_dict[conv], "per Unit of", name_dict[rate])

        } else {

                bY <- paste(name_dict[conv], "per",
                            substring(name_dict[rate],1, nchar(name_dict[rate]) - 1))
        }

        #plot title as input else use the default
        if(!pTitle == ""){

                PT <- as.character(pTitle)

        } else{

                PT <- paste(aY, "by", X)

        }

        #non aggregated graph
        a <- ggplot(df[!df$channel == "Total",]) +
                geom_bar(aes_string(x = "week", y = conv), stat = "identity", fill = pal[conv], alpha = 0.85) +
                geom_hline(yintercept = 0, color = "black", size = 1) +
                scale_x_continuous(breaks = weekRange) +
                scale_y_continuous(label = comma) +
                labs(x = X, y = aY, title = PT)

        #plot title as input else use the default
        if(!pTitle == ""){

                PT <- as.character(pTitle)

        } else{

                PT <- paste(bY, "for Each Channel", "by", X)

        }

        #sort out bestfit line
        #smooth method
        m <- ifelse(bestfit == "c", "loess",
                    ifelse(bestfit == "l", "lm", NA))

        if(is.na(m)){

                m <- NULL

        } else {

                m <- geom_smooth(se = FALSE, method = m, linetype = "twodash", color = bfCol)

        }

        #rate graph (line)
        b <- ggplot(df, aes_string(x = "week", y = "rate", color = "channel")) +
                geom_hline(yintercept = 0, color = "black", size = 1) +
                scale_x_continuous(breaks = weekRange) +
                scale_y_continuous(label = comma) +
                labs(x = X, y = bY, title = PT) +
                guides(color = guide_legend(title = "Channel"))

        if(points){

                b <- b + geom_point()

        }

        if(line){

                b <- b + geom_line()

        }

        #add smooth
        b <- b + m

        if(!pTitle == ""){

                PT <- as.character(pTitle)

        } else{

                PT <- paste(aY, "by", X, "Segmented by Channel")

        }

        #segment the variables by channel
        c <- ggplot(df[!df$channel == "Total",]) +
                geom_bar(aes_string(x = "week", y = conv, fill = "channel"), stat = "identity", alpha = 0.85, position = "dodge") +
                geom_hline(yintercept = 0, color = "black", size = 1) +
                scale_x_continuous(breaks = weekRange) +
                scale_y_continuous(label = comma) +
                labs(x = X, y = aY, title = PT) +
                guides(fill = guide_legend(title = "Channel"))

        #apply ylims if applicable
        if(is.na(ylim)[1] == FALSE){

                a <- a + coord_cartesian(ylim = ylim)
                b <- b + coord_cartesian(ylim = ylim)
                c <- c + coord_cartesian(ylim = ylim)
        }

        return(list(a, b, c))


}

#plotting the rest of the variables
plot_others <- function(col, df, conv, rate, mediums, pal, weekRange, name_dict = name_dict, ylim1, ylim2, pTitle = "",
                        xlim1, xlim2, bestfit, line = TRUE, points = TRUE, bfCol){

        #if applicable, change null inputs to NA for ease of use
        if(is.null(ylim1)){

                ylim1 <- NA

        }

        if(is.null(ylim2)){

                ylim2 <- NA

                }

        #if NAs exist, set ylim flag to NA else set ylims as variable
        if(is.na(ylim1) || is.na(ylim2)){
                ylim <- NA
        } else{
                ylim <- c(ylim2, ylim1)
                }

        #if applicable, change null inputs to NA for ease of use
        if(is.null(xlim1)){

                xlim1 <- NA

                }
        if(is.null(xlim2)){

                xlim2 <- NA

                }

        #if NAs exist, set xlim flag to NA else set xlims as variable
        if(is.na(xlim1) || is.na(xlim2)){

                xlim <- NA

        } else{
                xlim <- c(xlim2, xlim1)
        }

        #create weekrange variable (x:y) out of args
        weekRange <- c(weekRange[1]:weekRange[2])

        #ensure correct data type
        df <- df[df$channel %in% mediums & df$week %in% weekRange, ]

        #get new totals for every week, aggregate variables by week and where channel is not total for each variable
        for(i in weekRange){

                df[df$channel == "Total" & df$week == i, unique(c(conv, rate, col))] <- unique(c(sum(df[!(df$channel == "Total") & df$week == i, conv]),
                                                                                                 sum(df[!(df$channel == "Total") & df$week == i, rate]),
                                                                                                 sum(df[!(df$channel == "Total") & df$week == i, col])
                ))

        }

        #create rate variable
        df$rate <- df[,conv] / df [,rate]

        #make variables for titles
        X <- name_dict[[col]] ; aY <- name_dict[conv]

        #unique titles for revenue (grammar)
        if(rate == "revenue"){

                bY <- paste(name_dict[conv], "per Unit of", name_dict[rate])

        } else {

                bY <- paste(name_dict[conv], "per",
                            substring(name_dict[rate],1, nchar(name_dict[rate]) - 1))
        }

        #plot title as input else use the default
        if(!pTitle == ""){

                PT <- as.character(pTitle)

        } else{

                PT <- paste(aY, "by", X, ", Segmented by Week")

        }

        #sort out bestfit line
        #smooth method
        m <- ifelse(bestfit == "c", "loess",
                    ifelse(bestfit == "l", "lm", NA))

        if(is.na(m)){

                m <- NULL

        } else {

                m <- geom_smooth(se = FALSE, method = m, linetype = "twodash", color = bfCol)
        }


        #Week on week changes
        a <- ggplot(df[df$channel == "Total",], aes_string(x = conv, y = name_dict[[X]], color = "week")) +
                guides(color = guide_legend(title = "Week")) +
                scale_y_continuous(label = comma) +
                scale_x_continuous(label = comma) +
                labs(x = X, y = aY, title = paste(aY, "by", X, ", Segmented by Week")) +
                geom_hline(yintercept = 0, color = "black", size = 1)

        if(!pTitle == ""){

                PT <- as.character(pTitle)

        } else{

                PT <- paste(bY, "by", X, ", Segmented by Channel")

        }

        #add points
        if(points){

                a <- a + geom_point(shape = 18, size = 3)

        }

        #add lines
        if(line){

                a <- a + geom_line(aes_string(x = conv, y = name_dict[[X]], group = 1), color = pal[col])

        }

        #finalise graph
        a <- a + m

        # x over y by channel
        b <- ggplot(df[!df$channel == "Total",], aes_string(x = "rate", y = name_dict[[X]])) +
                guides(color = guide_legend(title = "Week"), shape = guide_legend(title = "Channel")) +
                scale_y_continuous(label = comma) +
                scale_x_continuous(label = comma) +
                labs(x = X, y = bY, title = paste(bY, "by", X, ", Segmented by Channel")) +
                geom_hline(yintercept = 0, color = "black", size = 1) +
                scale_shape_manual(values = c(16, 17, 18, 15))

        #add points
        if(points){

                b <- b + geom_point(aes_string(x = "rate", y = name_dict[[X]], color = "week", shape = "channel"), size = 3)

        }

        #add lines
        if(line){

                b <- b + geom_line(aes_string(x = "rate", y = name_dict[[X]], group = 1), color = pal[col])

        }

        #finalise graph
        b <- b + m

        #ylims if applicable
        if(is.na(ylim)[1] == FALSE){

                a <- a + coord_cartesian(ylim = ylim)
                b <- b + coord_cartesian(ylim = ylim)
        }

        #apply xlims if applicable
        if(is.na(xlim)[1] == FALSE){

                a <- a + coord_cartesian(xlim = xlim)
                b <- b + coord_cartesian(xlim = xlim)
        }

        return(list(a,b))

}

# #week on week dataframe gen for general viewing
wow_df <- function(df, conv, rate, mediums, weekRange, name_dict, values = FALSE){

        #create weekrange variable
        weekRange <- c(weekRange[1]:weekRange[2])

        #shave off unwanted channels and weeks
        df <- df[df$channel %in% mediums & df$week %in% weekRange, ]

        #new rate variable
        df$per <- df[,conv] / df [,rate]

        #make dfs for rate, conv and per
        df1 <- df[,c("week", "channel", conv)] %>% spread(week, conv)
        df2 <- df[,c("week", "channel", rate)] %>% spread(week, rate)
        df3 <- df[,c("week", "channel", "per")] %>% spread(week, per)


        #titles for variable
        if(rate == "revenue"){

                per <- paste(name_dict[conv], "per Unit of", name_dict[rate])

        } else {

                per <- paste(name_dict[conv], "per",
                             substring(name_dict[rate],1, nchar(name_dict[rate]) - 1))
        }

        #group together the dfs
        dfS <- list(df1, df2, df3)

        #list variables
        v <- c(conv, rate, per)

        #empty list for the new dfs
        fin <- list()

        #iterate through the tables and get changes
        for(i in 1:3){

                #temp table
                temp <- data.frame(Channel = mediums, Variable = rep(Hmisc::capitalize(v[i])))

                #iterate over the weeks and get values
                for(n in weekRange[-length(weekRange)]){

                        #get per variable
                        x <- dfS[[i]][,n]

                        #get variable of interest
                        y <- dfS[[i]][,n+1]

                        #change ( - 100 to get % change not just proportion of y regarding x) OR difference in values depending on flag

                        if(values){

                                z <- round(y - x, ifelse(i == 3, 4, 2))

                        } else {

                                z <- round(((y / x) * 100) - 100, 2)
                        }

                        unit <- ifelse(values, "value", "%")

                        #get title
                        t <- paste("weeks", as.character(n), "to", as.character(n+1), unit, "change", sep = "_")

                        #get values for temp
                        temp[,t] <- z

                }

                #get title
                t2 <- paste("mean", unit, "change", v[i], sep = "_")

                #get mean change for each row
                temp[,t2] <- round(apply(temp[-c(1,2)], 1, mean), ifelse(i == 3 && values, 4, 2))

                #get top variable for change calculations
                last <- dfS[[i]][weekRange[length(weekRange)]]

                #get bottom variable for change calculations
                first <- dfS[[i]][weekRange[1]]

                if(values){

                        chg <- round(last - first, ifelse(i == 3 && values, 4, 0))

                } else {

                        chg <- round(((last / first) * 100) - 100, 2)
                }

                #get the new title
                t2 <- paste("overall", v[i], unit, "change", sep = "_")

                #add in the change
                temp[,t2] <- chg

                #get clean names
                nms <- lapply(sapply(strsplit(names(temp), split = "_"), Hmisc::capitalize),
                              paste, collapse = " ")

                for(b in 1:length(nms)){

                        #apply names to the table
                        names(temp)[b] <- nms[[b]]

                        }

                #finalise tables
                fin <- append(fin, list(temp))

        }

        return(fin)


}

# #get data on nearest point clicked on the graph
others_get_data <- function(col, df, conv, rate, mediums, weekRange, name_dict, click_input, outPut){

        #conditions for starting the function
        if (is.null(click_input)){ return() }

        else{
                #get week range from the inputs
                weekRange <- c(weekRange[1]:weekRange[2])
                print(weekRange)
                #shave off unwanted channels and weeks
                df <- df[df$channel %in% mediums & df$week %in% weekRange, ]

                print(head(df))
                #get new totals for every week, aggregate variables by week and where channel is not total for each variable
                for(i in weekRange){

                        df[df$channel == "Total" & df$week == i, unique(c(conv, rate, col))] <- unique(c(sum(df[!(df$channel == "Total") & df$week == i, conv]),
                                                                                                         sum(df[!(df$channel == "Total") & df$week == i, rate]),
                                                                                                         sum(df[!(df$channel == "Total") & df$week == i, col])
                        ))

                }

                #calculate the rates
                df$rate <- df[, conv] / df [, rate]

                #generate names for plots from the variable
                if(rate == "revenue"){

                        bY <- paste(name_dict[conv], "per Unit of", name_dict[rate])

                } else {

                        bY <- paste(name_dict[conv], "per",
                                    substring(name_dict[rate],1, nchar(name_dict[rate]) - 1))
                }

                # depending on the graph, receive data on associated table (value vs rate stat)
                if(outPut == 1){

                        #get rows nearest to the clicked point and get relevant cols
                        fin <- nearPoints(df, click_input)[,c("week", conv, col)]

                        #tidy the name
                        names(fin) <- Hmisc::capitalize(names(fin))

                        #clean values
                        fin[2] <- round(fin[2],2) ; fin[3] <- round(fin[3], 2)

                        return(fin)

                }

                else if(outPut == 2){

                        fin <- nearPoints(df, click_input)[,c("week", "channel", "rate", col)]
                        names(fin) <- c("Week", "Channel", bY, "Revenue")

                        fin[4] <- round(fin[4], 2) ; fin[3] <- round(fin[3], 4)

                        return(fin)

                }
                else{
                        print("Input Invalid.")
                }
        }

}

# #get data on nearest point clicked on the graph - weeks tab
get_week_data <- function(df, weekRange, click_input, conv, rate, name_dict, mediums, outPut){

        #if no data is requested, return nothing
        if (is.null(click_input)){

                return()

        } else {

                #make input values usable
                weekRange <- c(weekRange[1]:weekRange[2])

                #ensure data types
                df$week <- as.integer(as.character(df$week))

                #shave off unused channels and weeks
                df <- df[df$channel %in% mediums & df$week %in% weekRange, ]

                #calculate rate value
                df$rate <- df[,conv] / df[,rate]
                X <- "Week"

                #compute the name
                if(rate == "revenue"){

                        bY <- paste(name_dict[conv], "per Unit of", name_dict[conv])

                } else {

                        bY <- paste(name_dict[conv], "per",
                                    substring(name_dict[rate],1, nchar(name_dict[rate]) - 1))
                }

                #get the data for the clicked value
                x <- round(as.numeric(click_input$x))

                #output different stats depending on output

                if(outPut == 1) { #value returns

                        #finalise the value
                        fin <- cbind(df[df$week == x,c(1,3)],
                                     round(df[df$week == x, conv], 2))

                        #assign the name
                        names(fin) <- c(X, "Channel", Hmisc::capitalize(conv))

                } else { #rate return

                        #finalise the value
                        fin <- cbind(df[df$week == x,c(1,3)],
                                     round(df$rate[df$week == x], 4))

                        #assign the name
                        names(fin) <- c(X, "Channel", bY)
                }

                #return
                return(fin)

        }

}

# # K <- read.csv("test_data.csv") ; identical(K, data)
# df <- data ; conv <- "orders" ; rate <- "page_views" ; mediums <- c("Direct", "Email", "Natural Search", "Paid Search", "Total") ; pal <- col_pal ; weekRange <- c(2,5)
# ylim1 <- NA  ; ylim2 <- NA ; click_input <- NA ; outPut <- 1  ; col <- "errors" ; pTitle = "" ; xlim1 <- NA ; xlim2 <- NA ; bestfit = NULL; points = T ; line = T ; values = T ; bfCol = "#7851a9"
