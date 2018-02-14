
## the pieces of code below assume that
# you have run a syndormic surveillance system, and results are already available
# results for different data sources are already plotted in individual html pages,
            #something like source1.htlm; source2.html, etc
#all html pages are stored in the same folder



#####################################################################

myHTMLfolder <- "C:/...."  #enter here the address to the folder where all html pages are saved

#the codes below will (re)create, every time it's run, pages called "index.html", "main.html" and "nav.html".
#Save all your htmls of results in the folder given above
#run the code below
#to see your results, open always      "index.html"

setwd(myHTMLfolder)

    html <- file("index.html", "w+")

    cat("<html>\n", file=html)
    cat("<head>\n", file=html)
    cat(sprintf("<title>%s</title>\n", "TITLE"), file=html)    #give a short title to your system
    cat("</head>\n", file=html)

    cat("<frameset cols=\"150px,*\">\n", file=html)
    cat("<frame noresize=\"noresize\" src=\"nav.html\" name=\"nav\"/>\n", file=html)
    cat("<frame noresize=\"noresize\" src=\"main.html\" name=\"main\"/>\n", file=html)
    cat("</frameset>\n", file=html)

    cat("</html>\n", file=html)

    close(html)



    html <- file("main.html", "w+")

    cat("<html>\n", file=html)
    cat("<head>\n", file=html)
    cat(sprintf("<title>%s</title>\n", "TITLE main page"), file=html)      #give a short title to your main page here,
    cat("</head>\n", file=html)

    cat("<body>\n", file=html)

    cat(sprintf('<h1 align="center">%s</h1>\n', "Syndromic surveillance results"), file=html)    #customize to your liking
    cat(sprintf('<h1 align="center">%s</h1>\n', Sys.Date()), file=html)                          #customize to your liking (here set to show current date)
    cat(sprintf('<h2 align="center">%s</h2>\n', "Select a group on the navigation menu to the left to see results"), file=html)     #customize to your liking or remove all 3 lines
    
    cat("<TABLE border=\"0\" align=\"center\">\n", file=html)
cat("<tr>\n", file=html)
cat("<td>System outputs are based on data up to the end of the PREVIOUS day.</td>\n", file=html)      #customize to your liking or remove all 3 lines
cat("</tr>\n", file=html)

cat("<tr>\n", file=html)
cat("<td>Number of events per day correspond to the number of laboratory submissions, classified into syndromic groups by a computer system. </td>\n", file=html)   #customize to your liking or remove all 3 lines
cat("</tr>\n", file=html)

cat("<tr>\n", file=html)
cat("<td> They reflect the number of ANIMALS tested per day for CATS and DOGS, and the number of HERDS otherwise</td>\n", file=html)   #customize to your liking or remove all 3 lines
cat("</tr>\n", file=html)


cat("<tr>\n", file=html)
cat("<td>For questions contact Fernanda Dorea (fernanda.dorea@sva.se)</td>\n", file=html)              #customize to your liking or remove all 3 lines
cat("</tr>\n", file=html)


cat("</table>  \n", file=html)
  cat("</p>\n", file=html)

    
    
    cat("<hr/>\n", file=html)

#    cat("<p align=\"center\">\n", file=html)
#    cat("<img src=\"summary.png\"/>\n", file=html)
#    cat("</p>\n", file=html)
#
    cat("</body>\n", file=html)

    cat("</html>\n", file=html)

    close(html)





 html <- file('nav.html', "w+")

 cat("<html>\n", file=html)
  cat("<head>\n", file=html)
  cat(sprintf("<title>%s</title>\n", "TITLE menu"), file=html)       #customize to your liking
  cat("</head>\n", file=html)

  cat("<body>\n", file=html)

  # Create navigation.
  cat("<table border=0>\n", file=html)

  cat("<tr>", file=html)
  cat("<td colspan=3><a href=\"main.html\" target=\"main\">Main page</a></td>\n", file=html)       #customize to your liking or remove all 3 lines
  cat("</tr>\n", file=html)
  
cat("<tr>", file=html)
cat("<td colspan=3><a href=\"help.pdf\" target=\"main\">HELP</a></td>\n", file=html)              #customize to your liking or remove all 3 lines
cat("</tr>\n", file=html)


  cat("<tr><td colspan=3>&nbsp;</td></tr>\n", file=html)

  for(r in 1:length(results)) {                 #this shows the pages list, with he green/red button
                                                # to be able to do this, you need to have generated, in your codes, prior to running this piece:
                                                # an object, which here a called "page.names", which lists the names of the pages you have to display,
                                                          #for example, if you have pages "source1.html", "source2.html", this would be created as
                                                          #page.names <- c("source1", "source2")      - yes, without the html
                                                # an object, which here a called "display.names", which lists how you want the page name to be shown in the menu
                                                          #for example,         c("area1", "area2")
                                                #an object called "results", which has the same length as the number of pages you have to display,
                                                         #and whhich as a 0 when now alarms were present, and a 1 when alarms were present
    cat("<tr>", file=html)

    cat("<td>&nbsp;</td>", file=html)

    if(status[r]==0) {
      cat("<td bgcolor='springgreen'>", file=html)
    } else {
      cat("<td bgcolor='red'>", file=html)
    }

    cat("&nbsp;&nbsp;&nbsp;&nbsp;</td>", file=html)

    cat(sprintf("<td><a href=\"%s.html\" target=\"main\">%s</a></td>", page.names[r], display.names[r]), file=html)

    cat("</tr>\n", file=html)
  }

  cat("</table>\n\n", file=html)

  cat("</body>\n", file=html)

  cat("</html>\n", file=html)

  close(html)
  
  
  
  
  
  
  ####################################
  


