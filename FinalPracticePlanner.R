#Combine all csv files
files <- list.files()
df <- do.call(rbind, lapply
              #Remove top 9 rows during merge
              (files, read.csv, as.is=T, skip = 9, header = TRUE))

#Remove all columns except:
df <- df[, c("Period.Name", "Total.Duration","Total.Player.Load","Player.Load.Per.Minute","Position.Name")]

library(dplyr)

#Create new column - Classify Drills

classify_period <- function(name) {
  if (name %in% c("Dynamic Warm Up", "Bounce balls", "2 Pepper", "OTP", "Bumper Ball", "Bumper balls", "Tennis","Warm Up Stations","2.4 Pepper","Prepractice warmup","Bounce Balls",
                  "Match Warm Up","Bounce Balls 2 4 Pepper","Warm Up Butterfly","Warmup Pepper","Volleyball Match Warm Up","VB Match Warm Up Bounce Balls","VB Match Warm Up",
                  "Triangle D Warm Up","Stations Warmup","Stationary & Dynamic Warm Up","Station Warm Up","Slow Dynamic Warm Up","Setter Pre Match Reps","Serving Warm Up",
                  "Pre Match Serving","Pre Match Serve Pass","Pre Match Serve","Pre Match Reps","Pre Match Pepper","Pre Match Passing Reps","Pre Match Passing","Pre Match Pass Set",
                  "Pre Match Hitting Lines","Pre Match Extra Passing Reps","Pre Match Dynamic Warm Up","Pre Match Extra Passing Reps","Pre Match Dynamic Warm Up","Pre Match Dig Set",
                  "Pre Match Bounce Balls","Over the Net Pepper","OYO Warm Up","Neville's Pepper 6 v 6","Monday 8.29.22 Warm Up","Middles Pepper","Match Warm Up Setting",
                  "Match Warm Up Serving+Passing","Match Warm Up Serving","Match Warm Up Serve Pass","Match Warm Up Pepper","Match Warm Up Passing and Setting",
                  "Match Warm Up Passing & Setting","Match Warm Up Passing","Match Warm Up Hitting Lines","Match Warm Up Hitting LInes","Match Warm Up Dig+Set",
                  "Match Warm Up Dig Set","Match Warm Up Dig SEt","Match Warm Up Crimson Court Back Row Exchange","Match Warm Up Bounce Balls","Match Warm Up Attacking",
                  "Match Warm Dig Set","Match VB Warm Up Serve Pass","Match VB Warm Hitting Lines","Match VB Warm Up Bounce Balls + Dig Set","Match VB Warm Up",
                  "Match Serving","Match Serve Pass","Match Pepper","Pre Match VB Warm Up","Pre Match Setting Reps","Pre Match Setters Reps","Serve Warm Up","Serve Recieve Warm Up",
                  "Match Bounce Bal","Match Dig Set","Match Dynamic Warm Up","Match Hitting Lines","2 P Pepper","2 Person Pepper","3 Person Ball Control","4 Corners","4 Person Pepper",
                  "Ball Control","Ball Control Warm Up","Ball Handling Work","Butterfly","Cont Hit Pepper","Dynamic Warmup","Four Corners","Pepper","Pre Match Extra Setting Reps",
                  "Pre Match Setter Reps","Stations Warm Up","Match VB Warm Up Hitting Lines","Warm Up Ball Control")) {
    return("Warm Up")
  } else if (name %in% c("Back Row Exchange","Back Row Exchange-For Points","Back Row Exchange w Middles","Situational Wash Table","Match Play","Tribond","2 Point Mini Games","SR DB",
                         "Brow Exchange","3 Point Rotations Games Win by 2","Who Blinks First","Wash Table- w Subs","Wash Table 3","Wash Table 2","Wash Table - w Subs","Wash Table",
                         "Two Way Tri Bond","TriBond","Trans Attack Point Scoring","Slap Jack","Short Court","Sets to 15","Mini Games","SR + DB w Subs",
                         "S+P 2Min. Rounds w MB Approach & Hit","Rotations- 2","Rotations with Down Ball","Rotations -Serve + Down Ball","Rotations",
                         "Rotational Game Passing Activation 6s","Rotation game reverse scoring with MB activation","Rotation Points Play","Rotation Points",
                         "Rotation Games Reverse Scoring w MB activation","Rotation Games Passing Activation 6's","Rotation Games","Rotation Game reverse score",
                         "Rotation Game","Red Zone Games with Subs","Red Zone Games","Queens +1","Serve Receive Rotations w Hitting","Over on 2","Over in 2 Oos",
                         "Over in 2","Over In 2","Old School Volleyball","Old School Scoring plus Bonus Ball","Matrix","1 V 1 Short Cout","1 v 1 Short Court","15 Point Game",
                         "1v1v1","2 Point Mini Games Old School Scoring","2 Way Wash","2 Way Wash Table","2 v 2 Short Court","2v2","2v2 Short Court","4 v 4 wit alley","5 V 5",
                         "5 v 5","5v5","6 Rotation Games w Setters Rotating","6s MB Kill first Ball 2nd ball FBT","6v6 Overload","6 Rotation Games w Seters Rotating","BR Exchange",
                         "BSBH Over in 2","Back Row Exchange 3 Point Mini Games","Back Row Exchange Middles Activated","Back Row Exchange by Rotation",
                         "Back Row Exchange w MBs","Backrow Exchange","Bic to Pin Exchange","Bic to Pin Exchange drill","Bingo","Darts","DiBond","End Zone Rotations",
                         "First Ball Kill w Subs","Gauntlet","Gauntlet w Subs","Guantlet","Guantlet w Subs","Half Court 1 v 1","Half Court Doubles",
                         "Back Row Exchange 3 Point Mini Game","Mini games","Over in 2 OoS","Pass Hit + Bic","Rotation 6 work","Rotation Games Reverse Scoring  w MB activation",
                         "Rotations- w Subs","Set to 15","Tri Bond","3 v 6 Defensive Stops","DiBiond")) {
    return("Live Play")
  } else if (name %in% c("Extra Skills","Post Practice work","Post Practice Reps","Post practice Reps","Post Practice Hitting","Post Practice Conditioning","Extra Reps",
                         "Extra Reps Post Practice","Extra Serving","Extra Serving Passing Reps")) {
    return("Post Practice")
  } else if (name %in% c("You're the Women-Outsides","You're the Women-Opposite","You're the Women- Opposites","You're the Women- Middles","6 v 0 Two Courts","You're the Women-Middles",
                         "You're the Woman","You're The Woman Game 6","You're The Woman Game 5","You're The Woman Game 4","You're The Woman Game 3","You're The Woman Game 2",
                         "You're The Woman Game 1","Setting and be attacks","Setting and Blocking","Setting and Be Attacks","Pin Attacking","Offensive System Work",
                         "Offensive Rotation Games","Offensive Breakout Transition Attacking","Offensive Breakout","OH Work","OH Passing to Attacking","OH passing to attacking",
                         "Non Passing Pins Attacking","Non Passing Attacking","Serve Recieve Attacking","Serve Receive Rotations with Attack","Out of System drill",
                         "Out of System Setting","Out of System Attacking","Oos & Transition Attacking","OoS Attacking","Pass Hit + BIC","MB Tournament","MB Oppo Attacking",
                         "MB Hitting","MB Footwork","MB Attacking off Setters","MB Attacking","MB's Attacking off Setters","MB's Footwork Patterns","3 MB Attacking - Triangle",
                         "Approaches","Approaches Blocking MBs","Armswing Exchange","Attacking - Serve Receive Footwork","Arm Swing Exchange","Breakout Attacking","Hitters & Setters",
                         "Hitting Lines","Hitting off Boxes","In System Attacking","In System. Medium. Out of System","Offensive Roation Games","Short Serve Recieve to Attack")) {
    return("Attacking")
  } else if (name %in% c("Utah Sets","Setter Technical","Short Serve Recieve to Trans Attack","Setting to Targets and MB eye work","Setting to Targets & Middles Eye Work",
                         "Setting to Targets","Setting to Target and MB Eye Work","Setting Work","Setting Slides","Setting OH Attacks","Setting MB's 2","Setting MB Attacks",
                         "Setting Fundamentals","Setters, MBs, Oppos","Setters with non Passing Attacking","Setters no Pass Attacking","Setters Work","Setters Targets & MB Eye Work",
                         "Setters Pre Match Reps","Setters Middles Breakout","Setters MBs","Setters MB Eyework","Setters MB Block Eyework","Setters + Middles Work","Setters + MBs",
                         "Setters","Setter work","Setter Tech Work","Setter Skill Work","Middles Work","Middle Eye Work","MB Eye work of Setters","MB's & Setters","MBs & Setters"
  )) {
    return("Setting")
  } else if (name %in% c("Passers Technical","Passing & Setting Fund","SP + Activation","pass+approach footwork","Short Serve Recieve","Passing wtih Hitting","Passing with Sandbags",
                         "Passing w hoops","Passing to Target","Passing by Rotation w Hitting","Passing Work","Passing Served Balls","Passing Rotations w Hitting","Passing Reps",
                         "Passing Fundamentals","Passing Competition","Passing Bowled Balls with Setting + Middles","Passing Bowled Balls and Setting","Passing Bowled Balls",
                         "Passing 1 v 1 Competition","Passing (Sand Bag on Back)","Passing & Defense","Sandbag Passing","SR by Rotation - Row 1","SR by Rotation","Serve Recieve by Rotation-Row 2",
                         "Serve Recieve by Rotation- Row 5","Serve Recieve by Rotation- Row 3","Serve Recieve by Rotation w Hitters","Serve Recieve by Rotation w Attackers",
                         "Serve Recieve by Rotation -Row 6","Serve and Recieve","Serve Recieve by Rotation - Row 4","Serve Recieve by Rotation","Serve Recieve Rotations",
                         "Serve Receive","1 v 1 Pass Competition","3 Person Serve Recieve","4 Corner Passing","4 Corners Passing","4 Person Exchange","3 Person Serve Receive",
                         "Alternating Serve Recieve Point Play","Cross Court Hoop Passing & Passing","Hoop Passing","Hoop Passing and Passing","Pass Set","SR 2 Mini Rounds",
                         "Sand Bag Passing","Hoop Passing & Passing","Serve Recieve by Rotation -Row 6")) {
    return("Passing")
  } else if (name %in% c("BSBH","V and Kam Defensive Reps","Pin Defense","Partner Defense","Megan Individual Defense","Sand Bag Digging","SR 2 Min Rounds",
                         "Serve Recieve Down Ball","Scout Defense","MBs Defensive Work","Setting Slides & Sand Bag Digging","3 V 6 Defensive Stops","BSBH 2 Hitter Rotations",
                         "BSBH 3 Hitter Rotations","BSBH Scored Points","BSBH vs. 2 Hitters","Back Row Defense","DS Work","Defensive Stops","Defensive System (Box)",
                         "Defensive System + Trans","Defensive System and Transition","Defensive Toolkit (partner)","Defensive Work","Dig","Dig Set","Dig Set Cover","Dig Set Hit Pepper",
                         "Dig Set TEAM","Dig or Die","Dig, Set, Cover","Floor Defense","Libs Dig Set 2","Libs Dig Set")) {
    return("Defense")
  } else if (name %in% c("Set 1","Set 2","Set 3","Set 4","Set5","SEt 2")) {
    return("Match Play")
  } else if (name %in% c("Pre Practice Reps","PrePractice Reps","Pre Practice Work and Dynamic","Pre Practice Work","Pre Practice Passing Reps","Pre Practice Passing & Setting Reps",
                         "Pre Pracice Passing","Early Reps","Pre Pracitce Reps","Pre Practice Passing")) {
    return("Pre Practice")
  } else if (name %in% c("Serve Pass 2 Min Rounds","S.P 2 min rounds","Serving by Rotation","Serving Work","Serving Competition","Serving and Partner Passing","Serving",
                         "Megan Serving","Service Game","Serve Under Rope 42 to 45 mph","Serve Ladder","Serve Dig","Serve","Bowl Spin Serve","Bowl Spin Serve 2",
                         "Intentional Serving","Jump Spin Serve","Serve and Dig","Serving & Partner Passing")) {
    return("Serving")
  } else if (name %in% c("Pin Blocking","Middle Block Work","MB Eye Work","MB Blocking","MBs Blocking","MBs Blocking against Pin Sets","Block Continuation","Block System",
                         "Block Trips","Blocking Eye Work Sequnece","Blocking Eye Work Series","Blocking Footwork","Blocking Footwork Eyework","Blocking Handwork","Blocking Moves",
                         "Blocking Work","Blocking vs. Coaches","Bounce Ball & Blocking Footwork","Box Blocking","Continuous Blocking","Drill Blocking Eyework","Middles Technical",
                         "Blocking Eyework Series")) {
    return("Blocking")
  } else {
    return("Do Not Include")
  }
}


# Use mutate() and the classify_period() function to create the new column
df <- df %>%
  mutate(Period.Type = sapply(Period.Name, classify_period))

# Print the resulting data frame
print(df)

#Load data.table library

library(data.table)

#Change duration to time

df$Total.Duration <- as.ITime(df$Total.Duration)

#Group drills by period.name and period type

grouping <- aggregate(cbind(df$Total.Player.Load, df$Player.Load.Per.Minute), by = list(df$Period.Name, df$Period.Type), FUN = mean)

colnames(grouping) <- c("Drill","Tag","Player.Load", "Player.Load.Per.Minute")

duration <- df$Total.Duration[!duplicated(paste(df$Period.Name, df$Period.Type))]

#Add back duration column

grouping$Duration <- duration

print(grouping)

#Change Datatypes

grouping$Drill <- as.character(grouping$Drill)
grouping$Tag <- as.character(grouping$Tag)

#App

#Initialize Libraries
library(shiny)
library(dplyr)
library(jsonlite)
library(knitr)
library(shinythemes)

#Format app & add containers
ui <- fluidPage(
  titlePanel("Practice Planner"),
  tags$style(HTML("
    body {
      background-color: #000000;
      color: white;
    }
    .well {
      background-color: #CC0000;
      border-color: #000000;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("duration", "Duration (min)", min = 0, max = 60, value = 30),
      selectInput("tag", "Tag", choices = c("Passing","Live Play", "Warm Up",
                                            "Post Practice","Attacking","Setting","Defense","Match Play",
                                            "Serving","Pre Practice","Blocking"), selected = "Passing"),
      selectInput("selected_drill", "Select a Drill", choices = NULL),
      actionButton("add_drill", "Add to Schedule"),
      br(),
      br(),
      actionButton("remove_drill", "Remove from Schedule"),
      
      h4("Drill Schedule:"),
      tableOutput("drill_schedule")
    ),
    
    mainPanel(
      h4("Estimated Player Load Per Drill:"),
      verbatimTextOutput("estimated_player_load")
    )
  )
)

#Initialize server
server <- function(input, output, session) {
 
  #Make grouping a reactive dataframe 
  grouping_filtered <- reactive({
    grouping %>%
      filter(Tag == input$tag)
  })
  
  #Filter drill based on tag
  observe({
    updateSelectInput(session, "selected_drill", choices = unique(grouping_filtered()$Drill))
  })
  
  #Create drill schedule reactive dataframe
  drill_schedule <- reactiveVal(data.frame(Drill = character(), Duration = numeric(), `Estimated Player Load` = numeric()))
  
  
  # Reactive expression to calculate player load
  player_load <- reactive({
    if (!is.null(input$selected_drill) && input$duration > 0) {
      grouping %>%
        filter(Drill == input$selected_drill, Tag == input$tag) %>%
        mutate(`Estimated Player Load` = Player.Load.Per.Minute * as.numeric(input$duration)) %>%
        select(Drill, `Estimated Player Load`)
    } else {
      data.frame(Drill = character(), `Estimated Player Load` = numeric())
    }
  })
  
  #Calculate output based on filters applied by is
  output$estimated_player_load <- renderPrint({
    if (input$duration > 0) {
      estimated_data <- grouping %>% 
        filter(Tag == input$tag,)
      estimated_data <- estimated_data %>%
        mutate(`Estimated Player Load` = Player.Load.Per.Minute * as.numeric(input$duration)) %>%
        select(Drill, `Estimated Player Load`)
      
      knitr::kable(estimated_data)
    } else {
      "Duration must be greater than zero."
    }
  })
  #Output player load for selected drill
  observeEvent(input$selected_drill, {
    pl <- player_load()
    if (nrow(pl) > 0) {
      output$selected_drill_player_load <- renderPrint(knitr::kable(pl))
    }
  })
  # Add selected drill from drill schedule
  observeEvent(input$add_drill, {
    if (!is.null(input$selected_drill)) {
      new_drill_row <- data.frame(Drill = input$selected_drill, Duration = input$duration, `Estimated Player Load` = player_load()$`Estimated Player Load`)
      drill_schedule_data <- drill_schedule()
      new_drill_schedule_data <- rbind(drill_schedule_data, new_drill_row)
      drill_schedule(new_drill_schedule_data)
    }
  })
  
  
  # Remove selected drill from drill schedule
  observeEvent(input$remove_drill, {
    if (!is.null(input$selected_drill)) {
      drill_schedule_data <- drill_schedule()
      new_drill_schedule_data <- drill_schedule_data[!drill_schedule_data$Drill %in% input$selected_drill, ]
      drill_schedule(new_drill_schedule_data)
    }
  })
  
  # Display drill schedule
  output$drill_schedule <- renderTable({
    drill_schedule()
  }, include.rownames = FALSE)
  
  # display w/ default values
  outputOptions(output, "estimated_player_load", suspendWhenHidden = FALSE)
  
}

shinyApp(ui, server)

