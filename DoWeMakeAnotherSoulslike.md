Do We Make Another Souls-Like?
================
[Adriel Ortiz](https://linktr.ee/adriel_ortiz)
05/08/2025

## Introduction

<br> Many game designers aspire to make video games in one fashion or
another. Often, these dreams don’t coincide with actual game design
concepts or games in mind that they’d like to create (including myself).
From a top-down level, video games are usually differentiated by
gameplay archetypes called “genres” similar to how books’ stories/worlds
are.

That being said, let’s say we/they went through all the proper avenues
to learn how to design, code, and market. What game should be created?

Using the “**Games Metadata & Ratings (5k + Dataset)**” dataset, we’ll
dive into the different genres and the reviews attached to over 5000
games with the goal of finding the top 10 high rated game genres as a
jumping-off point to making a game.

We start by importing, cleaning, and merging the two CSV files the
dataset comes in, checking the first few rows to make sure it’s laid out
properly, and checking the dataframe structure to make sure all fields
are in their proper data types: <br> <br>

``` r
#Installing and Loading Packages
install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(dplyr)
library(ggplot2)

#Import/Read CSV files into dataframes
games_metadata <- read.csv("games_metadata_5k.csv")
games_ratings <- read.csv("game_ratings.csv")

#CLEANING GAMES_METADATA
#Select relevant columns
games_metadata_cleaned <- games_metadata |>
  dplyr::select(game_id, name, genres, released)

#Handle missing values
games_metadata_cleaned <- games_metadata_cleaned |>
  dplyr::filter(!is.na(game_id) & !is.na(name) & !is.na(genres) & !is.na(released))

#Convert ‘released’ column to date format/datatype
games_metadata_cleaned$released <- as.Date(games_metadata_cleaned$released)

#CLEANING GAMES_RATINGS
#Select relevant columns
games_ratings_cleaned <- games_ratings |>
  dplyr::select(game_id, user_id, rating)

#Handle missing values
games_ratings_cleaned <- games_ratings_cleaned |>
  dplyr::filter(!is.na(game_id) & !is.na(user_id) & !is.na(rating))

#MERGING DATAFRAMES
#Merge based on ‘game_id’
merged_data <- merge(games_metadata_cleaned, games_ratings_cleaned, by = "game_id")

#Select the desired columns in the final, merged, dataset
final_dataset <- merged_data |>
  dplyr::select(game_id, user_id, rating, name, genres, released)

#Display the first few rows of the final, merged dataset
head(final_dataset)

#Display the structure of the final, merged dataset
str(final_dataset)
```

    ##   game_id   user_id rating     name                        genres   released
    ## 1       4 user_9695      3 Penarium Action, Arcade, Casual, Indie 2015-09-22
    ## 2       4 user_7237      5 Penarium Action, Arcade, Casual, Indie 2015-09-22
    ## 3       4 user_5741      4 Penarium Action, Arcade, Casual, Indie 2015-09-22
    ## 4       4 user_7863      4 Penarium Action, Arcade, Casual, Indie 2015-09-22
    ## 5       4 user_4783      2 Penarium Action, Arcade, Casual, Indie 2015-09-22
    ## 6       4 user_6202      5 Penarium Action, Arcade, Casual, Indie 2015-09-22

    ## 'data.frame':    273669 obs. of  6 variables:
    ##  $ game_id : int  4 4 4 4 4 4 4 4 4 4 ...
    ##  $ user_id : chr  "user_9695" "user_7237" "user_5741" "user_7863" ...
    ##  $ rating  : int  3 5 4 4 2 5 4 1 1 5 ...
    ##  $ name    : chr  "Penarium" "Penarium" "Penarium" "Penarium" ...
    ##  $ genres  : chr  "Action, Arcade, Casual, Indie" "Action, Arcade, Casual, Indie" "Action, Arcade, Casual, Indie" "Action, Arcade, Casual, Indie" ...
    ##  $ released: Date, format: "2015-09-22" "2015-09-22" ...

<br> \## Top 10 Genres Based On Average Rating <br> After cleaning and
merging the dataframes, we’ll now process the data further in order to
make a chart to visualize the information we want to see. We’ll do this
by first grouping the data by genres and calculating the average rating.
<br> <br>

``` r
#Group by genres and calculate the average rating
genre_avg_ratings <- final_dataset |>
  group_by(genres) |>
  summarize(average_rating = mean(rating, na.rm = TRUE), n_ratings = n()) |>
  arrange(desc(average_rating))

#Filter out genres with very few ratings to avoid bias
genre_avg_ratings_filtered <- genre_avg_ratings |>
  filter(n_ratings >= 10)

#Select the top 10 genres by average rating
top_10_genres <- genre_avg_ratings_filtered |>
  top_n(10, average_rating)

#Create the bar chart using ggplot2
ggplot(top_10_genres, aes(x = reorder(genres, average_rating), y = average_rating))+
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = sprintf("%.2f", average_rating)), hjust = -0.1, size = 3)+
  coord_flip()+
  labs(title = "Top 10 Game Genres by Average User Rating", x = "Game Genre", y = "Average Rating")+
  theme_minimal()
```

![](DoWeMakeAnotherSoulslike_files/figure-gfm/t10genresavgrating-1.png)<!-- -->
<br>

According to the data, the top 10 game genres (based on average user
reviews) are: <br>

1.  Action, Strategy, Card

2.  Action, Simulation, Racing, Arcade

3.  Action, Adventure, Simulation, Casual, Indie

4.  Action, Simulation, Sports, Racing, Casual, Indie

5.  Action, Strategy, Massively Multiplayer

6.  Strategy, Arcade, Casual, Educational, Indie, Puzzle

7.  Adventure, Arcade, Casual, Indie, Platformer

8.  Adventure, Strategy, Casual, Family

9.  RPG, Simulation, Card, Indie

10. Action, Simulation, Arcade, Casual, Family, Indie <br>

These are complex genres, so it’ll help to pull up some game examples.
So let’s go on steam and grab an example of each and summarize the top
reviews (both negative and positive). <br>

### Action, Strategy, Card

#### [Library Of Ruina](https://store.steampowered.com/app/1256670/Library_Of_Ruina/)

![](https://lh3.googleusercontent.com/pw/AP1GczOEBouMd1X46OeNrUey_yBWfXxkY93BIU3IDjOb5w_DvQPVmtk54wBwttgN9mo_RdmmM0kFx6bWQMiQzfcFl37ZCVAXDzAaMOx_Gi1isZkI8om2XmOTmXCdmVvDb5PoPOY8wnvD6PnfBhbujyYXoSk=w884-h497-s-no-gm?authuser=4)

Reading through the reviews, it seems that one of the biggest draws is
the story, followed by the difficulty of the game. There are people that
love the unforgiving nature of the game, but others hate it. They also
mentioned that there is significant time-investment in getting a setup
desirable for defeating challenging opponents. Seems to me that the
aesthetic paired with the story of the game with a strict gameplay loop
reinforces people to want to stick to the game to get more story. <br>
<br>

#### [One Step From Eden](https://store.steampowered.com/app/960690/One_Step_From_Eden/)

![](https://lh3.googleusercontent.com/pw/AP1GczNz4OxYp5lp0rWtsZ3qD9QGQggnXpjR0isNbBaT9vrH3H14zkW7a5fDWui9NsGLzrGSuf-9k1yQqsZ7yQzqHWdD8srWxpwpRv8JIF19ftwhhOxC9N8baHhlek3vg1rhKAE1oLPY_e-6TWjq3b3KdR4=w884-h498-s-no-gm?authuser=4)

This game is very reminiscent of an old popular game series named
“Megaman Battle Network”. I played the series myself and loved it.
However, unlike MMBN, this is less turn-based and more real-time. Kind
of like Kingdom Hearts Chain of Memories (which I’m surprised no one
mentioned). People like and hate the difficulty of the game as it is not
friendly to casual players and has a learning curve, but near no mention
of the story. The ones that did mention the story mentioned that it was
very forgettable or just not good. That being said, they say that the
ability to deck build however you want allows there to be an almost
endless amount of possibilities that allows the game to feel pretty new
with each playthrough. While Library of Ruina was loved for its story,
One Step From Eden is loved for its gameplay. <br> <br>

### Action, Simulation, Racing, Arcade

#### [Superflight](https://store.steampowered.com/app/732430/Superflight/)

![](https://lh3.googleusercontent.com/pw/AP1GczOBGS0lTBbkYtOfLAxdogNFyFPXXAg27NyO9YARJq4reHGCcHkguLgSNIIPdybHWF9kepGmSwAOxVtbj5pJdW0vJWVFsJn9qIB8AkoEDWtoZBBnUBHhPUoXoc0BA7sSHwnLGmvJxVni98pydU3jg5w=w884-h497-s-no-gm?authuser=4)

If you were thinking “I have no idea what games would fall under this
category… Mario Kart?” then you’re not alone, I was too. This game
simulates gliding in a wing-suit and maneuvering for a highscore; with
the high speed and short game rounds (the publisher says “A great game
to relax for half an hour and chase your latest highscore!”) make it fit
this genre. What people like about this game is how you can just jump
into it and zone-out. One even described it less of a game, and more of
an interactive toy because of its simplicity. The people that didn’t
like it say the “rounds” are too long, they don’t like the map designs,
and don’t like the art direction (no music, polygon models). A very
experimental game, but low barrier to entry (\$3) and caters to more of
a casual gaming crowd. <br> <br>

#### [CarX Drift Racing Online](https://store.steampowered.com/app/635260/CarX_Drift_Racing_Online/)

![](https://lh3.googleusercontent.com/pw/AP1GczN7SSnVn-9OTZxNSIDil9oSgcFyIB7gIofNHNek4UYS2SnAz2mcR3-zxon8WOjjL486GF2N6gBXOWS7wqZFzKW2ZOYwgDNH6GUKLl-MipxQxgW-Uei1a3onm4_H7nnCfkht7_HZG5MZMOzP_mg8Jxo=w884-h497-s-no-gm?authuser=4)

This is a game that is out on multiple game systems, from PC to
smartphones and people love it! They like the customization (you can
modify the car body AND engine preferences), you can drift race with
your friends online, and it has VR support! The negative reviews seem to
be solely from unoptimized coding making the game crash or using up too
many system resources. The game is \$15 so seeing that a lot of people
bought it multiple times bodes well for this genre if designed
correctly! <br> <br>

### Action, Adventure, Simulation, Casual, Indie

#### [Slime Rancher](https://store.steampowered.com/app/433340/Slime_Rancher/)

![](https://lh3.googleusercontent.com/pw/AP1GczPavVj42DtpJjBs6ap-gDB0T28I54ThHLY20JLMaQT0OX7EA0vdY64oyCo0v8dNEihv3w6-XWldq9FiFoDGPiCcSky0qf8_Fy_VnQbl3zAmseHWj-6CyKhdY2sLG1v0iVcyqJd1ykg5a7cflj-xCmE=w884-h497-s-no-gm?authuser=4)

This game is catered towards casual gamers (it’s in the genre). It
centers around being a slime (the monster kind) rancher/farmer with a
cute and colorful art style. It’s very simplistic in its gameplay loop
which is why many love it, but it’s the main reason for its dislike;
many describe it as boring and repetitive. Its PG nature may also be why
it’s so positively coveted as parents can buy this game for their
children and not worry about it being inappropriate or needlessly
violent. <br> <br>

#### [DAVE THE DIVER](https://store.steampowered.com/app/1868140/DAVE_THE_DIVER/)

![](https://lh3.googleusercontent.com/pw/AP1GczN3_FftOO0glp-MJhKSaAJMXs4ev0KoHEw2nmBTkCwy7SYxWSwTPCrOeuLC8VsoTkmM4JuQs5NSU43N74oVVbZkDfAPWFTECaiDR1vuDtsVPB2XDiJaddITzVddU52XCAJY1_LGue3JG7toMBXPUG8=w884-h496-s-no-gm?authuser=4)

Reading the reviews, it’s apparent that this game is one of the kings of
its genre. People find the art direction charming and harmonious (music,
art, and gameplay all match and feed well into each other). They love
the variety of having essentially two sides to the game. A day time
adventure side that has you scuba diving for fresh fish and other
materials for the night time restaurant management/simulation side with
what people say is a great story to accompany it. People love the
variety of minigames as well! The only thing holding it back, and the
only thing people have complained about in the negative reviews are
something I’ve never seen before and that’s “Limited Time DLCs”. I’ve
never heard of a game releasing DLCs only purchasable for a couple of
months before. I’ve heard of limited time events which are free, but
never limited time DLC unless it’s a cosmetic microtransaction. <br>
<br>

### Action, Simulation, Sports, Racing, Casual, Indie

Ironically, there isn’t much here for this genre. Superflight pops up
again as well as other racing games which are more of the same. This
could be a perfect opportunity to snuggle into this niche genre. Let’s
brainstorm two game ideas. <br>

1.  A low-poly skateboard/hoverboard racing game inspired by [Sonic
    Riders](https://en.wikipedia.org/wiki/Sonic_Riders) ([Metacritic
    user score 7.9/10](https://www.metacritic.com/game/sonic-riders/))

2.  A pixel bobsledding racing game inspired by the
    [F-Zero](https://en.wikipedia.org/wiki/F-Zero_X) series ([Metacritic
    user score 8.7/10](https://www.metacritic.com/game/f-zero-x/)) <br>

### Action, Strategy, Massively Multiplayer

#### [Squad](https://store.steampowered.com/app/393380/Squad/)

![](https://lh3.googleusercontent.com/pw/AP1GczPGesNxoLg3wYekkYnZI8TxU5ieeQldL8L7hy_L7F2WXLfI-K7mENuiO3wioivE2B0Ri1rnaNYYK-_aaOMAJ0TOdD6Oqopj7eKRv1z6owCrakIGEiYidGEc_V-LLSl_ZXjA2O0Opmgu5QdQJEqXP6E=w884-h496-s-no-gm?authuser=4)

According to reviews, this game is one of the best military sims out
there now. With pretty realistic gun mechanics to requiring strategizing
in 100-player battles, this ticks all the boxes for the genre. People
generally have a fantastic time with this game with plenty of players
reaching over 500 hours. There are two main points of contention when
reading the negative reviews: poor optimization, and over enthusiastic
military role players (one negative review called them meal team 6 and
almost choked on my drink). <br> <br>

#### [EVE Online](https://store.steampowered.com/app/8500/EVE_Online/)

![](https://lh3.googleusercontent.com/pw/AP1GczPuGNjOKrj-AQwqvZoDxIaJBLYriyNpna2HuyyXrsQ6CLYoCVOkgovJuP2GR1ws48pzGQfNjRK4c7npjBcRKVPqb2JmxPz2sHOJx1KjIlLfW2MjvW3_Qog_6Od3ssZDCosI89AyMmLcQzrNltyCDHs=w884-h497-s-no-gm?authuser=4)

I’ve heard of this game for literally decades and I’m surprised it’s
still going strong (this game was released in 2003). Players enjoy the
complexity and wide-range of options on how to play the game, and most
of the players are very loyal. Maybe it’s from the investments as this
game may be free to play, but it also has a subscription model.
According to the negative reviews, the developers began a number of
updates that changed the way the game played in a more restrictive way
that actually took away features that players enjoyed, like mining.
Someone that recommended the game said the downside of the game is “you
can lose days of progress in a matter of seconds” and the developers
actually responded saying “This is what gives EVE the unique status it
has in many players’ minds”, but I’m not sure this is something that
HUGE amount of people would like, only a small few. As the real world
economy gets worse and people’s time becomes scarce, they’d want their
time to be worth something that lasts longer. That may be something to
take advantage of when designing a game if leaning in this genre’s
direction. <br> <br>

### Strategy, Arcade, Casual, Educational, Indie, Puzzle

#### [RoboCo](https://store.steampowered.com/app/1067220/RoboCo/)

![](https://lh3.googleusercontent.com/pw/AP1GczNxjSfygv4hwJIrIac-vWc9U9qmtiq8zwli5VYxuPmIxl7bj4hj7AhNeGmv5c8gkDSxl96jGRVEFv0u19ircOL52xM8VCg0y5gX4lQEka89Bwr3ckVNUj8_8q2fQuePNSJEM6YbjUKZLj6k-9QnlOA=w884-h496-s-no-gm?authuser=4)

Looking at the steam page for this game, this is exactly what I would
imagine comes to mind with this genre. You create robots to solve
open-ended challenges and complete tasks. People really like the depth
of gameplay as you could even type in code to automate your robots. They
think that this is a great game for STEM field play/learning. There are
three main complaints for this game so far: buggy/bad physics, vague
scoring system (you need to meet certain scores to move onto the next
level), and lack of content. Most of these issues are growing pains, and
seeing as this is an early access game, they still have the opportunity
to fix them and be even more successful! <br> <br>

#### [Bloxi: The Word Game](https://store.steampowered.com/app/1782050/Bloxi_The_Word_Game/)

![](https://lh3.googleusercontent.com/pw/AP1GczMKtiWdVieT2mUc4pOoW2kSOU-UUmU7TanDtkBCP36YcIlR2RgJI3FzOf7bFHA8Lr1NGu8K8Z6N2dZ2gd0kRQm-Ua5Vu-DGSupAWqHyLtvAPFgmIBFokxS9Cf7CoQWAm6u0DHp5APc1aSDsYr9VHXw=w884-h495-s-no-gm?authuser=4)

Bloxi is a combination of Scrabble and Tetris where you have to make
words out of falling blocks/shapes similar to Tetris blocks, a very
unique concept. Reviewers like the originality of the concept and the
art style; they also mention that the game operates very smoothly and
the concept was applied very well. The downside is that as a word game,
you fall into the pitfalls of being unable to create long words because
sometimes these long words have word fragments in them. As a
tongue-in-cheek example, disestablishmentarianism, if I was trying to
build that word, it would probably clear itself after forming
“disestablish”, as well as it taking up the whole screen with “noise”
letters in true Tetris fashion. <br> <br>

### Adventure, Arcade, Casual, Indie, Platformer

#### [Geometry Dash](https://store.steampowered.com/app/322170/Geometry_Dash/)

![](https://lh3.googleusercontent.com/pw/AP1GczPoabx4Twu3LhdQdOsvgw3d0IK9FqViWIl2MDL6iX_IsBAqwPbhs7V3djGPIXTkNfQiXtKIJP_2sM8UZY1PVog4WyBZeChqMaHwuF6I-B6A6PzdR23nSH26MqpP50q3lqlc1xIheYbBu-34QycCUos=w884-h496-s-no-gm?authuser=4)

Geometry Dash has been out for 11 years and has been popular in its
genre the entire time. In a few words, you’re a geometric shape being
dragged along a stage and you must platform through it. Everyone loves
the art direction (visuals and music) and the simplicity of the
gameplay. Everyone pokes fun at how rage-inducing this game can be, and
if you ask me, the negative reviews just add to that. About 80% - 90% of
the negative reviews are people raging at the game. The only
constructive negative review that I could forage was that there’s no
cloud save, and in 2025 with release on multiple platforms and people
pouring hundreds of hours on this game, seems like an oversight. The
developer has been known to rage-bait people, so maybe this was also a
feature to add additional frustration to an already frustrating game!
<br> <br>

#### [UFO 50](https://store.steampowered.com/app/1147860/UFO_50/)

![](https://lh3.googleusercontent.com/pw/AP1GczNEpFPmRhEyNuU44h0nPIY_s15sIXqezePA2f8uYCZG3d4RxQ7s23TgjXcgZLgd9W3vXKSHpUXjI0Dl8-6JFg9IGaN_sE-J-ZSpxIqPT6CafUnsh67qWXy_sXgNwNJIE6RjQfO5o8HdMD3XIIC571Q=w884-h497-s-no-gm?authuser=4)

UFO 50 is a game collection of 50 retro-styled games. Consumers say the
games are very polished, nostalgic. Most people say that they were
caught off guard by how good all the games were as the value is
essentially 50 cents per game at full price which is an insane value
when AAA companies like Nintendo are about to charge \$80 for their
games. A lot of the reviews say they didn’t like the games, but it may
be a difference in experience as people that played these 8-bit games
when they were younger kind of expected the difficulty curve; such as,
no instructions for the games. You start the game, they toss you in,
that’s usually how it was back then as well as the instructions where
either on the game cabinets or in paper manuals. <br> <br>

### Adventure, Strategy, Casual, Family

**Disclaimer:** I’m not too sure what they meant by “family”. I assume
they mean “fun with the family” so I added a multiplayer tag to it in
order to fulfill that “playing with the family” definition.

#### [In Sink: A Co-op Escape Adventure](https://store.steampowered.com/app/1858650/In_Sink_A_Coop_Escape_Adventure/)

![](https://lh3.googleusercontent.com/pw/AP1GczMB9k6sLQOXpTTzIedWv1jxVp6gqW2ARrQR_9wvYxqWvMASc3pBOLcZoEAHgVGO1jHbwj6OFwHRD800rQD4CKr-qtFwzF0419wkg-Q8Vq80e6ET00GGB7puC73IzTewrPha07bSt0BtRRp1Nt7V_4E=w884-h497-s-no-gm?authuser=4)

In this game, two characters are ship wrecked and they need to find a
way to escape this deserted island. There are a bunch of puzzles and the
game takes on an escape room theme. Players on average seem to enjoy the
game a lot! They describe the puzzles as kind of challenging while also
enjoying the art direction. The negative reviews claim that some of the
stages are unoptimized, they say the platforming is atrocious, and that
the puzzles themselves are kind of repetitive after a while. <br> <br>

#### [Billion Road](https://store.steampowered.com/app/1163040/Billion_Road/)

![](https://lh3.googleusercontent.com/pw/AP1GczOSoi1G1mFR9KByQnDSFBAOHUOo789VdpAOcHa2B6ReACtzgBp4sZzG8aG62b8A-p1Ass4Hl00_0TMcgVSN2-NmcgLoLOd4ZQdVXFguMcTOJPzubbEq9wQson6tDmqLSsR7sih0fs6Bl5xxQo-4RWM=w884-h495-s-no-gm?authuser=4)

Billion Road is a silly japanese-pop version of Monopoly and Mario Party
and many of the reviews mention as much. Some say that this is their
favorite game despite playing many other real-life board games like
Monopoly and Chutes and Ladders. The negative reviews mention that it’s
redundant and there are some mechanics that make it near impossible to
come back from, but if you play Monopoly and Mario Party, the same could
be said about those two. All in all, this matches the genre well, and
seems to be a great example of it. <br> <br>

### RPG, Simulation, Card, Indie

#### [I Was a Teenage Exocolonist](https://store.steampowered.com/app/1148760/I_Was_a_Teenage_Exocolonist/)

![](https://lh3.googleusercontent.com/pw/AP1GczM5LhAPbgJWcDrlHokrx_YSkzbeipN5QgvwahVh2A3nO8Ns54yrz-RHLP_d3QKKq0Lll0s3a8m9xn8f9l5flaa9B1yYC4pp0TQtMTlIkhWaSgUY7Sp5OvoCYNIFEcfCy_wkcnaCdObVZDDMfibtFb0=w884-h495-s-no-gm?authuser=4)

This game is one that I haven’t seen reviewed so well in a long time
while also never hearing about it despite this game being released in
2022. This title is a life-sim about growing up on an alien planet with
RPG elements dependent on deckbuilding. It’s VERY story heavy and there
are some reviewers that mention that this being one of the few games
that made them cry and being one of the few games that got them
emotionally invested. The negative reviews mention that the story is
“twee” and others mention its more of a “choose your own adventure”
story than a game, but reading the tags that the developer put on it,
this is one of the game where choices matter which lead me to believe
that this was just a preference difference rather than a game flaw. <br>
<br>

#### [Potionomics](https://store.steampowered.com/app/1874490/Potionomics/)

![](https://lh3.googleusercontent.com/pw/AP1GczMgHo1-tqWunWUhTZI1F27OBT9dzEZNsffyP2s7r0dWe_Im2Uzqy7CilYev4vIfhWYTHqlGcgfmn2SGkzavzpiPQNw9o0oyw517Cv9PN8cq4fQm4lkQSvOrnlyC4DRiFmb3UmvSNGyb1WEOo7p_aU0=w884-h496-s-no-gm?authuser=4)

Potionomics is a dating and potion shop simulation with deckbuilding
tied to capitalism with RPG elements. Players say they really enjoy the
art and the gameplay loop. The voice acting is also really good. Though
there are two things that the positive and negative reviews agree on,
and those are: the UI/UX isn’t that good, and the time-management
portion of the gameplay is super strict. Many of the positive reviews
mention that they needed to make use of spreadsheets and notepads in
order to play the game the way they wanted to. That being said, a
majority of the negative reviews mention that the RNG of the game can be
unforgiving. They also bring up that the game throws a lot of mechanics
at you in the beginning which can be overwhelming. <br> <br>

### Action, Simulation, Arcade, Casual, Family, Indie

**Disclaimer:** Similar to the previous family genre, I added
Multiplayer to the tag to imply “fun with the family” as the definition
of the “family” genre/tag.

#### [Clash of Chefs VR](https://store.steampowered.com/app/960040/Clash_of_Chefs_VR/)

![](https://lh3.googleusercontent.com/pw/AP1GczN-o8COxL9MDiES5za-OL8AL9TzEM3tqWCwBl2yg5CgpH--B_ALpJrIHg5QClorA-ckbc_87WyimKpqT8OM-naVPv_lZjxA2KmgnitrxyDcWZ2RuuJr1Gn6E3KYU1A-VBLsVNmxdY4WCpa0opUWR6Q=w884-h496-s-no-gm?authuser=4)

Clash of Chefs VR is a competitive cooking game where you have to race
to complete orders; you can play single player or multiplayer. The
positive reviews allude to this being one of the better fast paced food
prep games on the market, but the negative reviews tout the number of
bugs that can really put a hamper on the speed aspect of the game. This
game is in early access, but a lot of these negative reviews were made
when the game was in early access, and now it’s fully released. <br>
<br>

#### [Overcooked! 2](https://store.steampowered.com/app/728880/Overcooked_2/)

![](https://lh3.googleusercontent.com/pw/AP1GczOvhtqxm97eWjbbRyYbZEebbRVEgqQk6HU4F4X69XbTsuwi7-MdErnifMM7G4MtLjIyaittu4_0MrnS_kzZCO8iweaGsV-xFsOk7p5kknmIU14QXDpx6gHx8K41K4XVSR3jsUFJdti_Ra_JvKI4xJA=w884-h496-s-no-gm?authuser=4)

The Overcooked series is one I hold far and away from my heart, not
because it’s bad, but because despite it being a game that is teamwork
dependent, it’ll have you screaming at your teammates like Gordon Ramsey
and my poor heart can’t take the rage. The positive reviews say the
same, other than also including that they love the game and how funny it
is. The negative reviews touch on lag when playing online, controller
bugs, and playing with random people online being impossible for beating
the game, but otherwise, everyone likes this cult classic and you can
play it without VR! <br> <br>

## Top 10 Genres Based On Review Volume Of 4+/5

Top 10 genres based on average reviews are one thing, but all of them
end up landing below a 4 out of 5. That leads to having an abundance of
“alright” games, but nothing that screams “super successful”. So let’s
reorganize the data based on review volume of 4 out of 5 or higher and
see if the genres remain the same. <br> <br>

``` r
#Creating a new plot based on high rating occurrences
#Filter for the highest ratings
high_ratings <- final_dataset |>
dplyr::filter(rating>=4)

#Group by genres and count the number of high ratings
genre_high_rating_counts <- high_ratings |>
group_by(genres) |>
summarize(high_rating_volume = n()) |>
arrange(desc(high_rating_volume))

#Select the top 10 genres by high rating volume
top_10_genres_by_volume <- genre_high_rating_counts |>
top_n(10, high_rating_volume)

#Create the bar chart for the top 10 genres by high rating volume (4+/5)
ggplot(top_10_genres_by_volume, aes(x = reorder(genres, high_rating_volume), y = high_rating_volume))+
geom_bar(stat="identity", fill="purple")+
geom_text(aes(label=high_rating_volume), hjust=-0.1, size=3)+
coord_flip()+
labs(title="Top 10 Game Genres by Volume of 4 and 5 Star Ratings", x = "Game Genre", y = "Number of 4 and 5 Star Ratings")+
theme_minimal()
```

![](DoWeMakeAnotherSoulslike_files/figure-gfm/10genresbaseonreviewvol-1.png)<!-- -->
<br>

According to the data, the top 10 game genres (based on average user
reviews) are: <br>

1.  Action, Shooter
2.  Action, Adventure
3.  Action
4.  Action, Indie
5.  Action, Adventure, Indie
6.  Action, RPG
7.  Strategy
8.  Adventure, Indie
9.  Adventure
10. Action, Adventure, RPG, Indie <br>

There’s a lot of repetition in the categories which makes me wonder if
there’s overlap in the genres. Based on this, I wonder if there are any
Action, Shooter, Adventure, Indie, RPG games in the market, but I
suspect there are as Destiny 2 is a game that comes to mind (only they
aren’t indie as they were created originally by Blizzard/Activision).
Instead of picking two games for each genre combination, I’ll choose
three without summarizing reviews as these are highly popular games, and
at the very end we’ll mash all the genres together and see what examples
pop up. <br> <br>

### Action, Shooter

#### [Counter-Strike 2](https://store.steampowered.com/app/730/CounterStrike_2/)

![](https://lh3.googleusercontent.com/pw/AP1GczMeqRuCRDEuJtI4F7nxl-0qRTWJnwoAvzjkuJICakZ5rarNc_nXh3B99XawuEVVGKvOpmGt-xKO9Vs3aJSuP68k5ShNdul_C8rcuZoIbAYeqCDHn9rddE-hKaQT23SWhDOSYaLv7bMxTfguz4RbGxw=w884-h497-s-no-gm?authuser=4)
<br> <br>

#### [Marvel Rivals](https://store.steampowered.com/app/2767030/Marvel_Rivals/)

![](https://lh3.googleusercontent.com/pw/AP1GczMbP_JTw3n0J07kJV0dfpFD33RW8lRFMh1uSqb7ncaYAWSpgQqhSOBfGxIf6mrzKpPUXRuZnirT5uyWt4RQ7tTJp7ROVF8a_EBzmisxVYKMXUW1sYol6YbrtSv6yfrF-ZsWfnOWdhccBDXQhVej8fU=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [Warframe](https://store.steampowered.com/app/230410/Warframe/)

![](https://lh3.googleusercontent.com/pw/AP1GczN35m13vCq_0Ox17PesUPe5x9DhKN02V6apWwtwz4xnMQKOV-33ZAa6UjL20Ggni-TSN3rCboo6096gTTFpFZbWUmOrrMwQ0gyaCQIaGnUk14R4f3EHAdIuUB-JrDBXnTMzpqG6dlweHRWuaMNmFe8=w884-h497-s-no-gm?authuser=4)
<br> <br>

### Action, Adventure

#### [The Elder Scrolls IV: Oblivion Remastered](https://store.steampowered.com/app/2623190/The_Elder_Scrolls_IV_Oblivion_Remastered/)

![](https://lh3.googleusercontent.com/pw/AP1GczOy1xpYlReOhk8cvBQe74Qdqr4AztIfwlmDXf7ZfNx5x9M6og-QrGt9XyW2l0jNcAUmyICOmXCW9pWnvrvEBS3GLXBUv_djBOYh8Hj7Wy9K5adNsn0OpzhsT8VxhQuBRGbGHJO9XS53ANXztfXRLA0=w884-h497-s-no-gm?authuser=4)
<br> <br>

#### [Once Human](https://store.steampowered.com/app/2139460/Once_Human/)

![](https://lh3.googleusercontent.com/pw/AP1GczMZDjvuyARJthQOLZoKchBcEFKNZ2TMUjk1LSSyLnOIwqCAkZ8SHT2_ZUjNo6faU3U5CxI9Sa4XrIT0JJxQT2hGhnt1GIprVFZaEWAjucUc9YuvIy0nTdkmg8tVTdSQ3JWUML3izTPkbpGFBPiuFvA=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [V Rising](https://store.steampowered.com/app/1604030/V_Rising/)

![](https://lh3.googleusercontent.com/pw/AP1GczMCN8TTpk16189ZgBbGir7vwcVqGM2jxV04yq6jv2UuI41tv8JMKahPjN7V92WqlN8aF-Aqvt6Cq-tOTixwGiwNwHwzW4_Hc5xl8mU9qksUfOU6DUhJ18IMWe5x-lbZTycJGx_p9n7Xh48DkkR1q4Q=w884-h497-s-no-gm?authuser=4)
<br> <br>

### Action

#### [HELLDIVERS 2](https://store.steampowered.com/app/553850/HELLDIVERS_2/)

![](https://lh3.googleusercontent.com/pw/AP1GczMj_SfpfcVZbNUNnTxzOOH5H_DSs0FV1i-0jpbxay_tsSxghGqo3DdwDtDsfLOJSmZi8qOXJVG_c2PaJBRoPQn9Sc0xuHTM6uWDDGIuC3HJWCFuW0nBuJ5IVAMTQVI31uhXn7G1wxbBTNg341SJPXE=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [Risk of Rain 2](https://store.steampowered.com/app/632360/Risk_of_Rain_2/)

![](https://lh3.googleusercontent.com/pw/AP1GczOPOFMfozAYgYlSlgs7se3bBfYr3rdVO5Tw88jp4E5wQx31Gr34S_Fb1_zxWJTc3rlG6nBOL_xGXc-sN6FhTy3TtWJXl4WNZt8O97f9rdCRiU_imbUOpozUltS9ffeFhsW5dlxhdEBXPKmIsu04Ooc=w884-h497-s-no-gm?authuser=4)
<br> <br>

#### [DOOM: Eternal](https://store.steampowered.com/app/782330/DOOM_Eternal/)

![](https://lh3.googleusercontent.com/pw/AP1GczMU6XLV_cg3GL885OfbGvf-wCRTSvkytLgcSdPgxqVtrGhqdkCi8pP10pSWfd5JswHRzcwrVf9FG5zakFr9qjgAJUOvssDtjCpvNfn3ZIvJtKTYAnyQVvTwQ76mIdjWohhAT-QAlz5rnHRAE7UAtLE=w884-h496-s-no-gm?authuser=4)
<br> <br>

### Action, Indie

#### [Cuphead](https://store.steampowered.com/app/268910/Cuphead/)

![](https://lh3.googleusercontent.com/pw/AP1GczPzCn0q7pG0apg0wJZR186AyjLxNDX1ZdKFKaSUiWsb0uYCQYzpdLbqx1qVnFmu7PqGsZwtQIk9urN6s7uxo9So3vV5lujoSXfbHOb-DWZtNQCC1ftqu0LjYd4aFuyXNpBfQzQIXOEBfszL8mu846Q=w884-h496-s-no-gm?authuser=4)
<br> <br>

#### [Hades](https://store.steampowered.com/app/1145360/Hades/)

![](https://lh3.googleusercontent.com/pw/AP1GczPlVOafuRt5c2eYKeGxzTlnhjWwwNan9kEN-Egfrl4fQ3mcvAUuru_G5DFH3kO5YNjMP3V6cdckxw5fcsoFIudBvgF5icCUIwd0Nv5XP8M1F5LOLRH5myqW_7h7o3aWJnxXvNLZOy4vkwfAKXRIycU=w884-h496-s-no-gm?authuser=4)
<br> <br>

#### [Hollow Knight](https://store.steampowered.com/app/367520/Hollow_Knight/)

![](https://lh3.googleusercontent.com/pw/AP1GczMUzNZQYgzf7dVlymsUKTCkM5TSXo5uWklipxZzZg66F2VAWJRZ6QtA9zLaAzKkDJWxIYK_L585ERfF0OUf-Cx-nCpVpiJiDjdZtr2dK2C8rDUdKHodkOxzFvi-MzkZQvfsQwxRciCUreEc_6aK0K8=w884-h498-s-no-gm?authuser=4)
<br> <br>

### Action, Adventure, Indie

#### [Nine Sols](https://store.steampowered.com/app/1809540/Nine_Sols/)

![](https://lh3.googleusercontent.com/pw/AP1GczMd7jcixdKM27O6FYivPO7eMTKVF4CR9EUG__uMF1Zv9CTeQblLtYYVjQUK0kiMffzq90FCMOa42O6kYh2THCErRe7pb_fMrQWlKUBxjU-fGHO-3Lv_veASbGZlOY0toyw8vGdhE4028jZSolRNHt4=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [Ori and the Will of the Wisps](https://store.steampowered.com/app/1057090/Ori_and_the_Will_of_the_Wisps/)

![](https://lh3.googleusercontent.com/pw/AP1GczP5hkBf_imLgWHVCKzWPmC28yAbrIV3HYGfOL1KafB2wSJVFk9sm4n9ZI0OXs7jibFjV48xUwr9x7aEqUpc-0rA7yNOROVSYl6dWHNEppL3Kwke-Fdag7dfjNjnfhlmEHgFYFsLzJEeKv0CHqstogQ=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [Kingdom Come: Deliverance](https://store.steampowered.com/app/1771300/Kingdom_Come_Deliverance_II/)

![](https://lh3.googleusercontent.com/pw/AP1GczPt0LGnYQ-8ut1eanogTLKAjirGNXK133RaMlIMULMl9R7W1vX2BtvS6yfEIKp79vrTsNyWOKlwJV7dQPCK0MMLh03IIXgl5vwlD-fYVYbn-lpy4xiGUfw8AkVhIve_RbwOC1sbT5hBffbsA9n0C1Q=w884-h495-s-no-gm?authuser=4)
<br> <br>

### Action, RPG

#### [Monster Hunter: World](https://store.steampowered.com/app/582010/Monster_Hunter_World/)

![](https://lh3.googleusercontent.com/pw/AP1GczM-LTqVqrRcuJs3o4VWDl_oUJ-JDYsRtzrf-ad0w_GTPDTIzYs20P6xmqH1jENYG9xP4cusyJK5IGM5obE2SISNRndYLPil5cMFWdW_hAW-9QdZ8uphPbRIEHbQt7OHFfz12T0dt7gYKzo3sIVIGAQ=w884-h497-s-no-gm?authuser=4)
<br> <br>

#### [Cyberpunk 2077](https://store.steampowered.com/app/1091500/Cyberpunk_2077/)

![](https://lh3.googleusercontent.com/pw/AP1GczOtZrrIYkCigT0XYuvpM_NPDBRVjy4x_WicFtIdPbtJEMZDI6DYrv_OEA7fNkqAz8sAYqVnFzr5Z61EUHi40aZaCwRNeIaQPTx2413K2Hj_y3YOXitRTaJd0gU9mhAizKqkl1N6tC59NdGcnRjwylw=w884-h496-s-no-gm?authuser=4)
<br> <br>

#### [NieR: Automata](https://store.steampowered.com/app/524220/NieRAutomata/)

![](https://lh3.googleusercontent.com/pw/AP1GczNLKCx0fMtHaJbsQKBRu80G1ZuacJfZ_vpR9gPjSwuWYwwo604fFaNBASytGroIGx0Tz-vIngMungfO-Q0zhz7Vh4WgcsQ98jZLinE137dCEkhuZsKOgGr1wbUSMMaKd22uTzPBhg5J2uG60TiwNXE=w884-h496-s-no-gm?authuser=4)
<br> <br>

### Strategy

#### [Sid Meier’s Civilization VI](https://store.steampowered.com/app/289070/Sid_Meiers_Civilization_VI/)

![](https://lh3.googleusercontent.com/pw/AP1GczOcFm66Zbgu5KdCZHzUQfpoHcmJ7v-J3nmq9mJIgZ87PvavFkXE8LiHOXS11c0SKin49uTAgVJkYm9EgvyzZJccjJ9__BDKT4dkohkGPdCYTPGtbJi2S7Hzh1lILd1tb-PE-DQbGTK5sNS4CcEyFZk=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [Stellaris](https://store.steampowered.com/app/281990/Stellaris/)

![](https://lh3.googleusercontent.com/pw/AP1GczOA5svrCNWCApi146CpAighsZaYPnDfbpooieAEnpFJGDsUmtEXHPQ_bcNJxtYDPwPIf-eNtktOLYoncx6ROthtiDonS9La1U5na4DdSrLAPQ2gsEJscJe14HT9TtaGbPrzebUcL6XclDTQJGArYh0=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [Balatro](https://store.steampowered.com/app/2379780/Balatro/)

![](https://lh3.googleusercontent.com/pw/AP1GczNigAiRc_8jZQy-TFzipwYSd3m_fXipngiaNzK7qrd5Ca4aa_2M7BQcG1xUASHCEYFKj7rzJr7kvA5oNZ_Za4X6AZDAjG0yAa_JZSkcYgyKK87aWPlKeAMDhzDTk2gAJ7k2Z07YjE0Dl0pdShxa91o=w884-h496-s-no-gm?authuser=4)
<br> <br>

### Adventure, Indie

#### [No Man’s Sky](https://store.steampowered.com/app/275850/No_Mans_Sky/)

![](https://lh3.googleusercontent.com/pw/AP1GczMv49eLaDhO3jtDoqqyvH6OhS6SMxf3nXHVSt1jsE84y6xlqoi98pvxG3JJQzeiH4jMFLt9-413sOxKTLtRn1B937aFWuRSrHb-MfKDKzpzBZ-pVAxehFhv_5zu5t3DkH0f_v99Y4knNe1G-bWWPg4=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [Stray](https://store.steampowered.com/app/1332010/Stray/)

![](https://lh3.googleusercontent.com/pw/AP1GczPX6HIQZs9JNRcg68nAJVQiuKk-yhaVfLqFJFloR_VpmY_lri_Jlrs3E_LFUnNbv_P2C0Nyyhdh1XGVYidqEZavdj6CPQ_RUR-w5yH1cQJRFdJpQuAG2G2BqsqPSZw32VijJAVl0D35MOdTFu672J8=w884-h497-s-no-gm?authuser=4)
<br> <br>

#### [Firewatch](https://store.steampowered.com/app/383870/Firewatch/)

![](https://lh3.googleusercontent.com/pw/AP1GczOIscEkB-2HOUdqg1E4UgUCtqK8IbSujuy8w30OEaOoJqUGfLnyB_GcBmCdK4WKPwpJD8Q3Y6WHSiKYeJWbu5srgPrvJLX1fW0zz6ywMt-JVE0xOZcglR-WLyl6ho-bkSo-0-rInaKp1QDs1CBC9Sk=w884-h496-s-no-gm?authuser=4)
<br> <br>

### Adventure

#### [Sea of Thieves: 2025 Edition](https://store.steampowered.com/app/1172620/Sea_of_Thieves_2025_Edition/)

![](https://lh3.googleusercontent.com/pw/AP1GczM8T2xPR53TrNILOaP15PfeQ-Twz6H24qsYTRobZ-zwzzPvN8d8do4Fw74p9rFPXUJTxfLRphgmn4Y3dC3gxutFUgnssWxbReyWPkpO8OXuoVGVYjtJB_eQrzxyNaocbHORRYr2Z8caa1yhsmIsa40=w884-h498-s-no-gm?authuser=4)
<br> <br>

#### [Ghost of Tsushima DIRECTOR’S CUT](https://store.steampowered.com/app/2215430/Ghost_of_Tsushima_DIRECTORS_CUT/)

![](https://lh3.googleusercontent.com/pw/AP1GczOBnJeZQdC0YucfiCnkv_tXGKLFLXkpEIcookkqLDARkNgHyeDJkTA4w6NAg25kGCbcCyszgV7nioZa2YkWoHd0pwIp6kpDtPkK0TSFcS5YCKgPnXuBcNjOpgspf0e0OlmTt208f276Vxky9tdi9YQ=w884-h496-s-no-gm?authuser=4)
<br> <br>

#### [God of War Ragnarok](https://store.steampowered.com/app/2322010/God_of_War_Ragnark/)

![](https://lh3.googleusercontent.com/pw/AP1GczOzi80sKqTZtX9SSrXh39J4ItNGXkrU4CVHAxzHm3Uxx1kafZODDn3sQAvVngkzIDiGSRt9sdks32glAaLxFOmpAi84-VUdRhpqUsAoLJZecegEUf8VTzMHQGon2ImUejgpzSSeXYHsFwIHnxfqIe4=w884-h495-s-no-gm?authuser=4)
<br> <br>

### Action, Adventure, RPG, Indie

#### [Bloodstained: Ritual of the Night](https://store.steampowered.com/app/692850/Bloodstained_Ritual_of_the_Night/)

![](https://lh3.googleusercontent.com/pw/AP1GczNBWWEU0coWG17szSC9W4x2EiD9W-yDGjKJF99kqZY7WId9eVCUNS7qrotf7HKU3qyufkuoQdF_TcVml_GP2yfEij6eQiTReZXMTsglvyBHKvtSaRjv0F2o24My_SHRgT9aV6CxGDcXcLpKtjNz3wo=w884-h496-s-no-gm?authuser=4)
<br> <br>

#### [Eastward](https://store.steampowered.com/app/977880/Eastward/)

![](https://lh3.googleusercontent.com/pw/AP1GczN8pMSwqzbn_Q6fxjF3EvHc8vC1FsaJ9HwOAN3Z8jWxizcc-QglG3qZ12F2_J6dBUigWG0LfCw0Mbsn9WA956jXsDVDn4W5vXUrTC5eHpmNADT0yImVSeuBg9AzblJkXW84dXMtpX5kC2CTqXotRi0=w884-h497-s-no-gm?authuser=4)
<br> <br>

#### [Dust: An Elysian Tail](https://store.steampowered.com/app/236090/Dust_An_Elysian_Tail/)

![](https://lh3.googleusercontent.com/pw/AP1GczOt9A8_2kLg87ss91-kStLa_HXyTdK96MQ6oMvP2-Q5uKp13yNVaDdcVRmmYirM7jrqhnYNKNfkPYl7fzDcEF3uF-VxP9TjePsvyGjsbq-PE0o-3oRiB5QcAtJvg79F9IU-g1oniP61SoXUlMPLFsA=w884-h496-s-no-gm?authuser=4)
<br> <br>

### Action, Shooter, Adventure, Indie, RPG, Strategy

#### [Orcs Must Die! 2](https://store.steampowered.com/app/201790/Orcs_Must_Die_2?snr=1_7_15__13)

![](https://lh3.googleusercontent.com/pw/AP1GczN5hlt8HoSJ6n15E8dpSWefbQtwyVr_HRDkd0v0O0XFPQflqe1ZmRxLT6KVKRVygb5wC2TBb9Rj8lwm3zNLLQGQOGiJwn0jPmFbbI7xllwj3-_fFnxKLGRIy-FtXJjXRyMWSCrrVeVVI0N4XlXsVT0=w884-h495-s-no-gm?authuser=4)
<br> <br>

#### [Unconventional Warfare](https://store.steampowered.com/app/831110/Unconventional_Warfare/)

![](https://lh3.googleusercontent.com/pw/AP1GczMYmBuG0xuyoReEC8OxfrgzPxkC7xRv7_Z8jz6SxjxqU3wEn9SZSdqXS2TakqtV-oJjS1pHFgtYsIs6tfGFK0fnZejnF1l5gxDjkoZelAETqrxQGlRicMb4u1xQYCFmdUb6buO5SyCIRVoqVpXcG5I=w884-h497-s-no-gm?authuser=4)
<br> <br>

#### [Steel Rain](https://store.steampowered.com/app/387240/Steel_Rain/)

![](https://lh3.googleusercontent.com/pw/AP1GczMhX13547wpHcKsLrbrCLigobpMJ3LIhpLDRr7g4nJaZG7u1Wp20qGqd0PLe-EA1HPCRJaf45NF_NT-QpHJAVuxojh8XjImEcoV3jqyyCAIimuTY5FvlKE2Ir2xX63fC9ymDmQkVHB-T8hFpF_chwU=w884-h495-s-no-gm?authuser=4)
<br> <br>

## Genre Count Per Title

Having the top 10 genres laid out is a great starting point, but which
genre combination will be best to start with?

I counted the amount of times a genre popped up since there were
multiple instances where genres were repeated and just added to the
genre combination.

![](https://lh3.googleusercontent.com/pw/AP1GczO3Fk3Xj-0ybn-Y4Lxjf2DfFrDv5aafJuRPS-QYUseSJLtrBB0OmMmDtMvICyRLtn7KixEVTqp5f4w56ey5zeaR6Uyuaj8pRN-EYXC4bjvaTEaEKL5_jvapySaYyqruPLdpt1-_Si6mevkLvN1ZXQo=w600-h371-s-no-gm?authuser=4)

[View the Google sheet chart source
here](https://docs.google.com/spreadsheets/d/1VplF384Hr9dPfnUkal7zpPNXyWGiKDHuu3s2adviSLk/edit?gid=0#gid=0&range=A1)

![](https://lh3.googleusercontent.com/pw/AP1GczMZA-qSrmTcJGeCdAYQ4ak9fSUzC78MmkrCxOBxethKEZFlzAi2unNeyipm_Lj6utfypPMXe5twD2dwsRm_1Sv0U0k1xG9TZPfXocN9eJm-iG-bwgg4c4RZKAu9BkrL7tI0UPG3XE-wZMofN2FDOJo=w600-h371-s-no-gm?authuser=4)

[View the Google sheet chart source
here](https://docs.google.com/spreadsheets/d/1VplF384Hr9dPfnUkal7zpPNXyWGiKDHuu3s2adviSLk/edit?gid=231330048#gid=231330048&range=A1)

![](https://lh3.googleusercontent.com/pw/AP1GczO98YACpGiepu3XQPFZQJR_HOjZkyXN6oRMJ_ecAv5fm24L8LzbpJTeLo-4XQGv29fjP9fvu1cAfwFziWH9bVcNva5u06tBiBgVZbnlOcTYE7_w9XvBENglZdUPk7BjMpxubrF3cG3KPDcKUTgHiB0=w600-h371-s-no-gm?authuser=4)

[View the Google sheet chart source
here](https://docs.google.com/spreadsheets/d/1VplF384Hr9dPfnUkal7zpPNXyWGiKDHuu3s2adviSLk/edit?gid=231330048#gid=231330048&range=A1)

We find that Action shows up the most on both organized datasets
(average reviews and review volumes 4+/5). However, while casual pops up
the second most on the average review dataset, it’s non-existent on the
review volume dataset. Indie ends up being the next best genre.

Now that we know which genres pop up the most, let’s count the amount of
genres in each genre-hybrid position in the top 10 of each dataset.

![](https://lh3.googleusercontent.com/pw/AP1GczOAvrRr_dEQz9ImgNKprBD9g7lOrs7P085tsHGG-Gn0XluljbWAEj0KMLpGnHtoaqzT_jXbdRRPnHOidudDMJjoaBd16MmLxKURNFVxKYtim8y_8v2xBDKRTkVE3wbN_3dCsoM4OC0Tnqc02bDw2ZU=w600-h371-s-no-gm?authuser=4)

[View the Google sheet chart source
here](https://docs.google.com/spreadsheets/d/1VplF384Hr9dPfnUkal7zpPNXyWGiKDHuu3s2adviSLk/edit?gid=2001879070#gid=2001879070&range=A1)

![](https://lh3.googleusercontent.com/pw/AP1GczPvO57eUZ8r0MLdnEvIh8epWe2BFsUNbhwcGSXXyrhNgp0ewrM1NMVklNbnpLojqhGzKFXKE4cMZxq3IMeW8JX6S6OLRr4DJ4zgF04YO_E7-vHzYMi1wrGJ3fMXTvunYD1yXgDghm9MKYjnuBYetPY=w600-h371-s-no-gm?authuser=4)

[View the Google sheet chart source
here](https://docs.google.com/spreadsheets/d/1VplF384Hr9dPfnUkal7zpPNXyWGiKDHuu3s2adviSLk/edit?gid=749247536#gid=749247536&range=A1)

![](https://lh3.googleusercontent.com/pw/AP1GczMYI3lPP6fa2bVu5rDERKwy9_2aRgqGGNZJdXcjP-fJfdZ0r8TqrOzVPakhOL70KKxLI5cDq7U_KzKFpGZ01ZjPW7ub02nJ8OBPCZW1qyVlUIoT2ATIF8z6-hMaIOk0B_n1iZr2nNfixMlIVUFRszo=w600-h371-s-no-gm?authuser=4)

[View the Google sheet chart source
here](https://docs.google.com/spreadsheets/d/1VplF384Hr9dPfnUkal7zpPNXyWGiKDHuu3s2adviSLk/edit?gid=749247536#gid=749247536&range=A1)

What is shown is that while the high-rated review volume games have less
tags, the average review games tend to lean towards the higher counts.
Overlapping the two datasets shows an overlap between 2-3 genres per
position. However, since three genre-tags and four genre-tags have the
same amount of occurrences for the high-rating volume reviews, the
occurrences have one of two peaks of 3 occurrences, one of them being at
the four genre-tags. This leads me to suggest between two - four tags
for a genre-hybrid.

## Indie! Set! Action!

Putting the data together, I recommend a multi-pronged approach to
choosing a proper genre:

#### Primary Focus:

- Action
- Indie

#### Secondary Focus:

- Adventure
- Casual
- Simulation

#### Tertiary Focus:

- Strategy
- Arcade <br> <br>

The data shows that three to four genre-tags are suggested. I would say
to focus on Action/Indie and choose at least one from the secondary
focus before moving to the tertiary focus. These are ordered by
popularity (average and high rated volume). Using the examples listed,
there could be a more in-depth analysis on how to tackle the game being
developed; whether they should be story-focused or gameplay focused, but
that’s currently out of the scope of this report.

## Conclusion

J.K. Rowling, Stephen King, George R.R. Martin, and even budding writers
will tell you, “If you want to write books, you need to read books.” The
same can be said for visual artists, movie directors, and even games. In
order to know what constitutes a good game, you should play more games.
What makes a game good? What makes a game bad? Good media is in
everyone’s favor, so everyone is willing to tell you the honest truth
about what they like and dislike about them.

Linking the Steam pages for the games were done in an attempt to help
get that creative pipeline started!

## Sources:

In case of broken images, all images were uploaded to Google Photos and
can be accessed here: <https://photos.app.goo.gl/BCvgEyX8g1sdeeud7> <br>
<br>

Original Source of Datasets:
<https://www.kaggle.com/datasets/bhanuprakashchegondi/games-metadata-and-ratings-5k-dataset>

Tools Used:

- RStudio (Desktop)
- Google Docs (Scripting and Outlining)
- Google Sheets (Charting Data in “Genre Count Per Title” chapter)
- Google Photos (Picture Hosting)
- Steam (Game Title Screenshots)
