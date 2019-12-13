#' Pokedex: Who's that Pokemon?!
#'
#' Ever walk through a random street and get caught off guard by a wild Pokemon? You exclaim, "Oh! a wild <insert Pokemon name> appeared!" But the problem is...you don't know its name!
#' Fear not, young Pokemon master-in-training! This function allows the user to narrow their search on what type of Pokemon they are facing and provides basic information on each Pokemon.
#' That's right, it's your very own Pokedex in function form!
#'
#' The function asks the user for the color (e.g. yellow, brown, red) and type (e.g. water, fire, grass) of the Pokemon.
#' The output shows a dataframe of various Pokemon and basic stats related to each Pokemon. This function can query information from all 807 Pokemon.
#'
#' @param color Color of the Pokemon.
#' @param type Type of the Pokemon (e.g. water, electric, fire, grass, fighting, psychic, flying, ghost, steel).
#' @keywords pokedex, pokemon, type, color
#' @examples
#' pokedexr("red", "fire")
#' # give the color and the type of the Pokemon
#' @return data frame
#' @author Montsie Guerrero <\url{http://github.com/acg2176}>
#' @export
#'
#' @import dplyr
#' @import httr
#' @import tidyr
#' @import knitr


pokedexr <- function(color, type){
  poke_info <- function(pokemon){
    base <- "https://pokeapi.co/api/v2/"
    endpoint <- "pokemon-species/"
    param <- pokemon #takes in id or name
    poke <- paste(base, endpoint, param, sep="")
    poke_response <- GET(poke)


    endpoint2 <- "pokemon/"
    poke2 <- paste(base, endpoint2, param, sep="")
    poke_response2 <- GET(poke2)


    if (http_error(poke_response)){
      warning("The request produced an error.")
    } else {poke_content <- content(poke_response)}

    if (http_error(poke_response2)){
      warning("The request produced an error.")
    } else {poke_content2 <- content(poke_response2)}


    Name <- c(poke_content[[19]][1])
    Number <- c(poke_content[[17]]) #id number of pokemon
    Color <- c(poke_content[[3]][1]$name) #color of pokemon
    Type_1 <- c(poke_content2$types[[1]]$type$name) #type 1 of pokemon

    if (length(poke_content2$types) == 2){   #type 2 of pokemon
      Type_2 <- c(poke_content2$types[[2]]$type$name)
    } else {
      Type_2 <- c("NA")
    }
    Shape <- c(poke_content$shape$name) #shape of pokemon
    Weight_kg <- c(poke_content2[[17]]/10) #weight of pokemon
    Height_m <- c(poke_content2[[5]]/10) #height of pokemon
    HP <- c(poke_content2[[15]][[6]]$base_stat) #HP of pokemon
    Base_happiness <- c(poke_content$base_happiness) #base happiness is 255
    Capture_rate <- c(poke_content$capture_rate) #capture rate max is 255
    Speed_stat <- c(poke_content2[[15]][[1]]$base_stat)
    Special_Defense_stat <- c(poke_content2[[15]][[2]]$base_stat)
    Special_Attack_stat <- c(poke_content2[[15]][[3]]$base_stat)
    Defense_stat <- c(poke_content2[[15]][[4]]$base_stat)
    Attack_stat <- c(poke_content2[[15]][[5]]$base_stat)

    poke_data <- data.frame(Name, Number, Color, Type_1, Type_2, Shape, Weight_kg, Height_m, HP, Base_happiness, Capture_rate, Speed_stat, Special_Defense_stat, Special_Attack_stat, Defense_stat, Attack_stat)
    poke_data
  }
  pokewho <- data.frame() #list of pokemon to enter the for loop.
  #for loop has the poke_info function
  for (i in 1:200){
    pokewho <- rbind.data.frame(pokewho, poke_info(i))
  }
  if (color %in% pokewho$Color & type %in% pokewho$Type_1 | color %in% pokewho$Color & type %in% pokewho$Type_2){
    pokewho%>%filter(Color == color & Type_1 == type | Color == color & Type_2 == type)
  } else {warning("No pokemon matches your search.")}
}

#' Strength or Weakness? Figure out your Pokemon's next Move!
#'
#' Oh no! Your in the middle of your first Pokemon battle and you didn't just forget to call mom about it,
#' you forgot which moves your Pokemon can do! Which type of move is strong against X and weak against Y? To figure out the best strategy,
#' this function allows the Pokemon trainer to find out a dataset of moves they can use to make the most "It's super effective!" move.
#'
#' This function takes in 2 arguments of types and determines whether the relationship of these types is either
#' a strength or a weakness (e.g. water is a strength while fire is a weakness). If there is no relationship, the function will let the user/trainer know. There are 18 Pokemon types to choose from: normal, fighting, flying, poison, ground, rock, bug, steel,
#' fire, water, grass, electric, psychic, ice, dragon, dark, fairy, shadow. The output is a dataset of 15 Pokemon moves (based on type) that can be used by the
#' Pokemon player for his/her next optimal move. Go forth and get that Gym Badge, young Pokemaster!
#'
#' @param type1 Type of Pokemon. (e.g. fire, steel, ice, psychic, ghost, electric).
#' @param type2 Type of Pokemon. (e.g. fire, steel, ice, psychic, ghost, electric).
#' @return data frame
#' @author Montsie Guerrero <\url{http://github.com/acg2176}>
#' @export
#' @examples
#' pokemaster("grass","ground")
#' # add two types of Pokemon moves
#' @import dplyr
#' @import httr
#' @import tidyr
#' @import knitr


pokemaster <- function(type1, type2){
  poke_types <- function(typeid, i){
    base<- "https://pokeapi.co/api/v2/"
    endpoint3 <- "type/"
    param2 <- typeid
    poke3 <- paste(base, endpoint3, param2, sep="")
    poke_response3 <- GET(poke3)


    if (http_error(poke_response3)){
      warning("The request produced an error.")
    } else {poke_content3 <- content(poke_response3)}



    Move <- c(poke_content3[[6]][[i]]$name[1]) # move of a pokemon (need for loop?)
    Type <- c(poke_content3[[7]]) #type

    if (length(poke_content3$move_damage_class$name) == 0){
      Damage_Class <- c("null")
    } else{
      Damage_Class <- c(poke_content3$move_damage_class$name) #damage class of move
    }

    if (length(poke_content3[[1]]$double_damage_from) == 0) {
      Weakness_1 <- c("null")
    } else {
      Weakness_1 <- c(poke_content3[[1]]$double_damage_from[[1]]$name)
    }

    if (length(poke_content3[[1]]$double_damage_from) >= 2) {
      Weakness_2 <-c(poke_content3[[1]]$double_damage_from[[2]]$name)
    } else if (length(poke_content3[[1]]$double_damage_from) == 0){
      Weakness_2 <- c("null")
    } else {
      Weakness_2 <- c("null")
    }


    if (length(poke_content3[[1]]$double_damage_to)==0){
      Strength_1 <- c("null")
    }else{
      Strength_1 <- c(poke_content3[[1]]$double_damage_to[[1]]$name) #strength 1
    }

    if(length(poke_content3[[1]]$double_damage_to)>=2){
      Strength_2 <- c(poke_content3[[1]]$double_damage_to[[2]]$name) #strength 2
    } else if (length(poke_content3[[1]]$double_damage_to)==0) {
      Strength_2 <- c("null")
    } else {
      Strength_2 <-c("null")
    }

    #use the MOVES endpoint
    poke_content4 <- content(GET(poke_content3[[6]][[i]]$url[1]))

    if (length(poke_content4[[1]])==0){
      Accuracy <- c("null")
    } else{
      Accuracy <- c(poke_content4[[1]]) #accuracy of move
    }
    if (length(poke_content4[[18]])==0){
      PP <- c("null")
    } else {
      PP <- c(poke_content4[[18]]) #power points of move; how many times this move can be used
    }

    if (length(poke_content4[[17]])==0){
      Power <- c(0)
    } else {
      Power <- c(poke_content4[[17]]) #power of move
    }
    poke_attack <- data.frame(Move, Type, Damage_Class, Strength_1, Strength_2, Weakness_1, Weakness_2, Accuracy, PP, Power)
    poke_attack
  }
  #for loop!
  pokemove <- data.frame()
  types <- c("normal","fighting", "flying","poison","ground","rock","bug","steel",
             "fire","water","grass","electric","psychic","ice","dragon","dark","fairy","shadow")
  for (typeid in types){
    for (i in 1:15){
      pokemove <- rbind(pokemove, poke_types(typeid, i), stringsAsFactors = FALSE)
    }
  }
  if (type1 %in% pokemove$Strength_1 & type2 %in% pokemove$Weakness_1) {
    pokemove%>%filter(Strength_1 == type1 & Weakness_1 == type2)  %>% arrange(desc(Power))
  } else if (type1 %in% pokemove$Strength_1 & type2 %in% pokemove$Weakness_2){
    pokemove%>%filter(Strength_1 == type1 & Weakness_2 == type2)  %>% arrange(desc(Power))
  } else if (type1 %in% pokemove$Strength_2 & type2 %in% pokemove$Weakness_1){
    pokemove%>%filter(Strength_2 == type1 & Weakness_1 == type2)  %>% arrange(desc(Power))
  } else if (type1 %in% pokemove$Strength_2 & type2 %in% pokemove$Weakness_2){
    pokemove%>%filter(Strength_2 == type1 & Weakness_2 == type2)  %>% arrange(desc(Power))
  } else if (type1 %in% pokemove$Weakness_2 & type2 %in% pokemove$Strength_2){
    pokemove%>%filter(Weakness_2 == type1 & Strength_2 == type2)  %>% arrange(desc(Power))
  } else if (type1 %in% pokemove$Weakness_2 & type2 %in% pokemove$Strength_1){
    pokemove%>%filter(Weakness_2 == type1 & Strength_1 == type2)  %>% arrange(desc(Power))
  } else if (type1 %in% pokemove$Weakness_1 & type2 %in% pokemove$Strength_2){
    pokemove%>%filter(Weakness_1 == type1 & Strength_2 == type2)  %>% arrange(desc(Power))
  } else if (type1 %in% pokemove$Weakness_1 & type2 %in% pokemove$Strength_1){
    pokemove%>%filter(Weakness_1 == type1 & Strength_1 == type2)  %>% arrange(desc(Power))
  } else {warning("No moves match your search.")}
}

#' Know your Pokemon Stats: Where're my Pokemon party monsters at?!
#'
#' In the Poke-universe, every Pokemon trainer can only hold a total of 6 Pokemon while on the go, also known as a Pokemon party. This function allows the Pokemon trainer to
#' get to know his/her party Pokemon (i.e. basic stats). This is advantageous for Pokemon trainers to understand each of their Pokemon's strengths and weaknesses in terms of their HP (hit points), attack and defense profiles.
#'
#' This function asks the Pokemon player for the names of 6 of the Pokemon in his/her party. The output is a visualized barplot of the Pokemons' basic stats.
#'
#' @param pokemon1 Name of Pokemon.
#' @param pokemon2 Name of Pokemon.
#' @param pokemon3 Name of Pokemon.
#' @param pokemon4 Name of Pokemon.
#' @param pokemon5 Name of Pokemon.
#' @param pokemon6 Name of Pokemon.
#' @examples
#' catchemall("bulbasaur", "togepi", "pikachu", "squirtle", "cyndaquil", "psyduck")
#' # add 6 names of Pokemon
#' @return data frame
#' @author Montsie Guerrero <\url{http://github.com/acg2176}>
#' @export
#' @import ggplot2
#' @import dplyr
#' @import httr
#' @import tidyr
#' @import knitr

catchemall <- function(pokemon1, pokemon2, pokemon3, pokemon4, pokemon5, pokemon6) {
  poke_party <- function(pokemon){
    base <- "https://pokeapi.co/api/v2/"
    endpoint <- "pokemon-species/"
    param <- pokemon
    poke <- paste(base, endpoint, param, sep="")
    poke_response <- GET(poke)


    endpoint2 <- "pokemon/"
    poke2 <- paste(base, endpoint2, param, sep="")
    poke_response2 <- GET(poke2)


    if (http_error(poke_response)){
      warning("The request produced an error.")
    } else {poke_content <- content(poke_response)}

    if (http_error(poke_response2)){
      warning("The request produced an error.")
    } else {poke_content2 <- content(poke_response2)}


    Pokemon <- c(poke_content[[19]][1]) #name of pokemon
    HP <- c(poke_content2[[15]][[6]]$base_stat) #HP of pokemon
    Speed <- c(poke_content2[[15]][[1]]$base_stat) #speed stat
    Special_Defense <- c(poke_content2[[15]][[2]]$base_stat) #special defense
    Special_Attack <- c(poke_content2[[15]][[3]]$base_stat) #special attack
    Defense <- c(poke_content2[[15]][[4]]$base_stat) #defense
    Attack <- c(poke_content2[[15]][[5]]$base_stat) #attack


    poke_stats <- data.frame(Pokemon, HP, Speed, Special_Defense, Special_Attack, Defense, Attack)
    poke_stats2 <- poke_stats %>% gather("Stats", "Value", 2:7)
    poke_stats2
  }
  pokewhat <- data.frame() #list of pokemon to enter the for loop.
  #for loop has the poke_info function
  for (i in 1:807){
    pokewhat <- rbind.data.frame(pokewhat, poke_party(i))
  }

  pokegraph <- pokewhat%>% filter(Pokemon == pokemon1 | Pokemon == pokemon2 | Pokemon == pokemon3 | Pokemon == pokemon4 | Pokemon == pokemon5 | Pokemon == pokemon6)

  ggplot(data=pokegraph, aes(x=Pokemon, y=Value, fill = Stats)) + geom_bar(stat="identity", position="dodge") + geom_text(aes(label=Value),position=position_dodge(width=0.9), hjust=-0.3, color = "black", size=2.5)+theme_minimal()+coord_flip()
}

#' Ready, Set, Evolve! Pokemon, All Grown Up"
#'
#' Like a proud parent, all Pokemon trainers beam with pride when their Pokemon make it to the next evolution. Happy tears aside, this function allows the Pokemon trainer
#' to determine how their Pokemon evolve (i.e. by level, by special item, etc). The output of this function is a dataset that provides information on the Pokemon evolution chain.
#'
#' The function takes in 3 arguments. Pokemon trainers can select 3 Pokemon to find out each Pokemon's evolution chain. If Pokemon that is inputted does not have an evolution chain, a warning message will occur.
#'
#' @param pokemon1 Name of Pokemon.
#' @param pokemon2 Name of Pokemon.
#' @param pokemon3 Name of Pokemon.
#'
#'@return data frame
#' @author Montsie Guerrero <\url{http://github.com/acg2176}>
#' @export
#' @examples
#' pokevolve("pikachu","weedle","blastoise")
#' # add 3 Pokemon names
#' @import dplyr
#' @import httr
#' @import tidyr
#' @import knitr


pokevolve <- function(pokemon1, pokemon2, pokemon3){
  pokechange <- function(i){
    base <- "https://pokeapi.co/api/v2/"
    endpoint5 <- 'evolution-chain/'
    param5 <- i
    poke5 <- paste(base, endpoint5, param5, sep="")
    poke_response5 <- GET(poke5)


    if (http_error(poke_response5)){
      warning("The request produced an error.")
    } else {poke_content5 <- content(poke_response5)}


    if (length(poke_content5$chain$evolves_to)==0){
      warning("This Pokemon does not have an evolution chain.")
    } else if (length(poke_content5$chain$evolves_to[[1]]$evolves_to)==1){
      Pokemon_1 <- poke_content5[[2]]$species$name #name of first pokemon
      if (length(poke_content5$chain$evolves_to[[1]]$evolution_details[[1]]$trigger$name)==1){
        Evolution_Trigger_1 <- c(poke_content5$chain$evolves_to[[1]]$evolution_details[[1]]$trigger$name) #min level that 1st pokemon will evolve
      } else {
        Evolution_Trigger_1 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_level)==1){
        Level_of_Evolution_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_level) #min level that 1st pokemon will evolve
      } else {
        Level_of_Evolution_1 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$species$name)==1){
        Pokemon_2 <- c(poke_content5[[2]]$evolves_to[[1]]$species$name) #min level that 1st pokemon will evolve
      } else {
        Pokemon_2 <- c("N/A")
      }
      if (length(poke_content5$chain$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$trigger$name)==1){
        Evolution_Trigger_2 <- c(poke_content5$chain$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$trigger$name) #held item to evolve to level 2
      } else {
        Evolution_Trigger_2 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$min_level)==1){
        Level_of_Evolution_2 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$min_level) #2nd level that 1st pokemon will evolve
      } else {
        Level_of_Evolution_2 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$item$name)==1){
        Stone_to_Evolution_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$item$name) #held item to evolve to level 2
      } else {
        Stone_to_Evolution_1 <- c("N/A")
      }

      if (length(poke_content5$chain$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$item$name)==1){
        Stone_to_Evolution_2<- c(poke_content5$chain$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$item$name) #held item to evolve
      } else {
        Stone_to_Evolution_2 <- c("N/A")
      }

      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$species$name)==1){
        Pokemon_3 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$species$name) #min level that 1st pokemon will evolve
      } else {
        Pokemon_3 <- c("N/A") #OPTIONAL
      }


      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$known_move)==1){
        Known_Move_2 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$known_move) #known move to evolve
      } else {
        Known_Move_2 <- c("N/A") #to level 3
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$min_affection)==1){
        Min_Affection_2 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$min_affection) #known move to evolve
      } else {
        Min_Affection_2 <- c("N/A") #to level 3
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$min_beauty)==1){
        Min_Beauty_2 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$min_beauty) #known move to evolve
      } else {
        Min_Beauty_2 <- c("N/A") #to level 3
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$min_happiness)==1){
        Min_Happiness_2 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$min_happiness)
      } else {
        Min_Happiness_2 <- c("N/A") #to level 3
      }

      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$known_move)==1){
        Known_Move_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$known_move) #known move to evolve
      } else {
        Known_Move_1 <- c("N/A") #to level 2
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_affection)==1){
        Min_Affection_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_affection) #known move to evolve
      } else {
        Min_Affection_1 <- c("N/A") #to level 2
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_beauty)==1){
        Min_Beauty_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_beauty) #known move to evolve
      } else {
        Min_Beauty_1 <- c("N/A") #to level 2
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_happiness)==1){
        Min_Happiness_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_happiness)
      } else {
        Min_Happiness_1 <- c("N/A") #to level 2
      }


      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$gender)==1){
        Gender_2 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$gender)
      } else {
        Gender_2 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$gender)==1){
        Gender_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$gender)
      } else {
        Gender_1 <- c("N/A")
      }

      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$needs_overworld_rain)==1){
        Needs_Rain_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$needs_overworld_rain)
      } else {
        Needs_Rain_1 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$party_type)==1){
        Party_type_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$party_type)
      } else {
        Party_type_1 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$needs_overworld_rain)==1){
        Needs_Rain_2 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$needs_overworld_rain)
      } else {
        Needs_Rain_2 <- c("No")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$party_type)==1){
        Party_type_2 <- c(poke_content5[[2]]$evolves_to[[1]]$evolves_to[[1]]$evolution_details[[1]]$party_type)
      } else {
        Party_type_2 <- c("N/A")
      }

      poke_fam <- data.frame(Pokemon_1, Evolution_Trigger_1, Level_of_Evolution_1, Stone_to_Evolution_1, Known_Move_1, Min_Affection_1, Min_Beauty_1, Min_Happiness_1, Gender_1, Needs_Rain_1,  Party_type_1, Pokemon_2, Evolution_Trigger_2, Level_of_Evolution_2,  Stone_to_Evolution_2, Known_Move_2, Min_Affection_2, Min_Beauty_2, Min_Happiness_2, Gender_2, Needs_Rain_2,  Party_type_2, Pokemon_3)
      poke_fam


    } else {
      Pokemon_1 <- poke_content5[[2]]$species$name #name of first pokemon

      if (length(poke_content5$chain$evolves_to[[1]]$evolution_details[[1]]$trigger$name)==1){
        Evolution_Trigger_1 <- c(poke_content5$chain$evolves_to[[1]]$evolution_details[[1]]$trigger$name) #min level that 1st pokemon will evolve
      } else {
        Evolution_Trigger_1 <- c("N/A")
      }


      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_level)==1){
        Level_of_Evolution_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_level) #min level that 1st pokemon will evolve
      } else {
        Level_of_Evolution_1 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$item$name)==1){
        Stone_to_Evolution_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$item$name) #held item to evolve to level 2
      } else {
        Stone_to_Evolution_1 <- c("N/A")
      }

      if (length(poke_content5[[2]]$evolves_to[[1]]$species$name)==1){
        Pokemon_2 <- c(poke_content5[[2]]$evolves_to[[1]]$species$name) #min level that 1st pokemon will evolve
      } else {
        Pokemon_2 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$known_move)==1){
        Known_Move_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$known_move) #known move to evolve
      } else {
        Known_Move_1 <- c("N/A") #to level 2
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_affection)==1){
        Min_Affection_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_affection) #known move to evolve
      } else {
        Min_Affection_1 <- c("N/A") #to level 2
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_beauty)==1){
        Min_Beauty_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_beauty) #known move to evolve
      } else {
        Min_Beauty_1 <- c("N/A") #to level 2
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_happiness)==1){
        Min_Happiness_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$min_happiness)
      } else {
        Min_Happiness_1 <- c("N/A") #to level 2
      }

      Evolution_Trigger_2 <- c("N/A")
      Level_of_Evolution_2 <- c("N/A")
      Stone_to_Evolution_2 <- c("N/A")
      Known_Move_2 <- c("N/A")
      Min_Affection_2 <- c("N/A")
      Min_Beauty_2 <- c("N/A")
      Min_Happiness_2 <- c("N/A")
      Pokemon_3 <- c("N/A")
      Gender_2 <- c("N/A")
      Needs_Rain_2 <- c("N/A")
      Party_type_2 <- c("N/A")
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$gender)==1){
        Gender_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$gender)
      } else {
        Gender_1 <- c("N/A")
      }

      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$needs_overworld_rain)==1){
        Needs_Rain_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$needs_overworld_rain)
      } else {
        Needs_Rain_1 <- c("N/A")
      }
      if (length(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$party_type)==1){
        Party_type_1 <- c(poke_content5[[2]]$evolves_to[[1]]$evolution_details[[1]]$party_type)
      } else {
        Party_type_1 <- c("N/A")
      }

      poke_fam <- data.frame(Pokemon_1, Evolution_Trigger_1, Level_of_Evolution_1, Stone_to_Evolution_1, Known_Move_1, Min_Affection_1, Min_Beauty_1, Min_Happiness_1, Gender_1, Needs_Rain_1,  Party_type_1, Pokemon_2, Evolution_Trigger_2, Level_of_Evolution_2,  Stone_to_Evolution_2, Known_Move_2, Min_Affection_2, Min_Beauty_2, Min_Happiness_2, Gender_2, Needs_Rain_2,  Party_type_2, Pokemon_3)
      poke_fam

    }
  }

  poke_fambam <- data.frame(stringsAsFactors = FALSE)
  pokemon_evol <- c(1:34, 36:52, 54:61, 64, 67:70, 72, 76, 79:98,100, 102, 104,105, 109:112, 114,116, 118,119,126, 130:148, 151:153, 158:162, 164, 166:168,173:178, 181,182, 184, 186:188,191:192, 203:209, 211:214,216:221, 224, 230, 232:233, 239)

  for (i in pokemon_evol){
    poke_fambam <- rbind.data.frame(poke_fambam, pokechange(i), stringsAsFactors = FALSE)
  }

  if (pokemon1 %in% poke_fambam$Pokemon_1 | pokemon1 %in% poke_fambam$Pokemon_2 | pokemon1 %in% poke_fambam$Pokemon_3 | pokemon2 %in% poke_fambam$Pokemon_1 | pokemon2 %in% poke_fambam$Pokemon_2 | pokemon2 %in% poke_fambam$Pokemon_3|pokemon3 %in% poke_fambam$Pokemon_1 | pokemon3 %in% poke_fambam$Pokemon_2 | pokemon3 %in% poke_fambam$Pokemon_3){
    poke_fambam%>%filter(Pokemon_1 == pokemon1 |Pokemon_2 == pokemon1 | Pokemon_3 == pokemon1 | Pokemon_1 == pokemon2 |Pokemon_2 == pokemon2 | Pokemon_3 == pokemon2 |Pokemon_1 == pokemon3 |Pokemon_2 == pokemon3 | Pokemon_3 == pokemon3)
  }



}
