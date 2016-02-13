# This story has been pulled out from Žižek's book "107 storielle di Žižek" as (shown) in the  cover below. If you want to purchase the book [you can follow this link](http://www.amazon.it/107-storielle-di-Zizek-Slavoj/dp/8868331152 "Zizek"). #

![copertina](copertina2.jpg)

# Three women and five men. #
It's an old story that goes back to the time of French Revolution. Three noble women are keep captive by some revolutionares. Their leader decides to free any woman who will solve a riddle. Being pushed over a table the three women would be fucked by three of the five man, two of them being blacks and the others white. Every woman would be able to see only the two companions with their men in front of herself but she wouldn't be able to see her partner (if we want to call him so, or just a fucker). Would she be able to rightly guess the color of the man standing behind her?

# The code. #
Firs of all we define a type for a male.

				type Male = 
					    | White
					    | Black

Then we declare the male company:

				let Men = [White; White; White; Black; Black]

So we declare first a recursive function called choices:

			let rec choices = function
			    | []      -> []
			    | p::tail -> (p,tail) :: [ for (y,l) in choices tail -> (y,l) ]

if we start from the first man of the list to the last we should have a similar output list:


			//  [(White, [White; White; Black; Black]); 
			//  (White, [White; Black; Black]);
			//  (White, [Black; Black]); 
			//  (Black, [Black]); 
			//  (Black, [])]

then we use it to find all possible combinations of 5 men on 3 women:

			let rec combinations S k =
			    [ if k=0 then yield [] else
			            for (e,r) in choices S do
			                for o in combinations r (k-1) do yield e::o  ]

If we print out the outcome we have:

			    //[[White; White; White]; [White; White; Black]; [White; White; Black];
			    //[White; White; Black]; [White; White; Black]; [White; Black; Black];
			    //[White; White; Black]; [White; White; Black]; [White; Black; Black];
			    //[White; Black; Black]]

Instead of introducing the concept of woman we're going to deal with the positions around the table:

				type Position =
				    | A
				    | B
				    | C
				
				let Positions = [A;B;C]

'cause we aren't interested in the modelling about a particular male or the other but only about the color we're going to make a one distinct list:


		let Couples = (combinations Men Positions.Length) |> List.distinct

so the output is this:

		//  [[White; White; White]; [White; White; Black]; [White; Black; Black]]

every male in the three different list is bound to a particular position, let's say A, B or C so we need to inquire into this particular view with a function that lists everyone except the observed:

		let removeAt index lista =
		    lista 
		    // Associate each element with a boolean flag specifying whether 
		    // we want to keep the element in the resulting list
		    |> List.mapi (fun i el -> (i <> index, el)) //index from (0)
		    // Remove elements for which the flag is 'false' and drop the flags
		    |> List.filter fst |> List.map snd

and all the matches that we have:

		let mutable matches = []
		for dance in Couples do
		    let counter = ref 0
		    for m in dance do
		        matches <- (removeAt !counter dance , Positions.[!counter])::matches 
		        counter := !counter + 1


hence we print something like that:
				
			//([White; Black], C)
			//([White; Black], B)
			//([Black; Black], A)

			//([White; White], C)
			//([White; Black], B)
			//([White; Black], A)

			//([White; White], C)
			//([White; White], B)
			//([White; White], A)

and if we introduce a function of truth, let's say if A sees 2 blacks then it's certain that A's partern is white.

				
		let free w = match w with 
		             | [White; White] -> false 
		             | [White; Black] -> false 
		             | [Black; Black] -> true
		             | _-> false
				
then we can print our table:


		false
		false
		true
		false
		false
		false
		false
		false
		false


As you can see things are pretty much fucked up, tho it would be possible to create a probability function to measure all the odds.








