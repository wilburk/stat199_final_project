# data

DIMENSIONS
21x9551 -> "29 variables and 22410 observations"

CODEBOOK

Business_id
	Unique id created by Yelp that corresponds to the business , character

Name 
	Name of the business

Neighborhood
	The name of neighborhood where the business is located, na is unknown, character,

Address 
	Address of the business, character

City 
	The city where the business is located, character
State 
	The state where the business is located, character

Postal-code 
	Postal code of the business, character

Latitude and longitude 
	Latitude and longitude of where the business is located, numeric

Stars
	Star rating of the business, numeric

Review-count 
	Number of reviews that a business has, integer

Is-open 
	Whether the business is still open, integer

Categories 
	The categories that the business belong to, factor

BusinessAcceptsCreditCards
	Whether or not the business accepts credit card, logical (true,false)

RestaurantsPriceRange2
	The price range of the restaurants, integer 
	(1 – under 10 dollars, 2 – 10-30 dollars, 3 – 31-60 dollars, 4 – over 61 dollars) 

GoodForKids 
	Whether the business is good for kids, logical 

BikeParking
	Whether the business has bike parking, logical 

Alcohol 
	The type of alcohol services that the business provides, factor
	(full_bar, none, beer_and_wine) 

HasTV
	Whether the business has TV, logical 

NoiseLevel 
	The type of noise level that the business has, factor 
	(quiet, average, loud, very_loud)

RestaurantsAttire
	The type of restaurants attire, factor 
	(casual, dressy, formal)

RestaurantGoodForGroups
	Whether the restaurant is good for groups, logical 

Caters
	Whether the business caters, logical 

WiFi
	The type of wifi services that the business provides, factor 
	(free, no , paid)

RestaurantsReservations 
	Whether the restaurants provides reservation services, logical 

RestaurantsTakeOut
	Whether the restaurants provides takeout services, logical 

aggBusinessParking
	The type of parking options that the business provides, factor
	(none, valet, validated, lot, street, garage)

aggAmbience
	The type of ambience that the business provide, factor 
	(casual, hipster, none, divey, romantic, intimate, trendy, touristy, upscale, classy)


aggGoodForMeal
	The type of meals that the restaurant is good for, factor
	(latenight, dessert, dinner, breakfast, lunch, brunch, none) 
