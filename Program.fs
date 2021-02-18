//
// F# program to analyze Divvy daily ride data.
//
// << SUBHRA KANUNGO, skanun2, 672649980>>
// U. of Illinois, Chicago
// CS 341, Fall 2019
// Project #04
//

#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [15,22,141,17,5,1124]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), starting day of week (0 Sunday-6 Saturday)
// and trip duration (secs), 
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

//getting the nth element from the list
let rec getNthIndex list n = 
  match list with
  | [] -> 0
  | hd::_ when (n=0) -> hd
  | _::rest -> getNthIndex rest (n-1)



//all the bikeIDs with the duplicates
let rec bikeIDwithDuplicates bikeList = 
  match bikeList with
  | [] -> []
  | hd::rest -> (getNthIndex hd 2) :: bikeIDwithDuplicates rest
//all the time for specific bikeID given
let rec timeForBikeIDgiven L bikeID=
  match L with 
  | [] -> []
  | hd::rest when((getNthIndex hd 2)=bikeID) -> (getNthIndex hd 5):: timeForBikeIDgiven rest bikeID
  | _::rest -> timeForBikeIDgiven rest bikeID



//station list
let rec stationIDlist L = 
  match L with
  | [] -> []
  | hd::rest -> (getNthIndex hd 1) :: stationIDlist rest

//getting time of stationID list
let rec stationIDlistTime L stationID = 
  match L with
  | [] -> []
  | hd::rest when((getNthIndex hd 1)=stationID) -> (getNthIndex hd 5):: stationIDlistTime rest stationID
  | _::rest -> stationIDlistTime rest stationID


//number of trips per day
let rec NumtripDays L x = 
  match L with
  | [] -> []
  | hd::rest when ((getNthIndex hd 4) = x) -> (getNthIndex hd 4) :: NumtripDays rest x
  | _::rest -> NumtripDays rest x

//top10 functions
let rec removeDupesStationID L x = 
  match L with
  | [] -> []
  | e::rest when e<>x -> e::removeDupesStationID rest e
  | _::rest -> removeDupesStationID rest x
  
let rec stationIDlist1 L = 
  match L with
  | [] -> []
  | hd::rest -> (getNthIndex hd 1) :: stationIDlist rest

let rec numCountride1 x L = 
  match L with 
  | [] -> 0
  | e::rest when e=x -> 1 + (numCountride1 x rest)
  | _::rest -> 0 + (numCountride1 x rest)
  
let rec numRidesStation L X  =
  match L with
  | [] -> []
  | e::rest -> (numCountride1 e X) :: numRidesStation rest X
  


let rec combine L1 L2 x =
  match L1, L2 with 
  | [], [] -> x
  | e1::rest1, e2::rest2 -> combine rest1 rest2 x @ [[e1; e2]] 
  | _, _ -> []

let rec printCrowded L x =
  match L with
  | [] -> ()
  | e::rest when x=0 -> printfn "# of rides to station %A: %A" (getNthIndex e 0) (getNthIndex e 1)
  | _::rest -> printCrowded rest (x-1)




let rec sumTime L = 
  match L with 
  | [] -> 0
  | hd::rest -> hd + sumTime rest
    
let rec removedupes L x = 
  match L with
  | [] -> 0
  | e::rest when e<>x -> 1 + (removedupes rest e)
  | _::rest -> 0 + (removedupes rest x)
  

  
let rec numCountride x L = 
  match L with 
  | [] -> 0
  | e::rest when e=x -> 1 + (numCountride x rest)
  | _::rest -> 0 + (numCountride x rest)

let rec printstars n = 
  match n with
  | 0 ->()
  | 1 ->printf "*"
  | _ ->printf "*"
        printstars (n-1)
        
[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of rides: %A" N
  printfn ""
  
  //let concatList = List.concat ridedata
  let bikeWithDuplicates = bikeIDwithDuplicates ridedata
  let bikeWithDuplicatesSorted = List.sort bikeWithDuplicates
  let numBikesUnique = removedupes bikeWithDuplicatesSorted 0 
  
  printfn "# of bikes: %A" numBikesUnique
  printfn ""
  
  printf "BikeID> "
  let BikeID = System.Console.ReadLine()
  printfn ""
  let bikeid = int BikeID
  let count = numCountride bikeid bikeWithDuplicatesSorted
  printfn "# of rides for BikeID %A: %A" bikeid count
  printfn ""
  
  let listfortimespent =timeForBikeIDgiven ridedata bikeid
  //printfn "%A" listfortimespent
  
  let totalTime = sumTime listfortimespent
  let min = totalTime / 60
  let sec = totalTime % 60
  
  printfn "Total time spent riding BikeID %A: %A minutes %A seconds" bikeid min sec
  printfn ""
  
  let avgTime = float(totalTime)/float(List.length listfortimespent)
  
  printfn "Average time spent riding BikeID %A: %.2f seconds" bikeid avgTime
  printfn ""
  
  printf "StationID> "
  let input = System.Console.ReadLine()
  printfn ""
  let stationID = int input
  let stationIDlist = stationIDlist ridedata
  let count = numCountride stationID stationIDlist
  printfn "# of rides to StationID %A: %A" stationID count
  printfn ""
  
  let listOfTimeStation = stationIDlistTime ridedata stationID
  let totalTimeStation = sumTime listOfTimeStation
  let avgTimeStation = float(totalTimeStation)/float(List.length listOfTimeStation)
  
  printfn "Average time spent on trips leading to StationID %A: %.2f seconds" stationID avgTimeStation
  printfn ""
  
  let numTripsSun = NumtripDays ridedata 0
  let numTripsMon = NumtripDays ridedata 1
  let numTripsTue = NumtripDays ridedata 2
  let numTripsWed = NumtripDays ridedata 3
  let numTripsThur = NumtripDays ridedata 4
  let numTripsFri = NumtripDays ridedata 5
  let numTripsSat = NumtripDays ridedata 6
  
  printfn "Number of Trips on Sunday: %A" (List.length numTripsSun)
  printfn "Number of Trips on Monday: %A" (List.length numTripsMon)
  printfn "Number of Trips on Tuesday: %A" (List.length numTripsTue)
  printfn "Number of Trips on Wednesday: %A" (List.length numTripsWed)
  printfn "Number of Trips on Thursday: %A" (List.length numTripsThur)
  printfn "Number of Trips on Friday: %A" (List.length numTripsFri)
  printfn "Number of Trips on Saturday: %A" (List.length numTripsSat)
  printfn ""
  
  let numSundayStars = (List.length numTripsSun) / 10
  let numMondayStars = (List.length numTripsMon) / 10
  let numTuesdayStars = (List.length numTripsTue) / 10
  let numWednesdayStars = (List.length numTripsWed) / 10
  let numThursdayStars = (List.length numTripsThur) / 10
  let numFridayStars = (List.length numTripsFri) / 10
  let numSaturdayStars = (List.length numTripsSat) / 10
  
  
  printf "0: " 
  printstars numSundayStars 
  printfn " %A"(List.length numTripsSun)
  printf "1: " 
  printstars numMondayStars 
  printfn " %A"(List.length numTripsMon)
  printf "2: " 
  printstars numTuesdayStars 
  printfn " %A"(List.length numTripsTue)
  printf "3: " 
  printstars numWednesdayStars 
  printfn " %A"(List.length numTripsWed)
  printf "4: " 
  printstars numThursdayStars 
  printfn " %A"(List.length numTripsThur)
  printf "5: " 
  printstars numFridayStars 
  printfn " %A"(List.length numTripsFri)
  printf "6: " 
  printstars numSaturdayStars 
  printfn " %A"(List.length numTripsSat)
  
  let stationIDWithDuplicates = stationIDlist1 ridedata
  let stationIDWithDuplicatesSorted = List.sort stationIDWithDuplicates
  //printfn "%A" stationIDWithDuplicatesSorted

  let uniqueStation = removeDupesStationID stationIDWithDuplicatesSorted 5000
  
  let stationIDnum = numRidesStation uniqueStation stationIDWithDuplicatesSorted
  
  let res =combine uniqueStation stationIDnum [] //List.map2 (fun x y -> (x,y)) uniqueStation stationIDnum
  //let res = (uniqueStation::stationIDnum::[])
  //printfn "%A" res
  
  let sortres = List.sortBy(fun [x;y] -> x) res
  let sortedres = List.sortBy(fun [x;y] -> -y) sortres
  
  //printfn "%A" sortedres
  //
  printfn ""
  printCrowded sortedres 0
  printCrowded sortedres 1
  printCrowded sortedres 2
  printCrowded sortedres 3
  printCrowded sortedres 4
  printCrowded sortedres 5
  printCrowded sortedres 6
  printCrowded sortedres 7
  printCrowded sortedres 8
  printCrowded sortedres 9
  printfn ""
  
  
  
  
  
  0 
