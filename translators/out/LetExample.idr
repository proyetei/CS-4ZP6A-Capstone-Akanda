module LetExample

n : Int
n = let x1 = 1 in 
	let x2 = x1 in 
	let x3 = x2 in 
	x3
